;; Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

(define-module (activitystuff base)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 hash-table)
  #:use-module (web uri)
  #:use-module (json)
  #:export (<as-type>
            as-type? make-as-type as-type-uri as-type-parents as-type-props
            define-astype

            <as-obj>
            as-obj? make-as-obj as-type as-fields
            as-implied-contexts as-contexts

            make-as
            make-as-obj-factory

            as-obj-type-string
            as->hash  ; usually you can use the json methods probably
            as->json as->json-pretty
            
            <as-context>
            make-as-context as-context?
            as-context-uri-to-shortname
            as-context-uri-to-as-type
            as-context-shortname-to-as-type
            default-context
            %default-as-contexts %default-as-implied-contexts

            ;; utilities
            vhash->hash-table

            ;; **********
            ;; Base vocab
            ;; **********

            ;; Core classes
            ;; ------------
            <ASObject> as-object <ASLink> as-link
            <Activity> as-activity
            <IntransitiveActivity> as-intransitive-activity
            <Actor> as-activity
            <Collection> as-collection
            <OrderedCollection> as-ordered-collection
            
            ;; Extended classes: Activities
            ;; ----------------------------
            <Accept> as-accept
            <TentativeAccept> as-tentative-accept
            <Add> as-add 
            <Arrive> as-arrive
            <Create> as-create <Delete> as-delete
            <Favorite> as-favorite <Follow> as-follow <Ignore> as-ignore
            <Join> as-join <Leave> as-leave
            <Like> as-like
            <Offer> as-offer <Invite> as-invite
            <Reject> as-reject <TentativeReject> as-tentative-reject
            <Remove> as-remove <Share> as-share
            <Undo> as-undo <Update> as-update
            <Experience> as-experience <View> as-view
            <Watch> as-watch <Read> as-read
            <Move> as-move <Travel> as-travel
            <Announce> as-announce <Block> as-block
            <Flag> as-flag <Dislike> as-dislike
            <Assign> as-assign <Complete> as-complete

            ;; Extended classes: Object types
            ;; ------------------------------
            <Connection> as-connection <Application> as-application
            <Content> as-content
            <Group> as-group <Person> as-person
            <Process> as-process <Service> as-service
            <Article> as-article
            <Album> as-album <Folder> as-folder <Story> as-story
            <Document> as-document <Audio> as-audio
            <Image> as-image <Video> as-video <Note> as-note
            <Question> as-question
            <Event> as-event <Place> as-place <Mention> as-mention
            <Profile> as-profile))

(define-record-type <as-type>
  (make-as-type uri parents props)
  as-type?
  (uri as-type-uri)
  (parents as-type-parents)
  (props as-type-props))

(set-record-type-printer!
 <as-type>
 (lambda (record port)
   (format port "#<as-type: ~s>" (as-type-uri record))))

;; ActivityStreams type Object (or Link)
(define-record-type <as-obj>
  (make-as-obj-internal type fields contexts implied-contexts)
  as-obj?
  (type as-type)
  (fields as-fields)
  ;; Implied contexts affect expansion of types and etc, but aren't
  ;; explicitly put in @context
  ;; 
  ;; Contexts do affect expansion of types, and also appear in @context
  (implied-contexts as-implied-contexts)
  (contexts as-contexts)
  ;; Not public
  (--json-promise-- as--json-promise as--set-json-promise))

;; TODO: support printing the @id of the object, if exists
(set-record-type-printer!
 <as-obj>
 (lambda (record port)
   (format port "#<<as-obj> type: ~a>" (as-type record))))

(define (vhash->hash-table vhash)
  "Convert VHASH into a hash table."
  (vhash-fold (lambda (key value hashtable)
                (let ((unique-sym (gensym)))
                  (if (eq? unique-sym
                           (hash-ref hashtable key unique-sym))
                      (hash-set! hashtable key value)))
                hashtable)
              (make-hash-table)
              vhash))

(define (as-obj-type-string as-obj)
  "Get the type as a string for an ActivityStreams object.

This is affected by the context."
  (let ((uri (as-type-uri (as-type as-obj))))
    (or
     (let loop ((contexts (append (as-contexts as-obj)
                                  (as-implied-contexts as-obj))))
       (if (null? contexts)
           #f
           (let ((type (as-context-uri-to-shortname
                        (car contexts) uri)))
             (if type
                 type
                 (loop (cdr contexts))))))
     uri)))

(define (as->hash as-obj)
  ;; TODO: handle contexts
  (let ((as-hash (make-hash-table)))
    (hash-for-each-handle
     (lambda (handle)
       (let ((key (car handle))
             (val (cdr handle)))
         (if (as-obj? val)
             (hash-set! as-hash key (as->hash val))
             (hash-set! as-hash key val))))
     (vhash->hash-table (as-fields as-obj)))
    (hash-set! as-hash "@type"
               (as-obj-type-string as-obj))
    as-hash))

(define* (as->json-internal as-obj #:key (pretty #f))
  (scm->json-string
   (as->hash as-obj) #:pretty pretty))

(define* (make-as-obj type fields
                      #:key
                      (contexts (%default-as-contexts))
                      (implied-contexts (%default-as-implied-contexts)))
  "Constructor for making activitystreams objects

TYPE is an <as-type> and FIELDS is a vhash of fields.

In general it is recommended that you use (make-as) instead,
which has a much more friendly syntax using keywords."
  (let ((as-obj (make-as-obj-internal type fields contexts implied-contexts)))
    ;; Yes, its promise field references itself ;p
    ;; 
    ;; Alternately, we could make the promise reference just the type
    ;; and fields?
    (as--set-json-promise
     as-obj (delay (as->json-internal as-obj)))
    as-obj))

(define (make-as type . fields)
  "Easily make an activitystreams object from keyword arguments

The first argument after type may be a string for the @id uri.  All
remaining arguments are keyword arguments used to construct the object
fields.

Example usage:
  (define root-beer-note
    (make-as <Post>
      ;; Putting the string first is the same thing as #:@id
      \"http://tsyesika.co.uk/act/foo-id-here/\"
      #:actor (make-as <Person>
                #:@id \"http://tsyesika.co.uk\"
                #:displayName \"Jessica Tallon\")
      #:to (list \"acct:cwebber@identi.ca\")
      #:object (make-as <Note>
                 \"http://tsyesika.co.uk/chat/sup-yo/\"
                 #:content \"Up for some root beer floats?\")))

Note that you can't supply the context or supplied contexts via make-as,
due to this aiming for simplicity.  However, if you want to take a
lazy route, you can use (parameterize) on the
%default-as-contexts and %default-as-implied-contexts fields.
"
  (define (vhash-fields fields)
    (let loop ((fields fields)
               (hashed-fields vlist-null))
      (match fields
        (((? keyword? key) val . rest)
         (loop rest
               (vhash-cons (symbol->string (keyword->symbol key))
                           val hashed-fields)))
        (()
         hashed-fields))))

  (match fields
    (((? string? id) rest ...)
     (make-as-obj type
                  (vhash-cons "@id" id
                              (vhash-fields rest))))
    ((. fields)
     (make-as-obj type (vhash-fields fields)))))

(define-syntax-rule (make-as-obj-factory proc-name type)
  (define (proc-name . args)
    (apply make-as type args)))

(define (as->json as-obj)
  (force (as--json-promise as-obj)))

(define (as->json-pretty as-obj)
  (as->json-internal as-obj #:pretty #t))

(define* (hash->as-obj hashed-json
                       #:key
                       (contexts (%default-as-contexts))
                       (implied-contexts (%default-as-implied-contexts)))
  (define (is-hash-and-looks-like-as-obj? obj)
    (and (hash-table? obj)
         (hash-ref obj "@type")))

  (define (get-fields-and-type)
    "Returned as a cons cell of (fields . type)"
    (hash-fold
     (lambda (key value prior)
       (let ((fields (car prior))
             (type (cdr prior)))
         (cond
          ((equal? key "@type")
           (cons fields
                 (as-contexts-resolve-type-string
                  (append contexts implied-contexts)
                  value)))
          ;; TODO: recurse on sub as-obj's
          ((is-hash-and-looks-like-as-obj? value)
           (cons
            (vhash-cons key
                        (hash->as-obj
                         value
                         #:contexts contexts
                         #:implied-contexts implied-contexts)
                        fields)
            type))
          (else
           (cons
            (vhash-cons key value fields)
            type)))))
     (cons vlist-null #f)
     hashed-json))

  (let* ((fields-and-type (get-fields-and-type))
         (fields (car fields-and-type))
         (type (cdr fields-and-type)))
    (if (not type)
        (throw 'no-valid-as-type))
    (make-as-obj type fields
                 #:contexts contexts
                 #:implied-contexts implied-contexts)))

(define* (json->as-obj json-string
                       #:key
                       (contexts (%default-as-contexts))
                       (implied-contexts (%default-as-implied-contexts)))
  "Convert json string into as-obj (recursively if need be)"
  (hash->as-obj (call-with-input-string json-string json->scm)
                #:contexts contexts
                #:implied-contexts implied-contexts))


(define-syntax define-astype
  (syntax-rules ()
    "Define an activitystream class / type"
    ((define-astype astype (parent ...)
       as-uri as-properties)
     (define astype (make-as-type as-uri (list parent ...)
                                   as-properties)))
    ;; invoke the macro above, but also add an object factory function
    ((define-astype astype (parent ...)
       as-uri as-properties factory-name)
     ;; invoke the function
     (begin
       (define-astype astype (parent ...)
         as-uri as-properties)
       ;; but also make a factory function
       (make-as-obj-factory factory-name astype)))))


;;; ******************
;;; * Standard vocab *
;;; * ============== *
;;; ******************

;;;; TODO: move all the as-object and friends sugar into sugar.scm?

;; ========================================
;; Core classes
;; ========================================

(define-astype <ASObject> ()
  "http://www.w3.org/ns/activitystreams#Object"
  '(alias attachment attributedTo content
          context displayName endTime generator icon
          image inReplyTo location preview published
          replies scope startTime summary tag title
          updated url)
  as-object)

(define-astype <ASLink> ()
  "http://www.w3.org/ns/activitystreams#Link"
  '(href rel mediaType displayName title
         hreflang height width duration)
  as-link)

(define-astype <Activity> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Activity"
  '(actor object target result origin priority
          to bto cc bcc)
  as-activity)

;;; In this one, the actor is a direct object of action as opposed to
;;; the object property
;;; 
;;; ... technically this does NOT inherit the "object" field, but our
;;; algorithm for determining available properties assumes it does...

(define-astype <IntransitiveActivity> (<Activity>)
  "http://www.w3.org/ns/activitystreams#IntransitiveActivity"
  '()
  as-intransitive-activity)

(define-astype <Actor> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Actor"
  '()
  as-actor)

(define-astype <Collection> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Collection"
  '(totalItems itemsPerPage current next prev
               first last self items)
  as-collection)

(define-astype <OrderedCollection> (<Collection>)
  "http://www.w3.org/ns/activitystreams#OrderedCollection"
  '(startIndex)
  as-ordered-collection)


;; ========================================
;; Extended classes: Activity types
;; ========================================

(define-astype <Accept> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Accept"
  '()
  as-accept)

(define-astype <TentativeAccept> (<Accept>)
  "http://www.w3.org/ns/activitystreams#TentativeAccept"
  '()
  as-tentative-accept)

(define-astype <Add> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Add"
  '()
  as-add)

(define-astype <Arrive> (<IntransitiveActivity>)
  "http://www.w3.org/ns/activitystreams#Arrive"
  '()
  as-arrive)

(define-astype <Create> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Create"
  '()
  as-create)

(define-astype <Delete> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Delete"
  '()
  as-delete)

(define-astype <Favorite> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Favorite"
  '()
  as-favorite)

(define-astype <Follow> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Follow"
  '()
  as-follow)

(define-astype <Ignore> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Ignore"
  '()
  as-ignore)

(define-astype <Join> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Join"
  '()
  as-join)

(define-astype <Leave> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Leave"
  '()
  as-leave)

(define-astype <Like> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Like"
  '()
  as-like)

(define-astype <Offer> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Offer"
  '()
  as-offer)

(define-astype <Invite> (<Offer>)
  "http://www.w3.org/ns/activitystreams#Invite"
  '()
  as-invite)

(define-astype <Reject> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Reject"
  '()
  as-reject)

(define-astype <TentativeReject> (<Reject>)
  "http://www.w3.org/ns/activitystreams#TentativeReject"
  '()
  as-tentative-reject)

(define-astype <Remove> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Remove"
  '()
  as-remove)

(define-astype <Share> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Share"
  '()
  as-share)

(define-astype <Undo> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Undo"
  '()
  as-undo)

(define-astype <Update> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Update"
  '()
  as-update)

(define-astype <Experience> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Experience"
  '()
  as-experience)

(define-astype <View> (<Experience>)
  "http://www.w3.org/ns/activitystreams#View"
  '()
  as-view)

;;; I'd love to see this one dropped :P
;;; see: https://github.com/jasnell/w3c-socialwg-activitystreams/issues/113
(define-astype <Watch> (<View>)
  "http://www.w3.org/ns/activitystreams#Watch"
  '()
  as-watch)

;;; I'd love to see this one become a subclass of Read
;;; see: https://github.com/jasnell/w3c-socialwg-activitystreams/issues/114
(define-astype <Read> (<View>)
  "http://www.w3.org/ns/activitystreams#Read"
  '()
  as-read)

(define-astype <Move> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Move"
  '()
  as-move)

(define-astype <Travel> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Travel"
  '()
  as-travel)

(define-astype <Announce> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Announce"
  '()
  as-announce)

(define-astype <Block> (<Ignore>)
  "http://www.w3.org/ns/activitystreams#Block"
  '()
  as-block)

(define-astype <Flag> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Flag"
  '()
  as-flag)

(define-astype <Dislike> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Dislike"
  '()
  as-dislike)

(define-astype <Assign> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Assign"
  '()
  as-assign)

(define-astype <Complete> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Complete"
  '()
  as-complete)


;; ========================================
;; Extended classes: Object types
;; ========================================

(define-astype <Connection> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Connection"
  '(a b relationship)
  as-connection)

(define-astype <Application> (<Actor>)
  "http://www.w3.org/ns/activitystreams#Application"
  '()
  as-application)

(define-astype <Content> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Content"
  '(duration width height)
  as-content)

(define-astype <Group> (<Actor>)
  "http://www.w3.org/ns/activitystreams#Group"
  '()
  as-group)

(define-astype <Person> (<Actor>)
  "http://www.w3.org/ns/activitystreams#Person"
  '()
  as-person)

(define-astype <Process> (<Actor>)
  "http://www.w3.org/ns/activitystreams#Process"
  '()
  as-process)

(define-astype <Service> (<Actor>)
  "http://www.w3.org/ns/activitystreams#Service"
  '()
  as-service)

(define-astype <Article> (<Content>)
  "http://www.w3.org/ns/activitystreams#Article"
  '()
  as-article)

(define-astype <Album> (<Collection>)
  "http://www.w3.org/ns/activitystreams#Album"
  '()
  as-album)

(define-astype <Folder> (<Collection>)
  "http://www.w3.org/ns/activitystreams#Folder"
  '()
  as-folder)

(define-astype <Story> (<OrderedCollection>)
  "http://www.w3.org/ns/activitystreams#Folder"
  '()
  as-story)

(define-astype <Document> (<Content>)
  "http://www.w3.org/ns/activitystreams#Document"
  '()
  as-document)

(define-astype <Audio> (<Document>)
  "http://www.w3.org/ns/activitystreams#Audio"
  '()
  as-audio)

(define-astype <Image> (<Document>)
  "http://www.w3.org/ns/activitystreams#Image"
  '()
  as-image)

(define-astype <Video> (<Document>)
  "http://www.w3.org/ns/activitystreams#Video"
  '()
  as-video)

(define-astype <Note> (<Content>)
  "http://www.w3.org/ns/activitystreams#Note"
  '()
  as-note)

(define-astype <Question> (<Content> <IntransitiveActivity>)
  "http://www.w3.org/ns/activitystreams#Question"
  '(oneOf anyOf)
  as-question)

(define-astype <Event> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Event"
  '()
  as-event)

(define-astype <Place> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Place"
  '(accuracy altitude latitude longitude radius units)
  as-place)

(define-astype <Mention> (<ASLink>)
  "http://www.w3.org/ns/activitystreams#Link"
  '()
  as-mention)

(define-astype <Profile> (<Content>)
  "http://www.w3.org/ns/activitystreams#Profile"
  '()
  as-profile)


;; -------------------------------
;; activitystreams pseudo-contexts
;; -------------------------------


;;; All the mappings are set up at time that (make-as-context) is invoked

;; TODO: Provide an optional uri for the context itself

(define-record-type <as-context>
  (make-as-context-internal uri-to-shortname-map
                            uri-to-as-type-map
                            shortname-to-as-type-map)
  as-context?
  ;; This one is the uri -> shortname, but is not generally
  ;; used
  ;;; Define mappings:
  ;;;  - uri -> shortname
  ;;;  - uri -> <as-type>
  ;;;  - shortname -> <as-type>
  (uri-to-shortname-map as-context-uri-to-shortname-map)
  (uri-to-as-type-map as-context-uri-to-as-type-map)
  (shortname-to-as-type-map as-context-shortname-to-as-type-map))

;; Constructor
(define (make-as-context mapping-list)
  "Build an <as-context> object from an alist with entries of
    (<as-type> . shortname)"
  (make-as-context-internal
   (alist->hash-table
    (map
     (lambda (entry)
       (cons
        (as-type-uri (car entry))
        (cdr entry)))
     mapping-list))
   (alist->hash-table
    (map
     (lambda (entry)
       (cons
        (as-type-uri (car entry))
        (car entry)))
     mapping-list))
   (alist->hash-table
    (map
     (lambda (entry)
       (cons
        (cdr entry)
        (car entry)))
     mapping-list))))


;; Access the mapping, public methods

(define (as-context-uri-to-shortname as-context uri)
  (hash-ref (as-context-uri-to-shortname-map as-context) uri))

(define (as-context-uri-to-as-type as-context uri)
  (hash-ref (as-context-uri-to-as-type-map as-context) uri))

(define (as-context-shortname-to-as-type as-context shortname)
  (hash-ref (as-context-shortname-to-as-type-map as-context) shortname))

(define* (as-contexts-resolve-type-string as-contexts type-str)
  "Given a set of as-contexts, try to find a type object from type-str"
  (define (as-type-finder find-method)
    (lambda ()
      (let loop ((contexts as-contexts))
        (if (null? contexts)
            #f
            (let ((type (find-method
                         (car contexts) type-str)))
              (if type
                  type
                  (loop (cdr contexts))))))))

  (define find-as-type-from-uri
    (as-type-finder as-context-uri-to-as-type))

  (define find-as-type-from-shortname
    (as-type-finder as-context-shortname-to-as-type))

  (if (string->uri type-str)
      ;; Okay, it *is* a URI
      (or
       (find-as-type-from-uri)
       (throw 'as-type-not-found-in-contexts))
      (or
       (find-as-type-from-shortname)
       (throw 'as-type-not-found-in-contexts))))

;;; ********************
;;; * Default contexts *
;;; * ================ *
;;; ********************

(define default-context
  (make-as-context
   `((,<Accept> . "Accept")
     (,<Activity> . "Activity")
     (,<IntransitiveActivity> . "IntransitiveActivity")
     (,<Actor> . "Actor")
     (,<Add> . "Add")
     (,<Album> . "Album")
     (,<Announce> . "Announce")
     (,<Application> . "Application")
     (,<Arrive> . "Arrive")
     (,<Article> . "Article")
     (,<Audio> . "Audio")
     (,<Block> . "Block")
     (,<Collection> . "Collection")
     (,<Connection> . "Connection")
     (,<Content> . "Content")
     (,<Create> . "Create")
     (,<Delete> . "Delete")
     (,<Dislike> . "Dislike")
     (,<Document> . "Document")
     (,<Event> . "Event")
     (,<Favorite> . "Favorite")
     (,<Folder> . "Folder")
     (,<Follow> . "Follow")
     (,<Flag> . "Flag")
     (,<Group> . "Group")
     (,<Ignore> . "Ignore")
     (,<Image> . "Image")
     (,<Invite> . "Invite")
     (,<Join> . "Join")
     (,<Leave> . "Leave")
     (,<Like> . "Like")
     (,<ASLink> . "Link")
     (,<Mention> . "Mention")
     (,<Note> . "Note")
     (,<ASObject> . "Object")
     (,<Offer> . "Offer")
     (,<OrderedCollection> . "OrderedCollection")
     (,<Person> . "Person")
     (,<Place> . "Place")
     (,<Process> . "Process")
     (,<Profile> . "Profile")
     (,<Question> . "Question")
     (,<Reject> . "Reject")
     (,<Remove> . "Remove")
     (,<Service> . "Service")
     (,<Story> . "Story")
     (,<TentativeAccept> . "TentativeAccept")
     (,<TentativeReject> . "TentativeReject")
     (,<Undo> . "Undo")
     (,<Update> . "Update")
     (,<Video> . "Video")
     (,<Experience> . "Experience")
     (,<View> . "View")
     (,<Read> . "Read")
     (,<Move> . "Move")
     (,<Travel> . "Travel"))))

(define %default-as-contexts
  (make-parameter '()))
(define %default-as-implied-contexts
  (make-parameter
   (list default-context)))
