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

(use-modules (oop goops)
             (ice-9 match)
             (ice-9 hash-table)
             (json))

(define-class <json-ldable> ()
  (properties #:allocation #:each-subclass)
  ;; (mandatory #:allocation #:each-subclass)
  (uri #:allocation #:each-subclass)
  (fields #:init-keyword #:fields #:getter as-fields))
(class-slot-set! <json-ldable> 'properties
                 '(@context @type @id))

;; emacs: (put 'define-asclass 'scheme-indent-function 2)

;;; This is a shortcut for defining activitystreams-style objects
;;; since there is often a lot of boilerplate

(define-syntax-rule (define-asclass asclass (parent ...)
                      as-uri as-properties . rest)
  (begin
    (define-class asclass (parent ...)
      . rest)
    (class-slot-set! asclass 'uri as-uri)
    (class-slot-set! asclass 'properties as-properties)))

;; Make, activitystream convenience style
;;; Redo as metaobject method
;; (define-generic make-as)
;; (define-method (make-as (class <json-ldable>) . fields)
;;   ...)

(define (make-as class . fields)
  "Easily make an activitystreams object from keyword arguments

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
"
  (define (hash-fields fields)
    (let loop ((fields fields)
               (hashed-fields (make-hash-table)))
      (match fields
        (((? keyword? key) val . rest)
         (begin
           (hash-set! hashed-fields
                      (symbol->string (keyword->symbol key))
                      val)
           (loop rest hashed-fields)))
        (()
         hashed-fields))))
  (match fields
    (((? string? id) rest ...)
     (let ((hashed-fields (hash-fields rest)))
       (begin
         (hash-set! hashed-fields "@id" id)
         (make class #:fields hashed-fields))))
    ((. fields)
     (make class #:fields (hash-fields fields)))))


;;; Recursively convert to hashtable, including all children
(define-generic as-to-hash)
(define-method (as-to-hash (as-object <json-ldable>) contexts)
  (let ((as-hash (make-hash-table)))
    (hash-for-each-handle
     (lambda (handle)
       (let ((key (car handle))
             (val (cdr handle)))
         (if (is-a? val <json-ldable>)
             (hash-set! as-hash key (as-to-hash val))
             (hash-set! as-hash key val))))
     (as-fields as-object))
    (hash-set! as-hash "@type"
               (extract-type-from-contexts-or-uri
                contexts (slot-ref as-object 'uri)))
    as-hash))

(define-method (as-to-hash (as-object <json-ldable>))
  (as-to-hash as-object (list default-context)))


(define-generic as-to-json)
(define-method (as-to-json (as-object <json-ldable>))
  (scm->json-string
   (as-to-hash as-object)))

(define-generic as-to-json-pretty)
(define-method (as-to-json-pretty (as-object <json-ldable>))
  (scm->json-string
   (as-to-hash as-object) #:pretty #t))


;; Utility to gather the list of all relevant properties

;;; NOTE: DOES NOT DELETE DUPLICATES!

(define* (gather-properties class #:optional (prop 'properties))
  "Gather available properties recursively from AS definitions

Use like:
  (gather-properties <Activity>)"
  (let ((listy-results
         (catch 'goops-error
           (lambda () (class-slot-ref class prop))
           (lambda (key . args) #:unbound))))
    (if (eq? #:unbound listy-results)
        '()
        (apply append
               listy-results
               (map (lambda (class) (gather-properties class prop))
                    (class-direct-supers class))))))

;; ========================================
;; Core classes
;; ========================================

(define-asclass <ASObject> (<json-ldable>)
  "http://www.w3.org/ns/activitystreams#Object"
  '(alias attachment attributedTo content
          context displayName endTime generator icon
          image inReplyTo location preview published
          replies scope startTime summary tag title
          updated url))

(define-asclass <ASLink> (<json-ldable>)
  "http://www.w3.org/ns/activitystreams#Link"
  '(href rel mediaType displayName title
         hreflang height width duration))

(define-asclass <Activity> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Activity"
  '(actor object target result origin priority
          to bto cc bcc))

;;; In this one, the actor is a direct object of action as opposed to
;;; the object property
;;; 
;;; ... technically this does NOT inherit the "object" field, but our
;;; algorithm for determining available properties assumes it does...

(define-asclass <IntransitiveActivity> (<Activity>)
  "http://www.w3.org/ns/activitystreams#IntransitiveActivity"
  '())

(define-asclass <Actor> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Actor"
  '())

(define-asclass <Collection> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Collection"
  '(totalItems itemsPerPage current next prev
               first last self items))

(define-asclass <OrderedCollection> (<Collection>)
  "http://www.w3.org/ns/activitystreams#OrderedCollection"
  '(startIndex))


;; ========================================
;; Extended classes: Activity types
;; ========================================

(define-asclass <Accept> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Accept"
  '())

(define-asclass <TentativeAccept> (<Accept>)
  "http://www.w3.org/ns/activitystreams#TentativeAccept"
  '())

(define-asclass <Add> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Add"
  '())

(define-asclass <Arrive> (<IntransitiveActivity>)
  "http://www.w3.org/ns/activitystreams#Arrive"
  '())

(define-asclass <Create> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Create"
  '())

(define-asclass <Delete> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Delete"
  '())

(define-asclass <Favorite> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Favorite"
  '())

(define-asclass <Follow> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Follow"
  '())

(define-asclass <Ignore> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Ignore"
  '())

(define-asclass <Join> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Join"
  '())

(define-asclass <Leave> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Leave"
  '())

(define-asclass <Like> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Like"
  '())

(define-asclass <Offer> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Offer"
  '())

(define-asclass <Invite> (<Offer>)
  "http://www.w3.org/ns/activitystreams#Invite"
  '())

(define-asclass <Reject> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Reject"
  '())

(define-asclass <TentativeReject> (<Reject>)
  "http://www.w3.org/ns/activitystreams#TentativeReject"
  '())

(define-asclass <Remove> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Remove"
  '())

(define-asclass <Share> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Share"
  '())

(define-asclass <Undo> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Undo"
  '())

(define-asclass <Update> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Update"
  '())

(define-asclass <Experience> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Experience"
  '())

(define-asclass <View> (<Experience>)
  "http://www.w3.org/ns/activitystreams#View"
  '())

;;; I'd love to see this one dropped :P
;;; see: https://github.com/jasnell/w3c-socialwg-activitystreams/issues/113
(define-asclass <Watch> (<View>)
  "http://www.w3.org/ns/activitystreams#Watch"
  '())

;;; I'd love to see this one become a subclass of Read
;;; see: https://github.com/jasnell/w3c-socialwg-activitystreams/issues/114
(define-asclass <Read> (<View>)
  "http://www.w3.org/ns/activitystreams#Read"
  '())

(define-asclass <Move> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Move"
  '())

(define-asclass <Travel> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Travel"
  '())

(define-asclass <Announce> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Announce"
  '())

(define-asclass <Block> (<Ignore>)
  "http://www.w3.org/ns/activitystreams#Block"
  '())

(define-asclass <Flag> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Flag"
  '())

(define-asclass <Dislike> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Dislike"
  '())

(define-asclass <Assign> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Assign"
  '())

(define-asclass <Complete> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Complete"
  '())


;; ========================================
;; Extended classes: Object types
;; ========================================

(define-asclass <Connection> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Connection"
  '(a b relationship))

(define-asclass <Application> (<Actor>)
  "http://www.w3.org/ns/activitystreams#Application"
  '())

(define-asclass <Content> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Content"
  '(duration width height))

(define-asclass <Group> (<Actor>)
  "http://www.w3.org/ns/activitystreams#Group"
  '())

(define-asclass <Person> (<Actor>)
  "http://www.w3.org/ns/activitystreams#Person"
  '())

(define-asclass <Process> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Process"
  '())

(define-asclass <Service> (<Actor>)
  "http://www.w3.org/ns/activitystreams#Service"
  '())

(define-asclass <Article> (<Content>)
  "http://www.w3.org/ns/activitystreams#Article"
  '())

(define-asclass <Album> (<Collection>)
  "http://www.w3.org/ns/activitystreams#Album"
  '())

(define-asclass <Folder> (<Collection>)
  "http://www.w3.org/ns/activitystreams#Folder"
  '())

(define-asclass <Story> (<OrderedCollection>)
  "http://www.w3.org/ns/activitystreams#Folder"
  '())

(define-asclass <Document> (<Content>)
  "http://www.w3.org/ns/activitystreams#Document"
  '())

(define-asclass <Audio> (<Document>)
  "http://www.w3.org/ns/activitystreams#Audio"
  '())

(define-asclass <Image> (<Document>)
  "http://www.w3.org/ns/activitystreams#Image"
  '())

(define-asclass <Video> (<Document>)
  "http://www.w3.org/ns/activitystreams#Video"
  '())

(define-asclass <Note> (<Content>)
  "http://www.w3.org/ns/activitystreams#Note"
  '())

(define-asclass <Question> (<Content> <IntransitiveActivity>)
  "http://www.w3.org/ns/activitystreams#Question"
  '(oneOf anyOf))

(define-asclass <Event> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Event"
  '())

(define-asclass <Place> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Place"
  '(accuracy altitude latitude longitude radius units))

(define-asclass <Mention> (<ASLink>)
  "http://www.w3.org/ns/activitystreams#Link"
  '())

(define-asclass <Profile> (<Content>)
  "http://www.w3.org/ns/activitystreams#Profile"
  '())

;; ---------------
;; pseudo-contexts
;; ---------------

(define-class <pseudo-context> ()
  (mapping #:init-keyword #:mapping
           #:accessor pseudo-context-mapping)
  ;;;; This will be necessary when constructing from json.
  ;;;; Should be memoized.
  ;; (reverse-map)
  )

(define-generic extract-type-from-context)
(define-method (extract-type-from-context (context <pseudo-context>) uri)
  "Retreive uri' simple name representation from context"
  (hash-ref (pseudo-context-mapping context) uri #f))

(define (extract-type-from-contexts-or-uri contexts uri)
  (or
   (let loop ((contexts contexts))
     (if (null? contexts)
         #f
         (let ((type (extract-type-from-context
                      (car contexts) uri)))
           (if type
               type
               (loop (cdr contexts))))))
   uri))


(define default-context
  (make <pseudo-context>
    #:mapping
    (alist->hash-table
     ;; This is a dumb way to do things but I'm kinda tired.
     (map (lambda (cell)
            (cons
             (class-slot-ref (car cell) 'uri)
             (cdr cell)))
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
            (,<Travel> . "Travel"))))))
