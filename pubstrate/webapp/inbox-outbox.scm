 ;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; This file is part of Pubstrate.
;;;
;;; Pubstrate is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Pubstrate is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Pubstrate.  If not, see <http://www.gnu.org/licenses/>.

(define-module (pubstrate webapp inbox-outbox)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (logging logger)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (sjson utils)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate date)
  #:use-module (pubstrate generics)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp utils)
  #:use-module (pubstrate webapp auth) ; for gen-bearer-token
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (pubstrate vocab)
  #:use-module ((pubstrate shorthand)
                #:renamer (symbol-prefix-proc 'as:))
  #:export (collect-recipients
            deliver-asobj
            actor-post-asobj-to-inbox!
            get-asobj post-asobj-to-actor

            asobj-outbox-effects!
            asobj-inbox-effects!

            <effect-error> effect-error?
            effect-error-message

            gen-asobj-id-for-username
            gen-asobj-id-for-user))


;;; Federation delivery tooling

;;; TODO: This shares code with apclient.scm
(define* (http-get-asobj id #:key (extra-headers '()))
  ;; TODO: handle errors!
  (receive (response body)
      (http-get id
                #:headers `((accept . ((application/activity+json)))
                            ,@extra-headers))
    (make-asobj
     (read-json-from-string
      (if (bytevector? body)
          (utf8->string body)
          body))
     (%default-env))))

;; @@: Maybe store-new should 
;; TODO: This needs *way* more async support
(define* (get-asobj id #:key (store-new #t))
  "Retrieve an asobj, either from the current store or by fetching
from the web if necessary."
  (let ((store (ctx-ref 'store))
        (id-uri (string->uri id)))
    (cond
     ;; Return the version from the store, if we have that
     ((store-asobj-ref store id) => identity)

     ;; We can handle this scheme type, right?
     ((member (uri-scheme id-uri) '(http https))
      ;; TODO: Error handling....!
      (let ((result
             ;; What's up with cons'ing the id onto here?  It might
             ;; already have an id!
             ;; Well, it turns out there's a good reason.
             (asobj-set (http-get-asobj id)
                        "id" id)))
        (if store-new
            (store-asobj-set! store result))
        result))
     
     (else
      (log-msg 'INFO
               (format #f "Can't fetch activity ~s: URI scheme not handled"
                       id))
      #f))))


;; TODO: Eventually, we'll want to look up recipient profiles using
;;   a user's permissions / signed requests
(define* (collect-recipients asobj #:key (store-new #t))
  "Collect a list of all actors to deliver to

Note that this has potential side effects; it fetches objects from
the store, but also if it does not yet have references to these objects,
it will fetch them remotely from the web.  It may even stick them
in the store!"
  (define (append-actor-or-collection id lst)
    ;; TODO: Dereference collections...!
    ;; TODO Handle inline actors!
    (let ((actor-obj (get-asobj id #:store-new store-new)))
      (if actor-obj
          (cons actor-obj lst)
          lst)))
  (define (asobj-key-as-list asobj key)
    (let ((result (asobj-ref asobj key '())))
      (if (json-array? result)
          result
          (list result))))
  (define (field-collector key)
    (lambda (collected)
      (fold (lambda (item prev)
              (match item
                ((? string? _)
                 (append-actor-or-collection item prev))
                ((? asobj? _)
                 (cond
                  ;; Cool, the object already has an inbox,
                  ;; use that
                  ((asobj-ref item "inbox")
                   (cons item prev))
                  ;; No inbox attached to asobj, so let's go
                  ;; fetch it
                  ((asobj-id item)
                   (append-actor-or-collection item prev))
                  ;; What!  The actor doesn't even have an id.
                  ;; Ignore it.
                  (else
                   (log-msg 'INFO "Can't send to actor without inbox or id!")
                   prev)))
                ;; We don't know what this is.
                (_
                 ;; TODO: Log this
                 prev)))
            collected
            (asobj-key-as-list asobj key))))
  (define collect-to (field-collector "to"))
  (define collect-cc (field-collector "cc"))
  (define collect-bcc (field-collector "bcc"))
  (define collect-bto (field-collector "bto"))
  (define collect-em-all
    (compose collect-to collect-cc collect-bcc collect-bto))

  (values
   (collect-em-all '())
   (asobj-delete (asobj-delete asobj "bto") "bcc")))


(define (actor-post-asobj-to-inbox! actor asobj)
  "Add ASOBJ to actor's inbox, posibly saving in the process.

Returns #t if the object is added to the inbox, #f otherwise."
  (define store (ctx-ref 'store))
  (define id (asobj-id asobj))
  (define (asobj-acceptability asobj)
    ;; TODO
    ;; accept not-authorized invalid reject
    'accept)
  (case (asobj-acceptability asobj)
    ((accept)
     ;; Add this object to the store if it's not there already
     (if (not (store-asobj-ref store id))
         (store-asobj-set! store asobj))
     ;; Add to the actor's inbox
     (when (not (user-inbox-member? store actor id))
       (asobj-inbox-effects! asobj actor)
       (user-add-to-inbox! store actor id)))
    (else
     'TODO)))

;; TODO: Provide auth of any sort?
(define (post-asobj-to-actor asobj actor)
  "Post ASOBJ to inbox-uri"
  (define (post-remotely)
    (define headers
      '((content-type application/activity+json (charset . "utf-8"))))
    ;; TODO: retry if this fails
    (define inbox-uri (asobj-ref actor "inbox"))
    (if inbox-uri
        (http-post inbox-uri
                   #:body (asobj->string asobj)
                   #:headers headers)))
  (define (post-locally)
    (actor-post-asobj-to-inbox! actor asobj))
  ;; TODO: Treat posting locally differently
  (if (asobj-local? actor)
      (post-locally)
      (post-remotely)))

(define* (deliver-asobj asobj recipients #:key (store-new #t))
  "Send activitystreams object to recipients."
  (for-each (cut post-asobj-to-actor asobj <>)
            recipients))



;;; *** SIDE EFFECTS STUFF ***
;;; ==========================
;;; So, many activities, upon being received either to the inbox or
;;; the outbox, have some sort of side effect.  They might create another
;;; object, or add to a followers/following collection, etc.
;;; ======================




;;; Utilities

(define %nothing (list '*nothing*))

(define-syntax-rule (let-asobj-fields asobj
                                      ((var field) ...)
                                      body ...)
  "Bind asobj fields to vars and evaluate body.  If
a field isn't found, we throw an exception."
  (let ((var (let ((val (asobj-ref asobj field %nothing)))
               (if (eq? val %nothing)
                   (throw 'effect-error
                          "Missing field on activitystreams object"
                          #:asobj asobj
                          #:field field)
                   val))) ...)
    body ...))

;; @@: Hardly seems like this belongs here.  Where else to put it?
(define (gen-asobj-id-for-username username)
  "Generate a fresh asobj id for this username"
  (abs-local-uri "u" username "p" (gen-bearer-token 30)))

(define (gen-asobj-id-for-user user)
  "Generate a fresh asobj id for this username"
  (gen-asobj-id-for-username
   (asobj-ref user "preferredUsername")))

(define (save-asobj! asobj)
  (store-asobj-set! (ctx-ref 'store) asobj))

(define (string-uri? obj)
  (and (string? obj) (string->uri obj)))

(define (id-or-id-from obj)
  (match obj
    ((? string-uri? id) id)
    ((? asobj? obj)
     (asobj-id obj))
    (_ #f)))


;;; Posting to outbox generics.  AKA client to server interactions.

(define-as-generic asobj-outbox-effects!
  "Handle all side effects related to receiving the asobj to the
outbox.  It doesn't include saving or delivery of the asobj itself
(though it might of its components), since that is done separately.

We return a (potentially) modified asobj.  If for some reason
the asobj can't be saved, an 'effect-error exception will be
thrown.")

(define (incoming-activity-common-tweaks asobj outbox-user)
  ;; TODO: Also strip out any @id that may have been attached...
  ((compose
    (cut asobj-delete <> "@id")
    (cut asobj-set <> "id" (gen-asobj-id-for-user outbox-user))
    ;; Copy the id of our actor onto the actor field
    (cut asobj-set <> "actor" (asobj-id outbox-user))
    (cut asobj-set <> "published"
         (date->rfc3339-string (current-date 0))))
   asobj))

(define-as-method (asobj-outbox-effects! (asobj ^Create)
                                         outbox-user)
  (let ((asobj (incoming-activity-common-tweaks asobj outbox-user)))
    (let-asobj-fields
     asobj ((object "object"))
     (let* (;; Perform common tweaks: add an id, published date (copied from),
            ;; etc.
            (tweak-object
             (compose
              (lambda (object)
                (incoming-activity-common-tweaks object outbox-user))
              (lambda (object)
                (asobj-set object "id"
                           (gen-asobj-id-for-user outbox-user)))
              ;; Copy the actor over to attributedTo
              (lambda (object)
                (asobj-set object "attributedTo"
                           (asobj-ref asobj "actor")))
              ;; Copy the published date
              (lambda (object)
                (asobj-set object "published"
                           (asobj-ref asobj "published")))))
            (object
             ;; saving is also done here, as well as any final modifications
             ;; by the object.
             (create-outbox-object! (tweak-object object)
                                    asobj outbox-user))
            (asobj
             ;; We replace the asobj's object reference with just the
             ;; identifier for the object rather than the object itself
             (asobj-set asobj "object" (asobj-id object))))
       asobj))))

(define-as-generic create-outbox-object!
  "Do any final tweaks to the object of an object on a Create
activity, as well as run any side effects it may itself have, then
save it and return it.")

(define-as-method (create-outbox-object! (object ^Object) create-asobj
                                         outbox-user)
  (save-asobj! object)
  object)

(define-as-method (create-outbox-object! (object ^Activity) create-asobj
                                         outbox-user)
  (throw 'effect-error "Can't create an Activity."
         #:asobj create-asobj
         #:object object))

;; @@: Hacky, maybe we shouldn't be creating questions like this?
(define-as-method (create-outbox-object! (object ^Question) create-asobj
                                         outbox-user)
  (save-asobj! object)
  object)

;;; The following two versions are the least common and least interesting
;;; of all the asobj implementations.

(define-as-method (asobj-outbox-effects! (asobj ^Activity) outbox-user)
  (throw 'effect-error "Don't know how to process activity with this activity type"
         #:asobj asobj
         #:astypes (asobj-types asobj)))


;;; If it's an object and not an activity, we wrap in a Create and run
;;; asobj-outbox-effects! on the wrapped object.
(define-as-method (asobj-outbox-effects! (asobj ^Object) outbox-user)
  (let* ((asobj (incoming-activity-common-tweaks asobj outbox-user))
         (copy-props
          (lambda (from-asobj to-asobj props)
            (fold (lambda (prop to-asobj)
                    (let ((from-val (asobj-ref from-asobj prop %nothing)))
                      (if (not (eq? from-val %nothing))
                          (asobj-set to-asobj prop from-val)
                          to-asobj)))
                  to-asobj
                  props)))
         (wrapped-asobj
          (copy-props asobj
                      (as:create #:id (gen-asobj-id-for-user outbox-user)
                                 #:object asobj)
                      '("actor" "published" "to" "cc" "bcc" "bto"))))
    (asobj-outbox-effects! wrapped-asobj outbox-user)))

;;; Follow
(define-as-method (asobj-outbox-effects! (asobj ^Follow)
                                         outbox-user)
  (let ((asobj (incoming-activity-common-tweaks asobj outbox-user)))
    (let-asobj-fields
     asobj ((object "object"))
     (let* ((follow-uri (match object
                          ((? string-uri? _)
                           object)
                          ((? asobj? _)
                           (match (asobj-id object)
                             ((? string-uri? uri)
                              uri)
                             (_ (throw 'effect-error
                                       "Follow activity's object has no id"
                                       #:asobj asobj))))))
            ;; Add follow-uri to the bcc list.  It doesn't matter
            ;; if there's already a bcc item, since recipients should be
            ;; de-duped.
            (asobj (asobj-set asobj "bcc"
                              (cons follow-uri
                                    (asobj-ref asobj "bcc" '())))))
       ;; Add to following list
       ;; TODO: Do we need to check if we're already subscribed?
       (user-add-to-following! (ctx-ref 'store) outbox-user
                               follow-uri)

       ;; Return asobj
       asobj))))

(define-as-method (asobj-outbox-effects! (asobj ^Delete)
                                         outbox-user)
  (let ((asobj (incoming-activity-common-tweaks asobj outbox-user)))
    (let-asobj-fields
     asobj ((object "object"))
     (let* ((object-id (id-or-id-from object))
            (stored-object (store-asobj-ref (ctx-ref 'store) object-id))
            ;; TODO: This isn't necessarily correct for checking permissions,
            ;;   but it'll do for the second.  We need a more robust permission
            ;;   system.
            (outbox-user-id (asobj-id outbox-user))
            (can-edit?
             (or (equal? (id-or-id-from (asobj-ref stored-object
                                                   "actor"))
                         outbox-user-id)
                 (equal? (id-or-id-from (asobj-ref stored-object
                                                   "attributedTo"))
                         outbox-user-id))))
       ;; TODO: We need a more way to handle tracking permissions around
       ;;   who can edit an object
       (if can-edit?
           ;; We'll delete the object, or rather replace it with a new version
           ;; that keeps the private data intact.
           ;; @@: *should* we keep the private data around?  Feels like we should
           ;;   until we know better?  Mayyyyyyyyybe?
           (let ((tombstone
                  (make-asobj `(@ ("type" "Tombstone")
                                  ("id" ,object-id)
                                  ("formerType" ,((@@ (pubstrate asobj) asobj-type-field)
                                                  stored-object))
                                  ("deleted" ,(date->rfc3339-string (current-date 0))))
                              (asobj-env asobj)
                              (asobj-private stored-object))))
             (save-asobj! tombstone)
             ;; make sure the saved Delete object just refers to object by id,
             ;; not structure
             (asobj-set asobj "object" object-id))
           (throw 'effect-error
                  "User doesn't have permission to delete this object."
                  #:asobj asobj))))))

(define %update-properties-blacklist
  '(;; Playing it safe, haven't really tested updating the "authors" of posts
    "actor" "attributedTo"
    ;; Definitely can't update type or context or id
    "type" "@type" "id" "@id" "@context"
    ;; No updating the addressing, at least not yet
    "audience" "bcc" "bto" "cc" "to"
    ;; AFAIK it doesn't make sense to update the collection type properties
    ;; "current" "first" "last"
    "current" "first" "last" "next" "items"
    ;; Can't update the published time?
    ;; (I'm not sure about this one...)
    "published" "updated"
    ;; Not relevant to anything other than Tombstones
    "deleted" "formerType"))

(define-as-method (asobj-outbox-effects! (asobj ^Update) outbox-user)
  (let* ((former-asobj asobj)
         (asobj (incoming-activity-common-tweaks asobj outbox-user))
         (object-from-update (asobj-ref asobj "object"))
         (object-id (asobj-id object-from-update))
         (stored-object (and object-id
                             (store-asobj-ref (ctx-ref 'store) object-id))))
    ;; @@: This is the same as in the ^Delete code...
    (define (can-update?)
      (define outbox-user-id (asobj-id outbox-user))
      (or (equal? (id-or-id-from (asobj-ref stored-object
                                            "actor"))
                  outbox-user-id)
          (equal? (id-or-id-from (asobj-ref stored-object
                                            "attributedTo"))
                  outbox-user-id)))
    ;; Do we have something to update?
    (when (not (or object-from-update object-id))
      (throw 'effect-error
             "No object with an id to update."))
    (when (or (not stored-object)
              (not (can-update?)))
      (throw 'effect-error
             "User has no permission to update this object or object does not exist."))
    ;; Otherwise, we're good to go.
    ;; Update the old object based on the new fields.
    ;; But which fields are okay to update?
    (let* ((new-object (jsobj-fold
                        (lambda (key val obj)
                          (if (member key %update-properties-blacklist)
                              obj
                              (asobj-set obj key val)))
                        stored-object
                        (asobj-sjson object-from-update)))
           (new-create (asobj-set asobj "object" new-object)))
      ;; TODO: We want to leanify this structure...
      (save-asobj! new-object)
      new-create)))


;;; Posting to inbox generics.  AKA server to server / federation interactions.

;;; TODO: We need a LOT more checks here to make sure we "trust" the
;;;   content we've received via federation!


(define-as-generic asobj-inbox-effects!
  "Handle all side effects related to receiving the asobj to the
inbox.  This includes saving to the database, if appropriate(???).

We return a (potentially) modified asobj.  If for some reason
the asobj can't be saved, an 'effect-error exception will be
thrown.")

(define-as-method (asobj-inbox-effects! (asobj ^Create)
                                        inbox-user)
  (let-asobj-fields
   asobj ((object "object"))
   (let ((asobj (asobj-set asobj "object" object)))
     ;; We need to break this apart more, but for now...
     ;; TODO: Also do verification that this object really is what it
     ;;   claims to be
     (save-asobj! object))))

(define-as-method (asobj-inbox-effects! (asobj ^Follow)
                                        inbox-user)
  (let-asobj-fields
   asobj ((object "object")
          (actor "actor"))
   ;; TODO: Again, we need to "audit" this incoming message
   (user-add-to-followers! (ctx-ref 'store) inbox-user
                           (match actor
                             ((? asobj? _)
                              (asobj-id actor))
                             ((? string-uri? uri)
                              uri)))))

;;; In the case of server to server federation, we don't do the "implied
;;; Create" that we do for client to server when it's an object without
;;; an Activity.  So really, any Object that isn't one of the recognized
;;; Activities counts as not recognized.

(define-as-method (asobj-inbox-effects! (asobj ^Object)
                                        inbox-user)
  (throw 'effect-error "Don't know how to process this activitystreams object"
         #:asobj asobj))

