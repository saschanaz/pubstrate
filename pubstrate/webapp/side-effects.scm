;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

;;; ======================
;;; So, many activities, upon being received either to the inbox or
;;; the outbox, have some sort of side effect.  They might create another
;;; object, or add to a followers/following collection, etc.
;;; ======================

(define-module (pubstrate webapp side-effects)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:use-module (pubstrate generics)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp utils)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp federation)
  #:use-module (pubstrate webapp auth) ; for gen-bearer-token
  #:use-module ((pubstrate shorthand)
                #:renamer (symbol-prefix-proc 'as:))
  #:export (asobj-outbox-effects!
            asobj-inbox-effects!

            <effect-error> effect-error?
            effect-error-message

            gen-asobj-id-for-username
            gen-asobj-id-for-user))


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


;;; Posting to outbox generics.  AKA client to server interactions.

(define-as-generic asobj-outbox-effects!
  "Handle all side effects related to receiving the asobj to the
outbox.  This includes saving to the database, if appropriate(???).
TODO: Still figuring out the above!
It doesn't include delivery, which is done separately.

We return a (potentially) modified asobj.  If for some reason
the asobj can't be saved, an 'effect-error exception will be
thrown.")

(define-as-method (asobj-outbox-effects! (asobj ^Create)
                                          outbox-user)
  (let-asobj-fields
   asobj ((object "object"))
   (let* (;; Perform common tweaks: add an id, published date (copied from),
          ;; etc.
          (tweak-object
           (compose
            ;; TODO: This is the same as in views.scm's user-outbox, we should
            ;;   merge code?
            ;; Add a unique id to the object
            (lambda (object)
              (asobj-cons object "id"
                          (gen-asobj-id-for-user outbox-user)))
            ;; Copy the actor over to attributedTo
            (lambda (object)
              (asobj-cons object "attributedTo"
                          (asobj-ref asobj "actor")))
            ;; Copy the published date
            (lambda (object)
              (asobj-cons object "published"
                          (asobj-ref asobj "published")))))
          
          (object
           ;; saving is also done here, as well as any final modifications
           ;; by the object.
           (create-outbox-object! (tweak-object object)
                                  asobj outbox-user))
          (asobj
           ;; We replace the asobj's object reference with just the
           ;; identifier for the object rather than the object itself
           (asobj-cons asobj "object" (asobj-id object))))
     (save-asobj! asobj)
     asobj)))

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

;;; The following two versions are the least common and least interesting
;;; of all the asobj implementations.

(define-as-method (asobj-outbox-effects! (asobj ^Activity) outbox-user)
  (throw 'effect-error "Don't know how to process activity with this activity type"
         #:asobj asobj
         #:astypes (asobj-types asobj)))


;;; If it's an object and not an activity, we wrap in a Create and run
;;; asobj-outbox-effects! on the wrapped object.
(define-as-method (asobj-outbox-effects! (asobj ^Object) outbox-user)
  ;; @@: 
  (define (copy-props from-asobj to-asobj props)
    (fold (lambda (prop to-asobj)
            (let ((from-val (asobj-ref from-asobj prop %nothing)))
              (if (not (eq? from-val %nothing))
                  (asobj-cons to-asobj prop from-val)
                  to-asobj)))
          to-asobj
          props))
  (define wrapped-asobj
    (copy-props asobj
                (as:create #:id (gen-asobj-id-for-user outbox-user)
                           #:object asobj)
                '("actor" "published" "to" "cc" "bcc" "bto")))
  (asobj-outbox-effects! wrapped-asobj outbox-user))

;;; Follow
(define-as-method (asobj-outbox-effects! (asobj ^Follow)
                                          outbox-user)
  (let-asobj-fields
   asobj ((object "object"))
   (let ((follow-uri (match object
                       ((? string-uri? _)
                        object)
                       ((? asobj? _)
                        (match (asobj-id object)
                          ((? string-uri? uri)
                           uri)
                          (_ (throw 'effect-error
                                    "Follow activity's object has no id"
                                    #:asobj asobj)))))))
     ;; Add to following list
     ;; TODO: Do we need to check if we're already subscribed?
     (user-add-to-followers! (ctx-ref 'store) outbox-user
                             follow-uri)
     ;; Send request to follow
     (post-asobj-to-actor asobj (get-asobj follow-uri))
     
     ;; Save asobj
     (save-asobj! asobj)
     ;; Return asobj
     asobj)))



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
   (let ((asobj (asobj-cons asobj "object" object)))
     ;; We need to break this apart more, but for now...
     ;; TODO: Also do verification that this object really is what it
     ;;   claims to be
     (save-asobj! object)
     (save-asobj! asobj))))

(define-as-method (asobj-inbox-effects! (asobj ^Follow)
                                        inbox-user)
  (let-asobj-fields
   asobj ((object "object")
          (actor "actor"))
   ;; TODO: Again, we need to "audit" this incoming message
   (user-add-to-following! (ctx-ref 'store) inbox-user
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

