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

(define-module (pubstrate webapp federation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (logging logger)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (sjson utils)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp utils)
  #:use-module (pubstrate webapp side-effects)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (pubstrate vocab)
  #:export (collect-recipients
            deliver-asobj
            actor-post-asobj-to-inbox!
            get-asobj post-asobj-to-actor))


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
             (asobj-cons (http-get-asobj id)
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
