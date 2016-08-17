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

(define-module (pubstrate webapp views)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (web request)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate paths)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate webapp auth)
  #:use-module (pubstrate webapp params)
  #:use-module (pubstrate webapp storage)
  #:use-module (pubstrate webapp templates)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp utils)
  #:use-module (pubstrate contrib mime-types)
  #:use-module ((pubstrate webapp http-status)
                #:renamer (symbol-prefix-proc 'status:))
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:export (index mockup
            user-page user-inbox user-outbox
            login logout
            display-post asobj
            oauth-authorize

            standard-four-oh-four render-static))

(define %items-per-page 20)

(define (index request body)
  (respond-html (index-tmpl)))

(define (mockup request body)
  (respond-html (mockup-tmpl)))

(define (user-page request body username)
  (define (render-user-page)
    (let ((activities
           (user-collection-first-page
            (%store) user "outbox"
            %items-per-page)))
      (respond-html
       (user-homepage-tmpl user activities
                           #f #f))))
  (define user (store-user-ref (%store) username))
  (cond
   ;; User not found, so 404
   ((not user)
    (respond-not-found))
   ;; Looks like they want the activitystreams object version..
   ((requesting-asobj? request)
    (respond (asobj->string user)
             #:content-type 'application/activity+json))
   ;; Otherwise, give them the human-readable HTML!
   (else (render-user-page))))

(define (user-inbox request body username)
  'TODO)

(define %debug-body #f)

(define (user-outbox request body username)
  (define oauth-user
    ;; TODO: Get this from OAUTH!
    'TODO)
  (define outbox-user
    (store-user-ref (%store) username))
  (define (post-to-outbox)
    ;; TODO: Handle side effects appropriately.
    ;;   Currently doing a "dumb" version of things where we just dump it
    ;;   into the database.
    (let* ((unique-id
            (abs-local-uri "u" username "p"
                           (gen-bearer-token 30)))
           ;; TODO: Also strip out any @id that may have been attached...
           (asobj
            (asobj-cons
             (string->asobj
              (if (bytevector? body)
                  (utf8->string body)
                  body)
              (%default-env))
             "id" unique-id)))
      (storage-asobj-set! (%store) asobj)
      (user-add-to-outbox! (%store) outbox-user (asobj-id asobj))
      (respond (asobj->string asobj)
               #:status status:created
               #:content-type 'application/activity+json)))
  (define (read-from-outbox oauth-user)
    'TODO)
  (define (user-can-post?)
    ;; TODO!  Right now we just accept it.
    ;;  - Extract the bearer token
    ;;  - See if the bearer token matches anything in the db
    #t)
  (match (request-method request)
    ('GET
     (read-from-outbox oauth-user))
    ('POST
     (if (user-can-post?)
         (post-to-outbox)
         (respond "Sorry, you don't have permission to post that."
                  #:status status:unauthorized
                  #:content-type 'text/plain)))
    (_ (respond #:status status:method-not-allowed))))

(define (login request body)
  'TODO)

(define (logout request body)
  'TODO)

(define (display-post request body username post-id)
  ;; GET only.
  (let* ((post-url (abs-local-uri "u" username "p" post-id))
         (asobj (storage-asobj-ref (%store) post-url)))
    (match (request-method request)
      ('GET
       ;; TODO: authorization check?
       (cond
        ((requesting-asobj? request)
         (respond (asobj->string asobj)
                  #:content-type 'application/activity+json))
        (else
         (if asobj
             (respond-html (asobj-page-tmpl asobj))
             (respond-not-found)))))
      (_ (respond #:status status:method-not-allowed)))))

(define (oauth-authorize request body)
  'TODO)


(define (standard-four-oh-four request body)
  ;; TODO: Add 404 status message
  (respond-not-found))

;;; Static site rendering... only available on devel instances (hopefully!)
(define (render-static request body static-path)
  (respond
   (call-with-input-file (web-static-filepath static-path) get-bytevector-all)
   #:content-type (mime-type static-path)))
