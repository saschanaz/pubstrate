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
  #:use-module (srfi srfi-26)
  #:use-module (web request)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate package-config)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate webapp auth)
  #:use-module (pubstrate webapp cookie)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp sessions)
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

            ;; @@: Temporary!
            bearer-token-test

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
            (ctx-ref 'store) user "outbox"
            %items-per-page)))
      (respond-html
       (user-homepage-tmpl user activities
                           #f #f))))
  (define user (store-user-ref (ctx-ref 'store) username))
  (define (user-with-extra-endpoints)
    (asobj-cons user "endpoints"
                `(@ ("getAuthToken" . ,(abs-local-uri "api" "get-auth-token"))
                    ("uploadMedia" . ,(abs-local-uri "api" "upload-media")))))

  (cond
   ;; User not found, so 404
   ((not user)
    (respond-not-found))
   ;; Looks like they want the activitystreams object version..
   ((requesting-asobj? request)
    (respond (asobj->string (user-with-extra-endpoints))
             #:content-type 'application/activity+json))
   ;; Otherwise, give them the human-readable HTML!
   (else (render-user-page))))

(define (user-inbox request body username)
  'TODO)

(define (user-outbox request body username)
  (define store (ctx-ref 'store))
  (define oauth-user
    ;; TODO: Get this from OAUTH!
    'TODO)
  (define outbox-user
    (store-user-ref (ctx-ref 'store) username))
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
      (store-asobj-set! store asobj)
      (user-add-to-outbox! store outbox-user (asobj-id asobj))
      (respond (asobj->string asobj)
               #:status status:created
               #:content-type 'application/activity+json)))
  (define (read-from-outbox oauth-user)
    'TODO)
  (define (user-can-post?)
    ;; TODO!  Right now we just accept it.
    ;;  - Extract the bearer token
    ;;  - See if the bearer token matches anything in the db
    (match (assoc-ref (request-headers request) 'authorization)
      (('bearer . (? string? token))
       (store-bearer-token-valid?
        store token outbox-user))
      (_ #f)))
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
  (define store (ctx-ref 'store))
  (define session-manager
    (ctx-ref 'session-manager))
  (match (request-method request)
    ('GET
     (respond-html (login-tmpl)))
    ('POST
     (let* ((form (decode-urlencoded-form body))
            (username (assoc-ref form "username"))
            (password (assoc-ref form "password"))
            (user (if username
                      (store-user-ref store username)
                      #f)))
       (if (and  user (user-password-matches? user password))
           (respond-redirect (local-uri "")
                             #:extra-headers
                             (list
                              ;; TODO: this should just *add to* the session,
                              ;;   not clobber it...
                              (set-session session-manager
                                           `((user . ,username)))))
           (respond-html (login-tmpl #:try-again #t)))))))

(define (logout request body)
  (respond-redirect (local-uri "")
                    #:extra-headers
                    (list (delete-session (ctx-ref 'session-manager)))))

(define (display-post request body username post-id)
  ;; GET only.
  (let* ((post-url (abs-local-uri "u" username "p" post-id))
         (asobj (store-asobj-ref (ctx-ref 'store) post-url)))
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
  (match (request-method request)
    ('GET
     
     )

    ('POST
     )))

(define (standard-four-oh-four request body)
  ;; TODO: Add 404 status message
  (respond-not-found))

;;; Static site rendering... only available on devel instances (hopefully!)
(define (render-static request body static-path)
  (respond
   (call-with-input-file (web-static-filepath static-path) get-bytevector-all)
   #:content-type (mime-type static-path)))
