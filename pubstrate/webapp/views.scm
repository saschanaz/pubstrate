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
  #:use-module (web request)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate paths)
  #:use-module (pubstrate webapp templates)
  #:use-module (pubstrate webapp utils)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp params)
  #:use-module (pubstrate contrib mime-types)
  #:use-module (rnrs io ports)
  #:export (index mockup
            user-page user-inbox user-outbox
            login logout
            content asobj
            oauth-authorize

            standard-four-oh-four render-static))

(define (index request body)
  (respond-html (index-tmpl)))

(define (mockup request body)
  (respond-html (mockup-tmpl)))

(define (user-page request body username)
  (define (user-tmpl user)
    (base-tmpl
     `(p "Hi!  This is "
         ,(or (asobj-ref user "name")
              (asobj-ref user "preferredUsername"))
         "'s page.")))
  (define (requesting-asobj?)
    (match (request-content-type request)
      (('application/activity+json _ ...)
       #t)
      (_ #f)))
  (let ((user (store-user-ref (%store) username)))
    (cond
     ;; User not found, so 404
     ((not user)
      (respond "User not found!"
               #:status 404))
     ;; Looks like they want the activitystreams object version..
     ((requesting-asobj?)
      (respond (asobj->string user)
               #:content-type 'application/activity+json))
     ;; Otherwise, give them the human-readable HTML!
     (else
      (respond-html
       (user-tmpl user))))))

(define (user-inbox request body username)
  'TODO)

(define (user-outbox request body username)
  (define (post-to-outbox oauth-user)
    'TODO)
  (define (read-from-outbox oauth-user)
    'TODO)
  'TODO)

(define (login request body)
  'TODO)

(define (logout request body)
  'TODO)

(define (content request body user slug-or-id)
  'TODO)

(define (asobj request body asobj-id)
  'TODO)

(define (oauth-authorize request body)
  'TODO)


(define (standard-four-oh-four request body)
  ;; TODO: Add 404 status message
  (values '((content-type . (text/plain)))
          "Not found!"))

;;; Static site rendering... only available on devel instances (hopefully!)
(define (render-static request body static-path)
  (respond
   (call-with-input-file (web-static-filepath static-path) get-bytevector-all)
   #:content-type (mime-type static-path)))
