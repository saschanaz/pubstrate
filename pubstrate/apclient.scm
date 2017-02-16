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

(define-module (pubstrate apclient)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (rnrs bytevectors)
  #:use-module (web uri)
  #:use-module (oop goops)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:use-module (sjson utils)
  #:export (<apclient>
            apclient-id apclient-auth-token

            make-apclient
            apclient-user apclient-inbox-uri apclient-outbox-uri

            apclient-submit))

;; TODO: Add authentication info, etc...
(define-class <apclient> ()
  (id #:init-keyword #:id
      #:getter apclient-id)
  (auth-token #:init-keyword #:auth-token
              #:getter apclient-auth-token)
  ;; Filled in as needed
  (user #:init-value #f)   ; @@: Why not let the user supply this one?
  (inbox-uri #:init-value #f)
  (outbox-uri #:init-value #f))

(define* (make-apclient id #:key auth-token)
  (let* ((id (match id
               ((? string? id)
                (string->uri id))
               ((? uri? id)
                id)))
         (apclient
          (make <apclient> #:id id)))
    (if auth-token
        (slot-set! apclient 'auth-token auth-token))
    apclient))

(define (apclient-user apclient)
  (define (retrieve-user)
    (receive (response body)
        (http-get (apclient-id apclient)
                  #:headers '((accept . ((application/activity+json)))))
      (make-asobj
       (read-json-from-string
        (if (bytevector? body)
            (utf8->string body)
            body))
       (%default-env))))
  (match (slot-ref apclient 'user)
    (#f
     (let ((user (retrieve-user)))
       (slot-set! apclient 'user user)
       user))
    ;; If it's already set up, return it as-is..
    (user
     user)))

(define (apclient-inbox-uri apclient)
  (let ((user (apclient-user apclient)))
    (and=> (asobj-ref user "inbox")
           string->uri)))

(define (apclient-outbox-uri apclient)
  (let ((user (apclient-user apclient)))
    (and=> (asobj-ref user "outbox")
           string->uri)))

(define (apclient-fetch-asobj apclient asobj-uri)
  ;; TODO: in the future, make sure we pass in the right authentication
  'TODO)

(define (apclient-inbox apclient)
  'TODO)

(define (apclient-submit apclient asobj)
  (define headers
    ;; @@: Maybe this shouldn't be optional
    (let ((auth-token (slot-ref apclient 'auth-token)))
      `((content-type application/activity+json (charset . "utf-8"))
        ,@(if auth-token
              `((authorization bearer . ,auth-token))
              '()))))
  (receive (response body)
      (http-post (apclient-outbox-uri apclient)
                 #:body (asobj->string asobj)
                 #:headers headers)
    (values
     ;; Return an <asobj> built out of the body or #f
     (match (response-code response)
       ((or 200 201)
        (string->asobj (utf8->string body) (%default-env)))
       (_ #f))
     ;; also return the response
     response)))

