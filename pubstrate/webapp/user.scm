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

(define-module (pubstrate webapp user)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate generics)
  #:use-module (pubstrate webapp auth)
  #:use-module (pubstrate webapp params)
  #:use-module (pubstrate webapp storage)
  #:use-module (pubstrate webapp utils)
  #:use-module (pubstrate webapp asentry)
  #:export (make-user
            add-new-user-to-store! store-user-ref))

(define (require-base-uri)
  (if (not (%base-uri))
      (throw 'base-uri-not-set
             "%base-uri not set")))

(define (assert-valid-username username)
  'TODO)

(define (user-id-from-username username)
  (abs-local-uri "u" username))

(define* (make-user username password
                    #:key (asenv (%default-env)))
  (define (user-endpoints )
    `(#:inbox ,(abs-local-uri "u" username "inbox")
      #:outbox ,(abs-local-uri "u" username "outbox")))
  (require-base-uri)
  (assert-valid-username username)
  (let* ((id (user-id-from-username username))
         (password-sjson
          (salted-hash->sjson
           (salt-and-hash-password password)))
         (user
          (make-asentry (apply make-as ^Person asenv
                               #:id id
                               #:name username
                               (user-endpoints))
                        `(@ ("password" . ,password-sjson)))))
    user))

(define* (add-new-user-to-store! username password
                                 #:key (asenv (%default-env))
                                 (store (%store)))
  "Add user with USERNAME to and PASSWORD to STORE

Optionally pass in ASENV, otherwise %default-env is used."
  (let ((user (make-user username password #:asenv asenv)))
    (storage-asentry-set! store user)))

(define* (store-user-ref username #:key (store (%store)))
  (storage-asentry-ref store (user-id-from-username username)))

(define (user-password-hash user)
  (sjson->salted-hash (json-alist-assoc "password" (asentry-private user))))

(define (user-password-matches? user password)
  "Check if PASSWORD matches that of asentry USER"
  (salted-hash-matches? (user-password-hash user)
                        password))
