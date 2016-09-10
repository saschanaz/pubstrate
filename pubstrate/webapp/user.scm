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
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-11)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate json-utils)
  #:use-module (pubstrate generics)
  #:use-module (pubstrate webapp auth)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp utils)
  #:export (make-user
            user-id-from-username
            store-add-new-user! store-user-ref

            user-inbox-container-key user-outbox-container-key
            user-inbox-followers-key user-outbox-following-key

            user-add-to-inbox! user-add-to-outbox!
            user-add-to-followers! user-add-to-following!

            user-collection-page user-collection-first-page

            user-password-matches?

            user-name-str))

(define (require-base-uri)
  (if (not (ctx-ref 'base-uri))
      (throw 'base-uri-not-set
             "%base-uri not set")))

(define (assert-valid-username username)
  'TODO)

(define (user-id-from-username username)
  (abs-local-uri "u" username))

(define* (make-user username password
                    #:key (asenv (%default-env))
                    name)
  (define (make-collection col-name)
    (make-as ^OrderedCollection asenv
             #:id (abs-local-uri "u" username col-name)
             #:name (format #f "~a for ~a"
                            (string-capitalize col-name)
                            username)))

  (require-base-uri)
  (assert-valid-username username)
  (let* ((id (user-id-from-username username))
         (password-sjson
          (salted-hash->sjson
           (salt-and-hash-password password)))
         (inbox (make-collection "inbox"))
         (outbox (make-collection "outbox"))
         (following (make-collection "following"))
         (followers (make-collection "followers"))
         (user
          (asobj-set-private
           (make-as ^Person asenv
                    #:id id
                    #:preferredUsername username
                    #:name (or name username)
                    #:inbox (asobj-id inbox)
                    #:outbox (asobj-id outbox)
                    #:following (asobj-id following)
                    #:followers (asobj-id followers))
           `(@ ("password" . ,password-sjson)))))
    (values user
            (list inbox outbox followers following))))

(define* (store-add-new-user! store username password
                              #:key (asenv (%default-env)))
  "Add user with USERNAME to and PASSWORD to STORE

Optionally pass in ASENV, otherwise %default-env is used.

This procedure returns two values to its continuation: the
user object produced, and a list of any other asobj objects added
to the database (in this case, the collections!)"
  ;; Ensure a user with this username doesn't already exist in the
  ;; database
  (if (store-user-ref store username)
      (throw 'user-already-exists
             "Tried adding a user with an already existing username."
             #:username username))

  (receive (user collections)
      (make-user username password #:asenv asenv)
    (let (;; Update collections with containers
          (new-collections
           (map
            (lambda (col)
              (let* ((container-key (store-container-new! store))
                     ;; Update with the private information set
                     ;; to the container key
                     (col (asobj-set-private*
                           col #:container container-key)))
                ;; Store in the database
                (store-asobj-set! store col)
                col))
            collections)))
      ;; Add the user to the store
      (store-asobj-set! store user)

      ;; Return both the user object and the list of collections
      ;; created
      (values user new-collections))))

(define* (store-user-ref store username)
  (store-asobj-ref store (user-id-from-username username)))


(define (store-user-container-key store user collection-name)
  "Get the container key for USER's COLLECTION-NAME"
  (let ((collection
         (store-asobj-ref store (asobj-ref user collection-name))))
    (asobj-private-ref collection "container")))

(define (user-inbox-container-key store user)
  (store-user-container-key store user "inbox"))
(define (user-outbox-container-key store user)
  (store-user-container-key store user "outbox"))
(define (user-followers-container-key store user)
  (store-user-container-key store user "followers"))
(define (user-following-container-key store user)
  (store-user-container-key store user "following"))


(define (store-user-add-to-collection! store user id-to-store
                                       collection-name)
  "Append item of ID-TO-STORE to USER's ourbox in STORE"
  (store-container-append!
   store (store-user-container-key store user collection-name)
   id-to-store))

(define (user-add-to-inbox! store user id-to-store)
  (store-user-add-to-collection! store user id-to-store "inbox"))
(define (user-add-to-outbox! store user id-to-store)
  (store-user-add-to-collection! store user id-to-store "outbox"))
(define (user-add-to-followers! store user id-to-store)
  (store-user-add-to-collection! store user id-to-store "followers"))
(define (user-add-to-following! store user id-to-store)
  (store-user-add-to-collection! store user id-to-store "following"))

(define (user-collection-page store user collection-name
                              member how-many)
  (let*-values (((container-key)
                 (store-user-container-key store user collection-name))
                ((page prev next)
                 (store-container-page store container-key
                                         member how-many)))
    (values (map
             (lambda (id)
               (store-asobj-ref store id))
             page)
            prev next)))

(define (user-collection-first-page store user collection-name
                                    how-many)
  (let*-values (((container-key)
                 (store-user-container-key store user collection-name))
                ((page prev next)
                 (store-container-first-page store container-key how-many)))
    (values (map
             (lambda (id)
               (store-asobj-ref store id))
             page)
            prev next)))

(define (user-password-hash user)
  (sjson->salted-hash (asobj-private-ref user "password")))

(define (user-password-matches? user password)
  "Check if PASSWORD matches that of asobj USER"
  (salted-hash-matches? (user-password-hash user)
                        password))

(define (user-name-str user)
  "Return the best human-readable name we can get for USER"
  (or (asobj-ref user "name")
      (asobj-ref user "preferredUsername")
      (asobj-ref user "id")))
