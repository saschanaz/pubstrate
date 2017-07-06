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
  #:use-module (pubstrate generics)
  #:use-module (pubstrate webapp auth)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp db)
  #:use-module (pubstrate webapp utils)
  #:export (make-user
            user-id-from-username
            db-add-new-user! db-user-ref
            db-user-change-password!

            user-inbox-container-key user-outbox-container-key
            user-followers-container-key user-following-container-key

            user-add-to-inbox! user-add-to-outbox!
            user-add-to-followers! user-add-to-following!

            db-user-container-key
            db-user-collection-member?
            user-inbox-member? user-outbox-member?
            user-followers-member? user-following-member?

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
  (require-base-uri)
  (assert-valid-username username)
  (let* ((id (user-id-from-username username))
         (password-sjson
          (salted-hash->sjson
           (salt-and-hash-password password)))
         (make-collection (lambda (col-name)
                            (make-as ^OrderedCollection asenv
                                     #:id (abs-local-uri "u" username col-name)
                                     #:name (format #f "~a for ~a"
                                                    (string-capitalize col-name)
                                                    username)
                                     #:attributedTo id)))
         (inbox (make-collection "inbox"))
         (outbox (make-collection "outbox"))
         (following (make-collection "following"))
         (followers (make-collection "followers"))
         (liked (make-collection "liked"))
         (user
          (asobj-set-private
           (make-as ^Person asenv
                    #:id id
                    #:preferredUsername username
                    #:name (or name username)
                    #:inbox (asobj-id inbox)
                    #:outbox (asobj-id outbox)
                    #:following (asobj-id following)
                    #:followers (asobj-id followers)
                    #:liked (asobj-id liked))
           `(@ ("password" ,password-sjson)))))
    (values user
            (list inbox outbox followers following liked))))

(define* (db-add-new-user! db username password
                           #:key (asenv (%default-env)))
  "Add user with USERNAME to and PASSWORD to DB

Optionally pass in ASENV, otherwise %default-env is used.

This procedure returns two values to its continuation: the
user object produced, and a list of any other asobj objects added
to the database (in this case, the collections!)"
  ;; Ensure a user with this username doesn't already exist in the
  ;; database
  (if (db-user-ref db username)
      (throw 'user-already-exists
             "Tried adding a user with an already existing username."
             #:username username))

  (receive (user collections)
      (make-user username password #:asenv asenv)
    (let (;; Update collections with containers
          (new-collections
           (map
            (lambda (col)
              (let* ((container-key (db-container-new! db))
                     ;; Update with the private information set
                     ;; to the container key
                     (col (asobj-set-private*
                           col #:container container-key)))
                ;; Db in the database
                (db-asobj-set! db col)
                col))
            collections)))
      ;; Add the user to the db
      (db-asobj-set! db user)

      ;; Return both the user object and the list of collections
      ;; created
      (values user new-collections))))

(define* (db-user-ref db username)
  (db-asobj-ref db (user-id-from-username username)))

(define (db-user-change-password! db username new-password)
  (let* ((password-sjson
          (salted-hash->sjson
           (salt-and-hash-password new-password)))
         (user (asobj-private-set (db-user-ref db username)
                                  "password"
                                  password-sjson)))
    (db-asobj-set! db user)))

(define (db-user-container-key db user collection-name)
  "Get the container key for USER's COLLECTION-NAME"
  (let ((collection
         (db-asobj-ref db (asobj-ref user collection-name))))
    (asobj-private-ref collection "container")))

(define (user-inbox-container-key db user)
  (db-user-container-key db user "inbox"))
(define (user-outbox-container-key db user)
  (db-user-container-key db user "outbox"))
(define (user-followers-container-key db user)
  (db-user-container-key db user "followers"))
(define (user-following-container-key db user)
  (db-user-container-key db user "following"))


(define (db-user-add-to-collection! db user id
                                    collection-name)
  "Append item of ID to USER's ourbox in DB"
  (db-container-append!
   db (db-user-container-key db user collection-name)
   id))

(define (user-add-to-inbox! db user id)
  (db-user-add-to-collection! db user id "inbox"))
(define (user-add-to-outbox! db user id)
  (db-user-add-to-collection! db user id "outbox"))
(define (user-add-to-followers! db user id)
  (db-user-add-to-collection! db user id "followers"))
(define (user-add-to-following! db user id)
  (db-user-add-to-collection! db user id "following"))

(define (db-user-collection-member? db user id
                                    collection-name)
  (db-container-member?
   db (db-user-container-key db user collection-name)
   id))

(define (user-inbox-member? db user id)
  (db-user-collection-member? db user id "inbox"))
(define (user-outbox-member? db user id)
  (db-user-collection-member? db user id "outbox"))
(define (user-followers-member? db user id)
  (db-user-collection-member? db user id "followers"))
(define (user-following-member? db user id)
  (db-user-collection-member? db user id "following"))

(define (user-collection-page db user collection-name
                              member how-many)
  (let*-values (((container-key)
                 (db-user-container-key db user collection-name))
                ((page prev next)
                 (db-container-page db container-key
                                         member how-many)))
    (values (map
             (lambda (id)
               (db-asobj-ref db id))
             page)
            prev next)))

(define (user-collection-first-page db user collection-name
                                    how-many)
  (let*-values (((container-key)
                 (db-user-container-key db user collection-name))
                ((page prev next)
                 (db-container-first-page db container-key how-many)))
    (values (map
             (lambda (id)
               (or (db-asobj-ref db id)
                   ;; if we can't find it in the db, just return the
                   ;; id as a string... (though this can cause issues...?)
                   id))
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
