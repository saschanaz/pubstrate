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


;;; Db stuff
;;; ========

(define-module (pubstrate webapp db)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (gcrypt random)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate webapp list-pagination)
  #:export (<db>
            db-close

            <docu-db>
            docu-db-set! docu-db-ref docu-db-remove!

            <memory-db>
            make-memory-db
            db-asobj-ref db-asobj-set!

            db-container-new! db-container-append!
            db-container-first-page db-container-page
            db-container-fetch-all db-container-member?
            db-container-remove!

            <bearer-entry>
            bearer-entry-token bearer-entry-user-id bearer-entry-expires
            bearer-entry->alist alist->bearer-entry

            db-bearer-token-new! db-bearer-token-valid?
            db-bearer-entry-ref db-bearer-token-delete!))

(define-class <db> ())

(define-method (db-close (db <db>))
  "Shut down and clean up the db."
  #f)  ; no-op by default

(define-class <docu-db> (<db>)
  (asobjs)
  (containers)
  (bearer-entries)
  (serializers #:allocation #:each-subclass)
  (deserializers #:allocation #:each-subclass))

;;; Simple in-memory db
(define-class <memory-db> (<docu-db>)
  (asobjs #:init-thunk make-hash-table)
  (containers #:init-thunk make-hash-table)
  (bearer-entries #:init-thunk make-hash-table)
  (serializers
   #:allocation #:each-subclass
   #:init-value
   (lambda (sym)
     (case sym
       ((asobjs containers bearer-entries)
        identity))))
  (deserializers
   #:allocation #:each-subclass
   #:init-value
   (lambda (sym)
     (case sym
       ((asobjs containers bearer-entries)
        identity)))))

(define (make-memory-db)
  (make <memory-db>))

;;; @@: Is it even helpful to have define-generic for these?
;; (define-generic db-asobj-set!)
;; (define-generic db-asobj-ref)

(define-method (docu-db-set! (db <memory-db>)
                             slot key val)
  "Db and serialize VAL for KEY in DB's SLOT"
  (let ((serialize ((slot-ref db 'serializers) slot)))
    (hash-set! (slot-ref db slot) key
               (serialize val))))

(define-method (docu-db-ref (db <memory-db>)
                            slot key)
  "Retrieve and deserialize value for KEY in DB's SLOT"
  (let ((deserialize ((slot-ref db 'deserializers) slot)))
    (and=> (hash-ref (slot-ref db slot) key)
           deserialize)))

(define-method (docu-db-remove! (db <memory-db>)
                                slot key)
  "Retrieve a (serialized) value for KEY in DB's SLOT"
  (hash-remove! (slot-ref db slot) key))


;;; Note that the default db method assumes that this is a
;;; simple docu-db
(define-method (db-asobj-set! (db <docu-db>) asobj)
  (let ((id (asobj-id asobj)))
    (if (not id)
        (throw 'asobj-db-failure
               "Can't save an asobj if no id set"))
    (docu-db-set! db 'asobjs id asobj)))

(define-method (db-asobj-ref (db <docu-db>) id)
  (docu-db-ref db 'asobjs id))

(define (db-asobj-ref-fat db id)
  'TODO)
(define (db-asobj-set-lean! db asobj)
  'TODO)


;;; Containers
;;; ==========

;; (define-generic db-container-new!)
;; (define-generic db-container-append!)

;; @@: The serialize/deserialize docu-db stuff seems to only work for our
;;   stupid lists method.  Which means all users of this are using a crappy
;;   O(n) db for containers!  Not good!

;; @@: Probabalistic method, but if this doesn't succeed, something is
;;   wrong with the universe, or your RNG...
;;   Collisions are so unlikely, hopefully this procedure's check is
;;   unnecessary anyway...!
(define-method (db-container-new! (db <docu-db>))
  "Add a new container and return its key"
  (define (keep-trying)
    (let* ((token (random-token))
           (existing-container
            (docu-db-ref db 'containers token)))
      (if existing-container
          (keep-trying)
          (begin
            (docu-db-set! db 'containers token '())
            token))))
  (keep-trying))

(define (get-container-or-error db key)
  (cond
   ((docu-db-ref db 'containers key) => identity)
   (else (throw 'no-container-for-key
                #:key key))))

(define-method (db-container-append! (db <docu-db>)
                                     container-key val)
  (define current-members
    (get-container-or-error db container-key))
  (when (not (member val current-members))
    (docu-db-set! db 'containers container-key
                  (cons val current-members))))

(define-method (db-container-remove! (db <docu-db>)
                                     container-key val)
  (define current-members
    (get-container-or-error db container-key))
  (docu-db-set! db 'containers container-key
                (delete val current-members)))

(define-method (db-container-fetch-all (db <docu-db>)
                                       container-key)
  (docu-db-ref db 'containers container-key))

(define-method (db-container-page (db <docu-db>) container-key
                                  member how-many)
  "Search for MEMBER in DB's container KEY with a page of HOW-MANY
items, as well as returning information on previous and next pages.

Returns three values to its continuation: a list of items (or #f if
not found) in the range of MEMBER and HOW-MANY, as well as the key for
the prior page (or #f), and the key for the next page (or #f)."
  (list-paginate
   (get-container-or-error db container-key)
   member how-many))

(define-method (db-container-first-page (db <docu-db>)
                                        container-key how-many)
  "Retrieve the first page of in DB's container KEY with HOW-MANY items.

Returns three values to its continuation: a list of items from the
first item to HOW-MANY, the key for the previous page (which will, in
this case, not exist so will always be #f), and the key for the next
page (or #f)"
  (receive (page prev next)
      (list-paginate-first
       (get-container-or-error db container-key)
       how-many)
    (values (or page '()) prev next)))

(define-method (db-container-member? (db <docu-db>)
                                     container-key item)
  (if (member item (get-container-or-error
                    db container-key))
      #t #f))


;;; Bearer token storage
;;; ====================

;;; Some day we'll have linked data signatures support and can nuke this
;;; stuff.  I hope!

(define-class <bearer-entry> ()
  (token #:init-keyword #:token
         #:init-thunk random-token
         #:getter bearer-entry-token)
  (user-id #:init-keyword #:user-id
           #:getter bearer-entry-user-id)
  (expires #:init-keyword #:expires
           #:init-value #f
           #:getter bearer-entry-expires))

(define (bearer-entry->alist bearer-entry)
  `(("token" . ,(slot-ref bearer-entry 'token))
    ("user-id" . ,(slot-ref bearer-entry 'user-id))
    ("expires" . ,(slot-ref bearer-entry 'expires))))

(define (alist->bearer-entry alist)
  (make <bearer-entry>
    #:token (assoc-ref alist "token")
    #:user-id (assoc-ref alist "user-id")
    #:expires (assoc-ref alist "expires")))

(define-method (db-bearer-token-new! (db <docu-db>) user)
  "Define a new bearer token for USER and place its entry in DB.
The bearer token key is returned, but the full bearer entry is not."
  (let ((bearer-entry (make <bearer-entry>
                        #:user-id (asobj-id user))))
    (docu-db-set! db 'bearer-entries
                  (slot-ref bearer-entry 'token)
                  bearer-entry)
    (slot-ref bearer-entry 'token)))

(define-method (db-bearer-entry-ref (db <docu-db>) token-key)
  (docu-db-ref db 'bearer-entries token-key))

;;; This one should work, docu-db or not, because it relies on the heavy
;;; lifting of the other methods
(define-method (db-bearer-token-valid? (db <db>)
                                       token-key user)
  "See if the bearer token with TOKEN-KEY is valid in the context of USER"
  (let ((bearer-entry (db-bearer-entry-ref db token-key)))
    (and (is-a? bearer-entry <bearer-entry>)
         ;; TODO: Check expires field
         (equal? (asobj-id user) (slot-ref bearer-entry 'user-id)))))

(define-method (db-bearer-token-delete! (db <docu-db>) token-key)
  (docu-db-remove! db 'bearer-entries token-key))
