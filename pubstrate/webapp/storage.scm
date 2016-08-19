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


;;; Storage stuff
;;; =============

(define-module (pubstrate webapp storage)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate webapp auth)
  #:use-module (pubstrate webapp list-pagination)
  #:use-module (pubstrate webapp params)
  #:export (<store>

            <docustore>
            docustore-set! docustore-ref docustore-remove!

            <memory-store>
            make-memory-store
            storage-asobj-ref storage-asobj-set!

            storage-container-new! storage-container-append!
            storage-container-first-page storage-container-page
            storage-container-fetch-all storage-container-member?

            <bearer-entry>
            bearer-entry-token bearer-entry-user-id bearer-entry-expires
            bearer-entry->alist alist->bearer-entry

            storage-bearer-token-new! storage-bearer-token-valid?
            storage-bearer-entry-ref storage-bearer-token-delete!))

(define-class <store> ())

(define-class <docustore> (<store>)
  (asobjs)
  (containers)
  (bearer-entries)
  (serializers #:allocation #:class)
  (deserializers #:allocation #:class))

;;; Simple in-memory storage
(define-class <memory-store> (<docustore>)
  (asobjs #:init-thunk make-hash-table)
  (containers #:init-thunk make-hash-table)
  (bearer-entries #:init-thunk make-hash-table)
  (serializers
   #:allocation #:class
   #:init-value
   (lambda (sym)
     (case sym
       ((asobjs containers bearer-entries)
        identity))))
  (deserializers
   #:allocation #:class
   #:init-value
   (lambda (sym)
     (case sym
       ((asobjs containers bearer-entries)
        identity)))))

(define (make-memory-store)
  (make <memory-store>))

;;; @@: Is it even helpful to have define-generic for these?
;; (define-generic storage-asobj-set!)
;; (define-generic storage-asobj-ref)

(define-method (docustore-set! (store <memory-store>)
                               slot key val)
  "Store and serialize VAL for KEY in STORE's SLOT"
  (let ((serialize ((slot-ref store 'serializers) slot)))
    (hash-set! (slot-ref store slot) key
               (serialize val))))

(define-method (docustore-ref (store <memory-store>)
                              slot key)
  "Retrieve and deserialize value for KEY in STORE's SLOT"
  (let ((deserialize ((slot-ref store 'deserializers) slot)))
    (and=> (hash-ref (slot-ref store slot) key)
           deserialize)))

(define-method (docustore-remove! (store <memory-store>)
                                   slot key)
  "Retrieve a (serialized) value for KEY in STORE's SLOT"
  (hash-remove! (slot-ref store slot) key))


;;; Note that the default storage method assumes that this is a
;;; simple docustore
(define-method (storage-asobj-set! (store <docustore>) asobj)
  (let ((id (asobj-id asobj)))
    (if (not id)
        (throw 'asobj-storage-failure
               "Can't save an asobj if no id set"))
    (docustore-set! store 'asobjs id asobj)))

(define-method (storage-asobj-ref (store <docustore>) id)
  (docustore-ref store 'asobjs id))

(define (storage-asobj-ref-fat store id)
  'TODO)
(define (storage-asobj-set-lean! store asobj)
  'TODO)


;;; Containers
;;; ==========

;; (define-generic storage-container-new!)
;; (define-generic storage-container-append!)

;; @@: The serialize/deserialize docustore stuff seems to only work for our
;;   stupid lists method.  Which means all users of this are using a crappy
;;   O(n) store for containers!  Not good!

;; @@: Probabalistic method, but if this doesn't succeed, something is
;;   wrong with the universe, or your RNG...
;;   Collisions are so unlikely, hopefully this procedure's check is
;;   unnecessary anyway...!
(define-method (storage-container-new! (store <docustore>))
  "Add a new container and return its key"
  (define (keep-trying)
    (let* ((token (gen-bearer-token))
           (existing-container
            (docustore-ref store 'containers token)))
      (if existing-container
          (keep-trying)
          (begin
            (docustore-set! store 'containers token '())
            token))))
  (keep-trying))

(define (get-container-or-error storage key)
  (cond
   ((docustore-ref storage 'containers key) => identity)
   (else (throw 'no-container-for-key
                #:key key))))

(define-method (storage-container-append! (store <docustore>)
                                          container-key val)
  (define current-members
    (get-container-or-error store container-key))
  (docustore-set! store 'containers container-key
                  (cons val current-members)))

(define-method (storage-container-fetch-all (store <docustore>)
                                            container-key)
  (docustore-ref store 'containers container-key))

(define-method (storage-container-page (store <docustore>) container-key
                                       member how-many)
  "Search for MEMBER in STORE's container KEY with a page of HOW-MANY
items, as well as returning information on previous and next pages.

Returns three values to its continuation: a list of items (or #f if
not found) in the range of MEMBER and HOW-MANY, as well as the key for
the prior page (or #f), and the key for the next page (or #f)."
  (list-paginate
   (get-container-or-error store container-key)
   member how-many))

(define-method (storage-container-first-page (store <docustore>)
                                             container-key how-many)
  "Retrieve the first page of in STORE's container KEY with HOW-MANY items.

Returns three values to its continuation: a list of items from the
first item to HOW-MANY, the key for the previous page (which will, in
this case, not exist so will always be #f), and the key for the next
page (or #f)"
  (receive (page prev next)
      (list-paginate-first
            (get-container-or-error store container-key)
            how-many)
    (values (or page '()) prev next)))

(define-method (storage-container-member? (store <docustore>)
                                          container-key item)
  (if (member (get-container-or-error
               store container-key)
              item)
      #t #f))

; (define-method (storage-container-))


;;; Bearer token storage
;;; ====================

;;; Some day we'll have linked data signatures support and can nuke this
;;; stuff.  I hope!

(define-class <bearer-entry> ()
  (token #:init-keyword #:token
         #:init-thunk gen-bearer-token
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

(define-method (storage-bearer-token-new! (store <docustore>) user)
  "Define a new bearer token for USER and place its entry in STORE.
The bearer token key is returned, but the full bearer entry is not."
  (let ((bearer-entry (make <bearer-entry>
                        #:user-id (asobj-id user))))
    (docustore-set! store 'bearer-entries
                    (slot-ref bearer-entry 'token)
                    bearer-entry)
    (slot-ref bearer-entry 'token)))

(define-method (storage-bearer-entry-ref (store <docustore>) token-key)
  (docustore-ref store 'bearer-entries token-key))

;;; This one should work, docustore or not, because it relies on the heavy
;;; lifting of the other methods
(define-method (storage-bearer-token-valid? (store <docustore>)
                                            token-key user)
  "See if the bearer token with TOKEN-KEY is valid in the context of USER"
  (let ((bearer-entry (storage-bearer-entry-ref store token-key)))
    (and (is-a? bearer-entry <bearer-entry>)
         ;; TODO: Check expires field
         (equal? (asobj-id user) (slot-ref bearer-entry 'user-id)))))

(define-method (storage-bearer-token-delete! (store <docustore>) token-key)
  (docustore-remove! store 'bearer-entries token-key))
