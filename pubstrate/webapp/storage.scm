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
  #:export (<memory-store>
            make-memory-store
            storage-asobj-ref storage-asobj-set!

            storage-container-new! storage-container-append!
            storage-container-first-page storage-container-page
            storage-container-fetch-all storage-container-member?

            storage-bearer-token-new! storage-bearer-token-valid?
            storage-bearer-token-ref storage-bearer-token-delete!))

;;; Simple in-memory storage
(define-class <memory-store> ()
  (asobjs #:init-thunk make-hash-table)
  (containers #:init-thunk make-hash-table)
  (bearer-tokens #:init-thunk make-hash-table))

(define (make-memory-store)
  (make <memory-store>))

;;; @@: Is it even helpful to have define-generic for these?
;; (define-generic storage-asobj-set!)
;; (define-generic storage-asobj-ref)

(define-method (storage-asobj-set! (store <memory-store>) asobj)
  (let ((id (asobj-id asobj)))
    (if (not id)
        (throw 'asobj-storage-failure
               "Can't save an asobj if no id set"))
    (hash-set!
     (slot-ref store 'asobjs)
     id asobj)))

(define-method (storage-asobj-ref (store <memory-store>) id)
  (hash-ref (slot-ref store 'asobjs) id))

(define (storage-asobj-ref-fat store id)
  'TODO)
(define (storage-asobj-set-lean! store asobj)
  'TODO)


;;; Containers
;;; ==========

;; (define-generic storage-container-new!)
;; (define-generic storage-container-append!)

;; @@: Probabalistic method, but if this doesn't succeed, something is
;;   wrong with the universe, or your RNG...
;;   Collisions are so unlikely, hopefully this procedure's check is
;;   unnecessary anyway...!
(define-method (storage-container-new! (store <memory-store>))
  "Add a new container and return its key"
  (define containers (slot-ref store 'containers))
  (define (keep-trying)
    (let ((token (gen-bearer-token)))
      (if (hash-ref containers token)
          (keep-trying)
          (begin
            (hash-set! containers token '())
            token))))
  (keep-trying))

(define (get-container-or-error containers key)
  (cond
   ((hash-ref containers key) => identity)
   (else (throw 'no-container-for-key
                #:key key))))

(define-method (storage-container-append! (store <memory-store>)
                                          container-key val)
  (define containers (slot-ref store 'containers))
  (define current-members
    (get-container-or-error containers container-key))

  (hash-set! containers container-key
             (cons val current-members)))

(define-method (storage-container-fetch-all (store <memory-store>)
                                            container-key)
  (define containers (slot-ref store 'containers))
  (get-container-or-error containers container-key))

(define-method (storage-container-page (store <memory-store>) container-key
                                       member how-many)
  "Search for MEMBER in STORE's container KEY with a page of HOW-MANY
items, as well as returning information on previous and next pages.

Returns three values to its continuation: a list of items (or #f if
not found) in the range of MEMBER and HOW-MANY, as well as the key for
the prior page (or #f), and the key for the next page (or #f)."
  (define containers (slot-ref store 'containers))
  (list-paginate
   (get-container-or-error containers container-key)
   member how-many))

(define-method (storage-container-first-page (store <memory-store>)
                                             container-key how-many)
  "Retrieve the first page of in STORE's container KEY with HOW-MANY items.

Returns three values to its continuation: a list of items from the
first item to HOW-MANY, the key for the previous page (which will, in
this case, not exist so will always be #f), and the key for the next
page (or #f)"
  (define containers (slot-ref store 'containers))
  (receive (page prev next)
      (list-paginate-first
            (get-container-or-error containers container-key)
            how-many)
    (values (or page '()) prev next)))

(define-method (storage-container-member? (store <memory-store>)
                                          container-key item)
  (if (member (get-container-or-error
               (slot-ref store 'containers)
               container-key)
              item)
      #t #f))

; (define-method (storage-container-))


;;; Bearer token storage
;;; ====================

;;; Some day we'll have linked data signatures support and can nuke this
;;; stuff.  I hope!

(define-class <bearer-token> ()
  (token #:init-keyword #:token
         #:init-thunk gen-bearer-token
         #:getter bearer-token-token)
  (user-id #:init-keyword #:user-id
           #:getter bearer-token-user-id)
  (expires #:init-keyword #:expires
           #:init-value #f
           #:getter bearer-token-expires))

(define (bearer-token->alist bearer-token)
  `(("token" . ,(slot-ref bearer-token 'token))
    ("user-id" . ,(slot-ref bearer-token 'user-id))
    ("expires" . ,(slot-ref bearer-token 'expires))))

(define (alist->bearer-token alist)
  (make <bearer-token>
    #:token (assoc-ref alist "token")
    #:user-id (assoc-ref alist "user-id")
    #:expires (assoc-ref alist "expires")))

;;; Bearer tokens
(define-method (storage-bearer-token-new! (store <memory-store>) user)
  (let ((bearer-token (make <bearer-token>
                        #:user-id (asobj-id user))))
    (hash-set! (slot-ref store 'bearer-tokens)
               (slot-ref bearer-token 'token)
               bearer-token)
    (slot-ref bearer-token 'token)))

(define-method (storage-bearer-token-ref (store <memory-store>) token-key)
  (hash-ref (slot-ref store 'bearer-tokens)
            token-key))

(define-method (storage-bearer-token-valid? (store <memory-store>)
                                            token-key user)
  "See if the bearer token with TOKEN-KEY is valid in the context of USER"
  (let ((bearer-token (storage-bearer-token-ref store token-key)))
    (and (is-a? bearer-token <bearer-token>)
         ;; TODO: Check expires field
         (equal? (asobj-id user) (slot-ref bearer-token 'user-id)))))

(define-method (storage-bearer-token-delete! (store <memory-store>) token-key)
  (hash-remove! (slot-ref store 'bearer-tokens) token-key))
