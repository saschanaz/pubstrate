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
  #:use-module (oop goops)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate webapp params)
  #:export (<memory-store>
            make-memory-store
            storage-asobj-set!
            storage-asobj-ref))

;;; Simple in-memory storage
(define-class <memory-store> ()
  (asobj-store #:init-thunk make-hash-table))

(define (make-memory-store)
  (make <memory-store>))

(define-generic storage-asobj-set!)
(define-generic storage-asobj-ref)

(define-method (storage-asobj-set! (store <memory-store>) asobj)
  (let ((id (asobj-id asobj)))
    (if (not id)
        (throw 'asobj-storage-failure
               "Can't save an asobj if no id set"))
    (hash-set!
     (slot-ref store 'asobj-store)
     id asobj)))

(define-method (storage-asobj-ref (store <memory-store>) id)
  (hash-ref (slot-ref store 'asobj-store) id))

(define (storage-asobj-ref-fat store id)
  'TODO)
(define (storage-asobj-set-lean! store asobj)
  'TODO)
