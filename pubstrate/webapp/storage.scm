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
  #:use-module (pubstrate webapp asentry)
  #:export (<simple-storage>
            storage-asentry-set!
            storage-asentry-ref))

;;; Simple in-memory storage
(define-class <memory-store> ()
  (asentry-store #:init-thunk make-hash-table))

(define-generic storage-asentry-set!)
(define-generic storage-asentry-ref)

(define-method (storage-asentry-set! (storage <memory-store>) asentry)
  (let ((id (asentry-id asentry)))
    (if (not id)
        (throw 'asentry-storage-failure
               "Can't save an asentry if no id set on its asobj"))
    (hash-set!
     (slot-ref store 'asentry-store)
     id asentry)))

(define-method (storage-asentry-ref (storage <memory-store>) id)
  (hash-ref (slot-ref storage 'asentry-store) id))

(define (storage-asentry-ref-fat store id)
  'TODO)
(define (storage-asentry-set-lean! store asentry)
  'TODO)
