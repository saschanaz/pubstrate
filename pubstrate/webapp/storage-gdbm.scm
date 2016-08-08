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

;;; GDBM based store

(define-module (pubstrate webapp storage-gdbm)
  #:use-module (gdbm)
  #:use-module (oop goops)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate webapp storage)
  #:export (<gdbm-store>
            make-gdbm-store
            gdbm-store-close))

(define-class <gdbm-store> ()
  (asobj-db #:init-keyword #:asobj-db))

(define (make-gdbm-store db-path)
  (make <gdbm-store>
    #:asobj-db
    (gdbm-open db-path GDBM_WRCREAT)))

(define-method (storage-asobj-set! (store <gdbm-store>) asobj)
  (let ((id (asobj-id asobj)))
    (if (not id)
        (throw 'asobj-storage-failure
               "Can't save an asobj if no id set on its asobj"))
    (gdbm-set! (slot-ref store 'asobj-db)
               id (asobj->combined-string asobj))))

(define-method (storage-asobj-ref (store <gdbm-store>) id)
  (let ((db-result (gdbm-ref (slot-ref store 'asobj-db)
                             id)))
    (if db-result
        (combined-string->asobj db-result (%default-env))
        #f)))

(define (gdbm-store-close store)
  (gdbm-close (slot-ref store 'asobj-db)))
