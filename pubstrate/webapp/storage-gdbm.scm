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
  #:use-module (pubstrate webapp asentry)
  #:use-module (pubstrate webapp storage)
  #:export (<gdbm-store>
            make-gdbm-store
            gdbm-store-close))

(define-class <gdbm-store> ()
  (asentry-db #:init-keyword #:asentry-db))

(define (make-gdbm-store db-path)
  (make <gdbm-store>
    #:asentry-db
    (gdbm-open db-path GDBM_WRCREAT)))

(define-method (storage-asentry-set! (store <gdbm-store>) asentry)
  (let ((id (asentry-id asentry)))
    (if (not id)
        (throw 'asentry-storage-failure
               "Can't save an asentry if no id set on its asobj"))
    (gdbm-set! (slot-ref store 'asentry-db)
               id (asentry->string asentry))))

(define-method (storage-asentry-ref (store <gdbm-store>) id)
  (string->asentry
   (gdbm-ref (slot-ref store 'asentry-db)
             id)))

(define (gdbm-store-close store)
  (gdbm-close (slot-ref store 'asentry-db)))
