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

;;; Generic methods for asobj types


(define-module (pubstrate generics)
  #:use-module (pubstrate asobj)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (define-as-generic define-as-method))

(define-record-type <as-generic>
  (make-as-generic method-map)
  as-generic?
  (method-map as-generic-method-map))

(define (as-generic-method-ref as-generic astype)
  (hashq-ref (as-generic-method-map as-generic) astype))

(define (as-generic-method-set! as-generic astype proc)
  (hashq-set! (as-generic-method-map as-generic)
              astype proc))

;; TODO: Make docstrings optional.  See mlambda in 8sync!
(define-syntax-rule (define-as-generic generic-name method-name
                      docstring)
  (begin
    (define generic-name
      (make-as-generic (make-hash-table)))

    (define (method-name asobj . args)
      docstring
      (apply (as-generic-find-method generic-name asobj)
             asobj args))))

(define-syntax-rule (define-as-method (generic astype args ...)
                      body ...)
  (as-generic-method-set!
   generic astype
   (lambda (asobj args ...)
     body ...)))

(define (as-generic-find-method as-generic asobj)
  (call/ec
   (lambda (return)
     (for-each
      (lambda (astype)
        (cond ((as-generic-method-ref as-generic astype) =>
               ;; Found it, so return!
               (lambda (proc)
                 (return proc)))
              ;; Nope, keep searching...
              (else #f)))
      (asobj-inherits asobj))

     ;; Well it's obviously not here now...
     (throw 'no-method-found "No method found for asobj's types"
            #:as-generic as-generic
            #:asobj asobj))))
