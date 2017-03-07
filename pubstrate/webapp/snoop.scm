;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

;;; Snoop on arguments passed to procedures applied with snoopply

(define-module (pubstrate webapp snoop)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (snoopply
            with-snooping hashq-one))

(define (null-snooper proc . args)
  (apply proc args))

(define %snooper (make-parameter null-snooper))

(define (snoopply proc . args)
  "Apply PROC with ARGS, but possibly snoop on arguments passed in/out."
  (apply (%snooper) proc args))

(define (make-hash-snooper)
  (define (hashq-append-to-list! table key val)
    (hashq-set! table key (cons val (hashq-ref table key '()))))
  (let* ((args-intel (make-hash-table))
         ;; (values-intel (make-hash-table))
         (snooper (lambda (proc . args)
                    (hashq-append-to-list! args-intel proc args)
                    (apply proc args))))
    (values snooper args-intel)))

(define (with-snooping proc)
  "Call PROC with one argument, a hash table that will contain a mapping
of snoopply-called procedures with the arguments passed into them."
  (receive (snooper args-intel)
      (make-hash-snooper)
    (parameterize ((%snooper snooper))
      (proc args-intel))))

(define %nothing (list '*nothing*))
(define* (hashq-one table key #:key (dflt %nothing))
  "Grab one item from hashq TABLE with lists as values.

It no match found and %dflt is not provided, an error will be thrown."
  (match (hashq-ref table key dflt)
    ;; Looks like we got the default.
    ((? (cut eq? <> dflt) _)
     (if (eq? dflt %nothing)
         ;; but if it's %nothing, that's an error
         (throw 'hashq-one-no-val
                "No value to return for key"
                #:key key)
         ;; otherwise, it's the intended default, return that!
         dflt))
    ;; Great!  It's a list, we can return the first one.
    ((val rest ...) val)))
