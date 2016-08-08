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

(define-module (tests test-asobj)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate generics)
  #:use-module ((pubstrate shorthand)
                #:renamer (symbol-prefix-proc 'as:)))

(test-begin "test-generics")

(define-as-generic generic-foo "foo the bar")
(define-as-method (generic-foo (asobj ^Object) x)
  (list 'object-time asobj x))
(define-as-method (generic-foo (asobj ^Activity) x)
  (list 'activity-time asobj x))

(let ((test-object (as:object)))
  (test-equal (generic-foo test-object 'yup)
    (list 'object-time test-object 'yup)))
(let ((test-object (as:like)))
  (test-equal (generic-foo test-object 'yup)
    (list 'activity-time test-object 'yup)))

(test-end "test-generics")

(test-exit)
