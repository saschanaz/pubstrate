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

(define-module (tests test-web-asentry)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (tests utils)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate webapp asentry)
  #:use-module ((pubstrate shorthand)
                #:renamer (symbol-prefix-proc 'as:)))

(test-begin "test-asentry")

(define create-foo-asentry
  (make-asentry
   ;; Nonsense keys, but it doesn't matter.
   (as:create #:foo #f #:bar "baz")
   '(@ ("private" . "info"))))

(test-equal (asentry-ref create-foo-asentry "foo")
  #f)
(test-equal (asentry-ref create-foo-asentry "foo" 'default)
  #f)
(test-equal (asentry-ref create-foo-asentry "bar" 'default)
  "baz")
(test-equal (asentry-ref create-foo-asentry "basil" 'default)
  'default)

(test-equal (asentry-private-ref create-foo-asentry "private" 'default)
  "info")
(test-equal (asentry-private-ref create-foo-asentry "not-here" 'default)
  'default)
(test-equal (asentry-private-assoc "private" create-foo-asentry)
  '("private" . "info"))
(test-equal (asentry-private-assoc "not-here" create-foo-asentry)
  #f)

;; Make sure we can serialize to string and get it back
(let ((restored-asentry
       (string->asentry
        (asentry->string create-foo-asentry))))
  (test-equal (asentry-ref restored-asentry "bar")
    "baz")
  (test-equal (asentry-private-ref create-foo-asentry "private")
    "info"))

(test-end "test-asentry")
(test-exit)
