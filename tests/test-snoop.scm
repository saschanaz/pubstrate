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

(define-module (tests test-snoop)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils)
  #:use-module (pubstrate webapp snoop))

(test-begin "test-snoop")

(define (foo bar baz)
  (list 'bar bar 'baz baz))

(define (bluh)
  (snoopply foo 1 2))

;; Things should still work with snooping turned off...
(test-equal (bluh)
  (list 'bar 1 'baz 2))

;; But with snooping turned on...
(with-snooping
 (lambda (args-report)
   ;; The result should still return
   (test-equal (bluh)
     (list 'bar 1 'baz 2))
   ;; But we should be able to see what it was called with
   (test-equal (hashq-one args-report foo)
     '(1 2))
   ;; in this case we only called it once
   (test-equal (length (hashq-ref args-report foo)) 1)
   ;; but if we call it again, it should record that one's values too
   (bluh)
   (test-equal (length (hashq-ref args-report foo)) 2)
   ;; Of course, if we didn't call something with snooply, which
   ;; bluh itself isn't called that way, it shouldn't have a value
   (test-equal (hashq-one args-report bluh #:dflt 'whole-lotta-nothin)
     'whole-lotta-nothin)))

(test-end "test-snoop")
(test-exit)
