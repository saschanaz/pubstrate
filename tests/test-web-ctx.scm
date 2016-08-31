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

(define-module (tests test-web-ctx)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64)  ; tests
  #:use-module (pubstrate webapp ctx))

(test-begin "test-web-ctx")

;;; variable references and context extending
(with-extended-ctx
 '((user . "mlinksva")
   (db . a-good-db))
 (lambda ()
   (test-equal (ctx-ref 'user) "mlinksva")
   (test-equal (ctx-ref 'db) 'a-good-db)
   (test-equal (ctx-ref 'nothing) #f)
   (test-equal (ctx-ref 'nothing 'something) 'something)
   ;; And now go deeper!
   (with-extended-ctx
    '((user . "nyergler"))
    (lambda ()
      (test-equal (ctx-ref 'user) "nyergler")))
   ;; still mike out here
   (test-equal (ctx-ref 'user) "mlinksva")))

;;; ctx=> and ctx=>*
(with-extended-ctx
 '((a-thing . "a-val")
   (b-thing . "b-val"))
 (lambda ()
   (test-equal (ctx=> 'a-thing
                      (lambda (thing)
                        `(this-is ,thing)))
     '(this-is "a-val"))
   (test-equal (ctx=> 'no-thing
                      (lambda (thing)
                        `(this-is ,thing)))
     #f)
   (test-equal (ctx=> 'no-thing
                      (lambda (thing)
                        `(this-is ,thing))
                      (lambda ()
                        "Whole lotta nothing"))
     "Whole lotta nothing")
   (test-equal (ctx=>* 'b-thing thing
                       `(I always wanted a ,thing))
     '(I always wanted a "b-val"))))

(test-end "test-web-ctx")
(test-exit)

