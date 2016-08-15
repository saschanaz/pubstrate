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

(define-module (tests storage)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate shorthand)
  #:use-module (pubstrate webapp storage))

(test-begin "test-list-storage")

(let ((test-store (make-memory-store)))
  ;; Insert some stuff
  (storage-asobj-set! test-store
                      (note #:id "http://coolsite.example/posts/da-best-note"
                            #:name "Da best note!"
                            #:content "This post is just da best"))

  ;; You shouldn't be able to insert something that lacks an id though
  (test-error
   (storage-asobj-set! test-store
                       (note #:name "nope nope nope")))

  ;; Get the asobj, make sure it's pulled out
  (test-equal
      (asobj-ref
       (storage-asobj-ref test-store
                          "http://coolsite.example/posts/da-best-note")
       "name")
    "Da best note!")

  ;; This doesn't exist though
  (test-eq (storage-asobj-ref test-store
                          "http://coolsite.example/posts/does-not-exist")
    #f))

(test-end "test-list-storage")

(test-exit)
