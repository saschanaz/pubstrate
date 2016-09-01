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

(define-module (tests store)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils)
  #:use-module (web uri)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate shorthand)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp user))

(test-begin "test-list-store")


;;; Asobj storing tests 
;;; ===================

(let ((test-store (make-memory-store)))
  ;; Insert some stuff
  (store-asobj-set! test-store
                      (note #:id "http://coolsite.example/posts/da-best-note"
                            #:name "Da best note!"
                            #:content "This post is just da best"))

  ;; You shouldn't be able to insert something that lacks an id though
  (test-error
   (store-asobj-set! test-store
                       (note #:name "nope nope nope")))

  ;; Get the asobj, make sure it's pulled out
  (test-equal
      (asobj-ref
       (store-asobj-ref test-store
                          "http://coolsite.example/posts/da-best-note")
       "name")
    "Da best note!")

  ;; This doesn't exist though
  (test-eq (store-asobj-ref test-store
                          "http://coolsite.example/posts/does-not-exist")
    #f))


;;; Container storing tests 
;;; =======================

;;; @@: We currently allow inserting things that are totally unrelated to any
;;;   asobj ids... so notice that we're not bothering to test that here.
(let* ((test-store (make-memory-store))
       (container-key (store-container-new! test-store)))
  ;; Well, the container-key shouldn't be #f
  (test-assert container-key)

  ;; Currently it should have nothing in it
  (test-equal (store-container-fetch-all test-store container-key)
    '())

  ;; But we can add stuff to it..
  (store-container-append! test-store container-key "a")
  (store-container-append! test-store container-key "e")
  (store-container-append! test-store container-key "i")
  (store-container-append! test-store container-key "o")
  (store-container-append! test-store container-key "u")
  (store-container-append! test-store container-key "y")

  (test-equal
      (store-container-fetch-all test-store container-key)
    '("y" "u" "o" "i" "e" "a"))

  (receive (page prev next)
      (store-container-first-page test-store container-key 2)
    (test-equal page '("y" "u"))
    (test-equal prev #f)
    (test-equal next "o"))
  
  (receive (page prev next)
      (store-container-page test-store container-key "o" 2)
    (test-equal page '("o" "i"))
    (test-equal prev "y")
    (test-equal next "e")))


;;; Bearer token tests
;;; ==================

(with-extended-ctx
 `((base-uri . ,(string->uri "https://coolsite.example/")))
 (lambda ()
   (let* ((store (make-memory-store))
          (cwebber (store-add-new-user! store "cwebber" "beep"))
          (rhiaro (store-add-new-user! store "rhiaro" "boop"))
          (token-key (store-bearer-token-new! store cwebber)))
     (test-assert (store-bearer-token-valid? store token-key cwebber))
     (test-assert (not (store-bearer-token-valid? store token-key rhiaro)))
     (store-bearer-token-delete! store token-key)
     (test-assert (not (store-bearer-token-valid? store token-key cwebber))))))

(test-end "test-list-store")

(test-exit)
