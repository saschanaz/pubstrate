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

(define-module (tests db)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils)
  #:use-module (web uri)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate shorthand)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp db)
  #:use-module (pubstrate webapp user))

(test-begin "test-list-db")


;;; Asobj storing tests 
;;; ===================

(let ((test-db (make-memory-db)))
  ;; Insert some stuff
  (db-asobj-set! test-db
                 (note #:id "http://coolsite.example/posts/da-best-note"
                       #:name "Da best note!"
                       #:content "This post is just da best"))

  ;; You shouldn't be able to insert something that lacks an id though
  (test-error
   (db-asobj-set! test-db
                  (note #:name "nope nope nope")))

  ;; Get the asobj, make sure it's pulled out
  (test-equal
      (asobj-ref
       (db-asobj-ref test-db
                     "http://coolsite.example/posts/da-best-note")
       "name")
    "Da best note!")

  ;; This doesn't exist though
  (test-eq (db-asobj-ref test-db
                         "http://coolsite.example/posts/does-not-exist")
    #f))


;;; Container storing tests 
;;; =======================

;;; @@: We currently allow inserting things that are totally unrelated to any
;;;   asobj ids... so notice that we're not bothering to test that here.
(let* ((test-db (make-memory-db))
       (container-key (db-container-new! test-db)))
  ;; Well, the container-key shouldn't be #f
  (test-assert container-key)

  ;; Currently it should have nothing in it
  (test-equal (db-container-fetch-all test-db container-key)
    '())

  ;; But we can add stuff to it..
  (db-container-append! test-db container-key "a")
  (db-container-append! test-db container-key "e")
  (db-container-append! test-db container-key "i")
  (db-container-append! test-db container-key "o")
  (db-container-append! test-db container-key "u")
  (db-container-append! test-db container-key "y")

  (test-equal
      (db-container-fetch-all test-db container-key)
    '("y" "u" "o" "i" "e" "a"))

  (receive (page prev next)
      (db-container-first-page test-db container-key 2)
    (test-equal page '("y" "u"))
    (test-equal prev #f)
    (test-equal next "o"))
  
  (receive (page prev next)
      (db-container-page test-db container-key "o" 2)
    (test-equal page '("o" "i"))
    (test-equal prev "y")
    (test-equal next "e")))


;;; Bearer token tests
;;; ==================

(with-extended-ctx
 `((base-uri . ,(string->uri "https://coolsite.example/")))
 (lambda ()
   (let* ((db (make-memory-db))
          (cwebber (db-add-new-user! db "cwebber" "beep"))
          (rhiaro (db-add-new-user! db "rhiaro" "boop"))
          (token-key (db-bearer-token-new! db cwebber)))
     (test-assert (db-bearer-token-valid? db token-key cwebber))
     (test-assert (not (db-bearer-token-valid? db token-key rhiaro)))
     (db-bearer-token-delete! db token-key)
     (test-assert (not (db-bearer-token-valid? db token-key cwebber))))))

(test-end "test-list-db")

(test-exit)
