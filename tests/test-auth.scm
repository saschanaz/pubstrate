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

(define-module (tests test-auth)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils)
  #:use-module (sjson utils)
  #:use-module (pubstrate webapp auth))

(test-begin "test-auth")

;; Basic hashing checks
(test-assert
    (salted-hash-matches?
     (salt-and-hash-password "monkey")
     "monkey"))
(test-assert
    (not (salted-hash-matches?
          (salt-and-hash-password "monkey")
          "banana")))

(define salted-hash-salt (@@ (pubstrate webapp auth)
                             salted-hash-salt))
(define salted-hash-hash (@@ (pubstrate webapp auth)
                             salted-hash-hash))

;; Shouldn't be the same thing twice because it should be
;; a different hash
(let ((salted-hash1 (salt-and-hash-password "monkey"))
      (salted-hash2 (salt-and-hash-password "monkey")))
  
  ;; not the same when serialized
  (test-assert
      (not (equal? (salted-hash->string salted-hash1)
                   (salted-hash->string salted-hash2))))
  ;; not the same salt
  (test-assert
      (not (equal? (salted-hash-salt salted-hash1)
                   (salted-hash-salt salted-hash2))))
  ;; and certainly not the same hash!
  (test-assert
      (not (equal? (salted-hash-hash salted-hash1)
                   (salted-hash-hash salted-hash2)))))

;; Serializing to sjson and back
(let* ((salted-hash (salt-and-hash-password "fun-times"))
       (sjson-hash (salted-hash->sjson salted-hash))
       (restored-salted-hash (sjson->salted-hash sjson-hash)))
  (test-assert
      (equal? (salted-hash-hash salted-hash)
              (salted-hash-hash restored-salted-hash)))
  (test-assert
      (equal? (salted-hash-salt salted-hash)
              (salted-hash-salt restored-salted-hash))))


(test-end "test-auth")

(test-exit)
