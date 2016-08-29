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

(define-module (tests test-crypto)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils)
  #:use-module (pubstrate crypto))

(test-begin "test-crypto")

(define test-key (gen-signing-key))

(let ((sig (sign-data test-key "monkey party")))
  ;; Should be a bytevector
  (test-assert (bytevector? sig))
  ;; Correct sig succeeds
  (test-assert (verify-sig test-key "monkey party" sig))
  ;; Incorrect data fails
  (test-assert (not (verify-sig test-key "something else" sig)))
  ;; Fake signature fails
  (test-assert (not (verify-sig test-key "monkey party"
                                (string->utf8 "fake sig"))))
  ;; Should equal a re-run of itself
  (test-equal sig (sign-data test-key "monkey party"))
  ;; Shouldn't equal something different
  (test-assert (not (equal? sig (sign-data test-key "cookie party")))))

;; Now with base64 encoding
(let ((sig (sign-data-base64 test-key "monkey party")))
  ;; Should be a string
  (test-assert (string? sig))
  ;; Correct sig succeeds
  (test-assert (verify-sig-base64 test-key "monkey party" sig))
  ;; Incorrect data fails
  (test-assert (not (verify-sig-base64 test-key "something else" sig)))
  ;; Fake signature fails
  (test-assert (not (verify-sig-base64 test-key "monkey party"
                                       "f41c3516")))
  ;; Should equal a re-run of itself
  (test-equal sig (sign-data-base64 test-key "monkey party"))
  ;; Shouldn't equal something different
  (test-assert (not (equal? sig (sign-data-base64 test-key "cookie party")))))

(test-end "test-crypto")
(test-exit)
