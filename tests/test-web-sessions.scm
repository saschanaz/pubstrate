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

(define-module (tests test-web-sessions)
  #:use-module (tests utils)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64)
  #:use-module (pubstrate crypto)
  #:use-module (pubstrate webapp sessions))

(test-begin "test-web-sessions")

(define-syntax-rule (import-from-sessions name)
  (define name
    (@@ (pubstrate webapp sessions) name)))

;; Pull in some non-exported procedures
(import-from-sessions session-manager-future-expires)
(import-from-sessions still-fresh-by-date-string?)
(import-from-sessions still-fresh-by-date?)
(import-from-sessions split-session-string)

;; Fix the current time for easier testing
(import-from-sessions %current-time)
(define a-time
  (make-time 'time-utc 0 1472514613))  ; "2016-08-29T23:50:13"

(define-syntax-rule (at-fixed-time body1 body2 ...)
  (parameterize ((%current-time (const a-time)))
    body1 body2 ...))

;; This time shouldn't be expired yet
(at-fixed-time
 (test-assert (still-fresh-by-date-string?
               "2016-08-31T14:01:59.977681000-05:00"))  ; the expires-by time
 ;; Neither should the time it currently believes it is
 (test-assert (still-fresh-by-date?
               (session-manager-future-expires
                (make-session-manager (gen-signing-key))))))

;; This should be though
(at-fixed-time
 (test-assert (not (still-fresh-by-date-string?
                    "2016-02-28T14:01:59Z")))) ; that was like, yesterday, man!

;; An invalid http date string will be considered not-fresh
(at-fixed-time
 (test-assert (not (still-fresh-by-date-string? "I'm a date, honest!!!!"))))

;; A date explicitly in 2 days, 1 hour, 30 minutes
(at-fixed-time
 (test-equal (make-date 0 13 20 01 1 9 2016 0)
   (session-manager-future-expires
    (make-session-manager (gen-signing-key)
                          #:expire-delta '(2 1 30)))))

(test-end "test-web-sessions")
(test-exit)

