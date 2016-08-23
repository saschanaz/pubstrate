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

(define-module (tests test-date)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils)
  #:use-module (srfi srfi-19)
  #:use-module (pubstrate date))

(test-begin "test-date-rfc3339")

(let ((date (rfc3339-string->date "1985-04-12T23:20:50.52Z")))
  (test-eqv (date-year date) 1985)
  (test-eqv (date-month date) 04)
  (test-eqv (date-day date) 12)
  (test-eqv (date-hour date) 23)
  (test-eqv (date-minute date) 20)
  (test-eqv (date-second date) 50)
  (test-eqv (date-nanosecond date) 52)
  (test-eqv (date-zone-offset date) 0))

;; now without any second or microsecond
(let ((date (rfc3339-string->date "1985-04-12T23:20Z")))
  (test-eqv (date-year date) 1985)
  (test-eqv (date-month date) 04)
  (test-eqv (date-day date) 12)
  (test-eqv (date-hour date) 23)
  (test-eqv (date-minute date) 20)
  (test-eqv (date-second date) 0)
  (test-eqv (date-nanosecond date) 0)
  (test-eqv (date-zone-offset date) 0))

;; now with time zone offset
(let ((date (rfc3339-string->date "1996-12-19T16:39:57-08:30")))
  (test-eqv (date-year date) 1996)
  (test-eqv (date-month date) 12)
  (test-eqv (date-day date) 19)
  (test-eqv (date-hour date) 16)
  (test-eqv (date-minute date) 39)
  (test-eqv (date-second date) 57)
  (test-eqv (date-nanosecond date) 0)
  (test-eqv (date-zone-offset date) -30600))

;; Here and back again
(test-equal (date->rfc3339-string (rfc3339-string->date "1996-12-19T16:39:57-08:30"))
  "1996-12-19T16:39:57-08:30")


;;; Http dates

;; Convert to <date>
(let ((date (http-date-string->date "Sunday, 06-Nov-94 08:49:37 GMT")))
  (test-eqv (date-year date) 1994)
  (test-eqv (date-month date) 11)
  (test-eqv (date-day date) 6)
  (test-eqv (date-hour date) 8)
  (test-eqv (date-minute date) 49)
  (test-eqv (date-second date) 37)
  (test-eqv (date-nanosecond date) 0)
  (test-eqv (date-zone-offset date) 0))

;; Convert to HTTP date string

(test-equal (date->http-date-string (http-date-string->date
                                     "Sunday, 06-Nov-94 08:49:37 GMT"))
  "Sun, 06 Nov 1994 08:49:37 GMT")

(test-end "test-date-rfc3339")
(test-exit)
