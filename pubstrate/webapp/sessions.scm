;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; Also pulls some code from Guix, which has the following copyrights:
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (pubstrate webapp sessions)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (pubstrate date)
  #:use-module (pubstrate crypto)
  #:use-module (pubstrate webapp cookie)
  #:use-module (rnrs bytevectors)
  #:export (<session-manager>
            make-session-manager session-manager?

            <expire-delta>
            make-expire-delta expire-delta?

            get-session-data set-session delete-session))

(define-record-type <session-manager>
  (make-session-manager-intern key expire-delta reader writer
                               cookie-name)
  session-manager?
  (key session-manager-key)
  (expire-delta session-manager-expire-delta)
  (reader session-manager-reader)
  (writer session-manager-writer)
  (cookie-name session-manager-cookie-name))

;; Intentionally opaque so as to hide the key
(set-record-type-printer! <session-manager>
  (lambda (record port)
    (display "#<session-manager>")))

(define (read-from-string str)
  (call-with-input-string str
    (lambda (port)
      (read port))))

(define (write-to-string obj)
  (call-with-output-string
    (lambda (port)
      (write obj port))))

(define-record-type <expire-delta>
  (make-expire-delta days hours minutes)
  expire-delta?
  (days expire-delta-days)
  (hours expire-delta-hours)
  (minutes expire-delta-minutes))

;; Defined as parameters for testing
(define %current-time
  (make-parameter current-time))
(define (get-current-time)
  ((%current-time)))

(define* (http-date-in-future day hour minute)
  (let* ((secs-delta
          (+ (* 60 60 24 day)
             (* 60 60 hour)
             (* 60 minute)))
         (current-secs (time-second (get-current-time)))
         (future-time (make-time 'time-utc 0 (+ current-secs secs-delta)))
         (future-date (time-utc->date future-time)))
    (date->http-date-string future-date)))

(define* (make-session-manager key #:key
                               ;; expire in 30 days by default
                               (expire-delta
                                (make-expire-delta 30 0 0))
                               (reader read-from-string)
                               (writer write-to-string)
                               (cookie-name "session"))
  (make-session-manager-intern key expire-delta reader writer
                               cookie-name))

(define (expire-delta-future-date expire-delta)
  (http-date-in-future (expire-delta-days expire-delta)
                       (expire-delta-hours expire-delta)
                       (expire-delta-minutes expire-delta)))

(define (session-manager-future-expires session-manager)
  (and=> (session-manager-expire-delta session-manager)
         expire-delta-future-date))

(define (split-session-string session-string)
  "Split the session string into three strings: key, expire date, data

Split on the semicolon character.  This is safe because the key is
base64 encoded, and the date uses HTTP style dates, neither of which
should ever contain a semicolon."
  (let* ((first-semicolon (string-index session-string #\;))
         (second-semicolon (and first-semicolon
                                (string-index session-string #\;
                                              (+ first-semicolon 1)))))
    (if second-semicolon  ; no second without the first anyway
        (list
         (substring session-string 0 first-semicolon)
         (substring session-string (+ first-semicolon 1) second-semicolon)
         (substring session-string (+ second-semicolon 1)))
        #f)))

(define (expired-yet? date-string)
  (time<=? (date->time-utc (http-date-string->date date-string))
           (get-current-time)))

(define (get-session-data session-manager request)
  "Extract session data from REQUEST via SESSION-MANAGER, assuming it
contains valid session data in its header."
  ;; What's a valid session cookie?
  ;;  - First we check whether the cookie's expired... there's no sense
  ;;    checking the signature if it is.
  ;;  - Next, we check the signature against the date + data (as a
  ;;    combined utf8 bytevector).
  ;;  - If that's okay, then we return the read data using the
  ;;    session-manager's reader method.
  'TODO)

(define (set-session session-manager obj)
  "Produce an HTTP cookie header containing signed OBJ, using SESSION-MANAGER."
  ;; Sign the date + data, joined with a semicolon, as a bytevector.
  ;(let* ((date (http-date-in-future))))

  'TODO)

(define (delete-session session-manager)
  "Produce an HTTP header deleting the session cookie entirely.
A great way to log users out!"
  'TODO)
