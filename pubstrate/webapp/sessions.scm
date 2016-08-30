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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (web request)
  #:use-module (pubstrate date)
  #:use-module (pubstrate crypto)
  #:use-module (pubstrate webapp cookie)
  #:use-module (pubstrate contrib base64)
  #:use-module (rnrs bytevectors)
  #:export (<session-manager>
            make-session-manager session-manager?

            <expire-delta>
            make-expire-delta expire-delta?

            get-session-data set-session delete-session))

(define-record-type <session-manager>
  (make-session-manager-intern key expire-delta reader writer
                               cookie-name algorithm)
  session-manager?
  (key session-manager-key)
  (expire-delta session-manager-expire-delta)
  (reader session-manager-reader)
  (writer session-manager-writer)
  (cookie-name session-manager-cookie-name)
  (algorithm session-manager-algorithm))

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

(define* (date-in-future day hour minute)
  (let* ((secs-delta
          (+ (* 60 60 24 day)
             (* 60 60 hour)
             (* 60 minute)))
         (current-secs (time-second (get-current-time)))
         (future-time (make-time 'time-utc 0 (+ current-secs secs-delta)))
         (future-date (time-utc->date future-time)))
    future-date))

(define* (make-session-manager key #:key
                               ;; expire in 30 days by default
                               (expire-delta
                                (make-expire-delta 30 0 0))
                               (reader read-from-string)
                               (writer write-to-string)
                               (cookie-name "session")
                               (algorithm 'sha512))
  (make-session-manager-intern key expire-delta reader writer
                               cookie-name algorithm))

(define (expire-delta-future-date expire-delta)
  (date-in-future (expire-delta-days expire-delta)
                  (expire-delta-hours expire-delta)
                  (expire-delta-minutes expire-delta)))

(define (session-manager-future-expires session-manager)
  (and=> (session-manager-expire-delta session-manager)
         expire-delta-future-date))

(define (split-session-string session-string)
  "Split the session string into three strings: key, expire date, encoded-data.

Note that the data is still base64 encoded at this point, and will not be
decoded or read until later.

Split on the dollar-sign character.  This is safe because the key is
base64 encoded, and the date uses HTTP style dates, neither of which
should ever contain a dollar-sign."
  (let* ((first-dollar-sign (string-index session-string #\$))
         (second-dollar-sign (and first-dollar-sign
                                (string-index session-string #\$
                                              (+ first-dollar-sign 1)))))
    (if second-dollar-sign  ; no second without the first anyway
        (list
         (substring session-string 0 first-dollar-sign)
         (substring session-string (+ first-dollar-sign 1) second-dollar-sign)
         (substring session-string (+ second-dollar-sign 1)))
        #f)))

(define (date-still-fresh? expires-date)
  "Make sure that we haven't yet passed the expiration date"
  (time<=? (get-current-time)
           (date->time-utc expires-date)))

(define (date-string-still-fresh? expires-date-string)
  "Parse date string, if valid at all, and see if it's still within
the expiration time"
  (and=> (rfc3339-string->date expires-date-string)
         date-still-fresh?))

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
  (define session-str
    (and=> (assoc-ref (request-headers request) 'cookie)
           (cut assoc-ref <> (session-manager-cookie-name session-manager))))
  (define (decode-data data)
    (utf8->string (base64-decode data)))
  (match (and=> session-str split-session-string)
    ;; If it's false, we return false
    (#f #f)
    ((sig expires-str (= decode-data data))
     (cond
      ;; Return false if the date string is invalid
      ((not (date-string-still-fresh? expires-str))
       #f)
      ;; Otherwise, check signature against data + data
      (else
       (let* ((date-and-data (string-append expires-str "$" data))
              (valid-sig (verify-sig-base64
                          (session-manager-key session-manager)
                          (string->utf8 date-and-data) sig
                          #:algorithm (session-manager-algorithm
                                       session-manager))))
         (if valid-sig
             ((session-manager-reader session-manager) data)
             #f)))))))

(define (set-session session-manager obj)
  "Produce an HTTP cookie header containing signed OBJ, using SESSION-MANAGER."
  ;; Sign the date + data, joined with a dollar-sign, as a bytevector.
  (let* ((expires-date (session-manager-future-expires session-manager))
         (expires-str (date->rfc3339-string expires-date))
         (written-data ((session-manager-writer session-manager)
                        obj))
         (date-and-data (string-append expires-str "$" written-data))
         (sig (sign-data-base64 (session-manager-key session-manager)
                                date-and-data
                                #:algorithm (session-manager-algorithm
                                             session-manager)))
         (signed-string
          (string-append sig "$" expires-str "$"
                         (base64-encode (string->utf8 written-data)))))
    (set-cookie* (session-manager-cookie-name session-manager)
                 signed-string)))

(define (delete-session session-manager)
  "Produce an HTTP header deleting the session cookie entirely.
A great way to log users out!"
  'TODO)
