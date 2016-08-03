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


(define-module (pubstrate webapp cookie)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:use-module (web http))

;;; HTTP Cookie support
;;;
;;; Pretty crude, but should do the job for now.

;; TODO: Not the best implementation.  On known cookie-av pairs
;;   (see rfc6265) we should do proper parsing.
(define (cookie-parser cookie-text)
  (let ((parts (string-split cookie-text #\;)))
    (define (split-cookie-pair cookie-pair)
      (let* ((trimmed (string-trim cookie-pair))
             (delim (string-index trimmed #\=))
             (attrib (if delim
                         (substring trimmed 0 delim)
                         trimmed))
             (val (if delim
                      (substring trimmed (+ delim 1))
                      #t)))
        (cons attrib val)))
    (map split-cookie-pair parts)))

(define (cookie-writer cookie-alist port)
  (let ((cookie-str
         (string-join 
          (map (match-lambda
                 (((? string? attr) . (? string? val))
                  (string-append attr "=" val))
                 (((? string? attr) . #t)
                  attr))
               cookie-alist)
          "; ")))
    (display cookie-str port)))

(define (cookie-validator cookie-alist)
  (match cookie-alist
    ((((? string? key) . (or #t (? string? _))) ...)
     #t)
    (_ #f)))

(define* (cookie-valid? cookie request #:optional (date (current-date)))
  "See whether or not a cookie is valid (applies to domain/path/date)

Not the same as cookie-validator, this isn't about syntax."
  'TODO)

(declare-header! "Set-Cookie"
                 cookie-parser cookie-validator cookie-writer
                 #:multiple? #t)
(declare-header! "Cookie"
                 cookie-parser cookie-validator cookie-writer)
