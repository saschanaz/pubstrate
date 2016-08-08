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

(define-module (pubstrate webapp asentry)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate json-utils)
  #:export (make-asentry <asentry>
            asentry?
            asentry-asobj set-asentry-asobj
            asentry-private set-asentry-private
            asentry-id

            asentry->sjson asentry->string string->asentry
            asentry-assoc asentry-ref
            asentry-private-assoc asentry-private-ref))

(define-immutable-record-type <asentry>
  (make-asentry-intern asobj private)
  asentry?
  (asobj asentry-asobj set-asentry-asobj)
  ;; Private data is sjson
  (private asentry-private set-asentry-private))

(define* (make-asentry asobj #:optional (private-data json-alist-nil))
  (make-asentry-intern asobj private-data))

(define (asentry-id asentry)
  "Shortcut to get the id from an asentry's asobj."
  (asobj-id (asentry-asobj asentry)))

(define (asentry->sjson asentry)
  `(@ ("sjson" . ,(asobj-sjson (asentry-asobj asentry)))
      ("private" . ,(asentry-private asentry))))

(define (asentry->string asentry)
  (write-json-to-string (asentry->sjson asentry)))

(define* (string->asentry str #:optional (asenv (%default-env)))
  (let* ((str-sjson (read-json-from-string str))
         (sjson (json-alist-ref str-sjson "sjson"))
         (private (json-alist-ref str-sjson "private")))
    (make-asentry (make-asobj sjson asenv)
                  private)))

(define (asentry-assoc key asentry)
  "Shorthand for asobj-assoc on ASENTRY's asobj"
  (asobj-assoc key (asentry-asobj asentry)))

(define* (asentry-ref asentry key #:optional dflt)
  "Shorthand for asobj-ref on ASENTRY's asobj"
  (asobj-ref (asentry-asobj asentry) key dflt))

(define (asentry-private-assoc key asentry)
  (json-alist-assoc key (asentry-private asentry)))

(define* (asentry-private-ref asentry key #:optional dflt)
  (match (asentry-private-assoc key asentry)
    ((_ . val)
     val)
    (#f dflt)))
