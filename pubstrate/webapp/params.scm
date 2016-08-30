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

(define-module (pubstrate webapp params)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-39)
  #:export (%user %store %base-uri %session-manager

            set-params! with-pubstrate-params))


;;; Parameters
;;; ==========

(define %user (make-parameter #f))
(define %store (make-parameter #f))
(define %base-uri (make-parameter #f))
(define %session-manager (make-parameter #f))


;;; Parameter utilities
;;; ===================

(define param-map
  `((store . ,%store)
    (user . ,%user)
    (base-uri . ,%base-uri)
    (session-manager . ,%session-manager)))

(define (get-param sym)
  (assoc-ref param-map sym))

(define (kwlist->alist kwlist)
  (match kwlist
    (((? keyword? kwarg) val rest ...)
     (cons (cons (keyword->symbol kwarg) val)
           (kwlist->alist rest)))
    ('() '())))

(define (set-params! . params)
  "Set a list of keywords and values to their associated parameters."
  (for-each
   (match-lambda
     ((key . val)
      ((get-param key)
       val)))
   (kwlist->alist params)))

;; @@: This function's api sucks and I don't even know if we need/want it
(define (with-pubstrate-params params thunk)
  (define unrolled-alist
    (let lp ((alist (kwlist->alist params))
             (key-list '())
             (value-list '()))
      (match alist
        (((key . val) rest ...)
         (lp rest
             (cons key key-list)
             (cons val value-list)))
        ('()
         (cons key-list value-list)))))
  (define key-list (car unrolled-alist))
  (define param-list
    (map get-param key-list))
  (define value-list (cdr unrolled-alist))
  (with-parameters* param-list value-list thunk))
