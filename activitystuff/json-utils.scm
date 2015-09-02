;; Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA


(define-module (activitystuff json-utils)
  #:use-module (activitystuff contrib json)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (json-alist?
            json-assoc json-ref json-acons
            read-json-from-string write-json-to-string
            vhash-ref
            sjson->vjson vjson->sjson))

;; JSON helper procedures

(define (json-alist? json-scm)
  (and (pair? json-scm)
       (eq? (car json-scm) '@)))

(define (json-assoc key json-alist)
  (assoc key (cdr json-alist)))

(define (json-ref json-alist key)
  "Like assoc-ref for a json-alist"
  (assoc-ref (cdr json-alist) key))

(define (json-acons key value json-alist)
  (cons '@ (acons key value (cdr json-alist))))

(define (read-json-from-string string)
  (call-with-input-string string
    (lambda (p)
      (read-json p))))

(define (write-json-to-string exp)
  (call-with-output-string
   (lambda (p)
     (write-json exp p))))


;; This just makes my life slightly easier
(define (vhash-ref vhash key)
  "Like assoc-ref but for a vhash"
  (cdr (vhash-assoc key vhash)))


(define (sjson->vjson sjson)
  "Convert sjson format to vjson
Really this means converting @-prefixed alists to vhashes"
  (define (convert-list exp)
    (map convert-json exp))

  (define (convert-json-alist exp)
    (fold
     (lambda (item prev)
       (match item
         ((key . val)
          (vhash-cons key (convert-json val) prev))))
     vlist-null
     ;; cdr because first item is the @
     (cdr exp)))

  (define (convert-json exp)
    (match exp
      ((? json-alist? exp)
       (convert-json-alist exp))
      ((? pair? exp)
       (convert-list exp))
      (anything-else anything-else)))

  (convert-json sjson))


(define (vjson->sjson vjson)
  "Convert sjson format to vjson
Really this means converting vhashes to @-prefixed alists
The opposite of sjson->vjson!"
  (define (convert-list exp)
    (map convert-json exp))

  (define (convert-vlist exp)
    (cons '@
     (vhash-fold
      (lambda (key val prev)
        (cons (cons key (convert-json val))
              (if (eq? prev vlist-null)
                  '()
                  prev)))
      vlist-null
      ;; cdr because first item is the @
      exp)))

  (define (convert-json exp)
    (match exp
      ((? vlist? exp)
       (convert-vlist exp))
      ((? pair? exp)
       (convert-list exp))
      (anything-else anything-else)))

  (convert-json vjson))
