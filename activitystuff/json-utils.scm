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
  #:export (json-alist?
            json-assoc json-ref json-acons
            read-json-from-string write-json-to-string))

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
