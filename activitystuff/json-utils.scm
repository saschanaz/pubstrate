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
            json-alist-null? json-alist-nil
            json-alist-assoc json-alist-ref json-alist-acons
            json-alist-delete
            json-alist-map json-alist-fold
            json-alist->alist
            alist->json-alist
            sjson-array?

            ;; Abstracting and hedging our bets
            ;; on json object representation
            jsmap?
            jsmap-null? jsmap-nil
            jsmap-assoc jsmap-ref jsmap-cons
            jsmap-delete
            jsmap-map jsmap-fold
            jsmap->alist alist->jsmap
            jsmap-fold-unique
            json-array?

            read-json-from-string write-json-to-string
            vhash-ref
            sjson->vjson vjson->sjson))

;; JSON helper procedures

(define (json-alist? json-scm)
  (and (pair? json-scm)
       (eq? (car json-scm) '@)))

(define (json-alist-null? json-alist)
  (match json-alist
    (('@ . '()) #t)
    (_ #f)))

(define json-alist-nil '(@))

(define (json-alist-assoc key json-alist)
  (assoc key (cdr json-alist)))

(define (json-alist-ref json-alist key)
  "Like assoc-ref for a json-alist"
  (assoc-ref (cdr json-alist) key))

(define (json-alist-acons key value json-alist)
  (cons '@ (acons key value (cdr json-alist))))

(define (json-alist-delete key json-alist)
  (alist->json-alist (alist-delete key (json-alist->alist json-alist))))

(define* (json-alist-map proc json-alist)
  "Map over PROC which takes a key and a value each from each pair of
json-alist"
  (map
   (lambda (x)
     (match x
       ((key . val)
        (proc key val))))
   (match json-alist
     (('@ . alist)
      alist))))

(define* (json-alist-fold proc initial json-alist)
  "Fold over PROC which takes a key and a value from each from each pair of
json-alist as well as the previous value"
  (fold
   (lambda (x prev)
     (match x
       ((key . val)
        (proc key val prev))))
   initial
   (match json-alist
     (('@ . alist)
      alist))))

(define (json-alist->alist json-alist)
  (match json-alist
    ((@ . alist)
     alist)))

(define (sjson-array? elt)
  (or (eq? elt '())
      (and (pair? elt)
           (not (eq? (car elt) '@)))))

(define (alist->json-alist alist)
  (cons '@ alist))

;; Hedging our bets on what we're using for javascript objects...
(define jsmap? json-alist?)
(define jsmap-null? json-alist-null?)
(define jsmap-nil json-alist-nil)
(define jsmap-assoc json-alist-assoc)
(define jsmap-ref json-alist-ref)
(define jsmap-cons json-alist-acons)
(define jsmap-delete json-alist-delete)
(define jsmap-map json-alist-map)
(define jsmap-fold json-alist-fold)
(define jsmap->alist json-alist->alist)
(define alist->jsmap alist->json-alist)
(define (jsmap-fold-unique proc init jsmap)
  "Like jsmap-fold, but skip keys that we've already seen"
  ;; Uses mutation under the hood, but still
  ;; referentially transparent
  (let* ((seen (make-hash-table))
         (see! (lambda (key) (hash-set! seen key #t)))
         (seen? (lambda (key) (hash-ref seen key))))     
    (jsmap-fold
     (lambda (key val prev)
       (if (seen? key) 
           prev
           (begin
             (see! key)
             (proc key val prev))))
     init jsmap)))
(define json-array? sjson-array?)

(define (vjson-array? elt)
  (or (eq? elt '())
      (pair? elt)))

;; Simpler json reading and writing functions

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


;; Conversion funcs

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
      ((? json-alist? alist)
       (convert-json-alist alist))
      ((items ...)
       (convert-list items))
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
      ((? vlist? vlist)
       (convert-vlist vlist))
      ((items ...)
       (convert-list items))
      (anything-else anything-else)))

  (convert-json vjson))
