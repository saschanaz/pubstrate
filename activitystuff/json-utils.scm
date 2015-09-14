;; Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>
;; Copyright (C) 2015 Free Software Foundation, Inc.
;; (Borrows code from David Thompson's json library, submitted to Guile)

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
            jsmap->unique-alist
            jsmap->sorted-unique-alist
            jsmap-length

            vjson-array?

            read-json-from-string write-json-to-string
            vhash-ref
            sjson->vjson vjson->sjson

            default-pprint-indent
            pprint-json))

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
(define json-array? sjson-array?)

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


(define vjson-array? list?)

;; This just makes my life slightly easier
(define (vhash-ref vhash key)
  "Like assoc-ref but for a vhash"
  (cdr (vhash-assoc key vhash)))

;; TODO (define (vhash-map))

;; -- vhash edition --

;; (define (jsmap? obj)
;;   (or (eq? vlist-null obj)
;;       (vhash? obj)))

;; (define (jsmap-null? obj)
;;   (and (vlist? obj) (vlist-null? obj)))

;; (define jsmap-nil vlist-null)
;; (define jsmap-assoc vhash-assoc)
;; (define jsmap-ref vhash-ref)
;; (define jsmap-cons vhash-cons)
;; (define jsmap-delete vhash-delete)
;; ;; (define jsmap-map vhash-map)
;; (define jsmap->alist vlist->list)
;; (define alist->jsmap alist->vhash)
;; (define json-array? list?)

;; -- end vhash edition --

(define (jsmap->unique-alist jsmap)
  "Return an alist with only unique key-value pairs"
  (delete-duplicates (jsmap->alist jsmap)
                      (lambda (x y)
                        (equal? (car x) (car y)))))

(define* (jsmap->sorted-unique-alist jsmap #:optional (compare string<?))
  "Return a unique and sorted alist

Protip: change compare to string>? if you want to
fold instead of fold-right >:)"
  (sort
   (jsmap->unique-alist jsmap)
   (lambda (x y) (compare (car x) (car y)))))

;; @@: In a future time maybe we can make jsmap a record
;;   (it's abstract enough at this point) and memoize
;;   values like this with delay/force

(define (jsmap-length jsmap)
  "Find the number of pairs in a jsmap

This is O(n) (twice over!) so beware"
  (length (jsmap->unique-alist jsmap)))


;; Simpler json reading and writing functions

(define (read-json-from-string string)
  (call-with-input-string string
    (lambda (p)
      (read-json p))))

(define (write-json-to-string exp)
  (call-with-output-string
   (lambda (p)
     (write-json exp p))))


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


;;;
;;; Pretty-printer
;;;   ... adapted from David Thompson's contrib/json.scm
;;;

(define default-pprint-indent 2)

(define* (write-padding indent level port
                        #:optional opening)
  "Write (* INDENT LEVEL) level of whitespace to PORT"
  (if (not (and (or (eq? indent 0) (eq? level 0))
                opening))
      (newline port))

  (for-each (lambda _ (display " " port))
            (iota (* indent level))))

(define (write-string str port)
  "Write STR to PORT in JSON string format."
  (define (escape-char char)
    (display (match char
               (#\" "\\\"")
               (#\\ "\\\\")
               ;; (#\/ "\\/")
               (#\backspace "\\b")
               (#\page "\\f")
               (#\newline "\\n")
               (#\return "\\r")
               (#\tab "\\t")
               (_ char))
             port))

  (display "\"" port)
  (string-for-each escape-char str)
  (display "\"" port))

(define* (pprint-object alist port
                        #:key (indent default-pprint-indent) (level 0))
  "Write ALIST to PORT in JSON object format."
  (define next-level (1+ level))

  ;; Keys may be strings or symbols.
  (define key->string
    (match-lambda
     ((? string? key) key)
     ((? symbol? key) (symbol->string key))))

  (define (write-pair pair)
    (match pair
      ((key . value)
       (write-padding indent next-level port)
       (write-string (key->string key) port)
       (display ": " port)
       (pprint-json value port
                    #:indent indent
                    #:level next-level))))

  (write-padding indent level port #t)
  (display "{" port)
  (match alist
    (() #f)
    ((front ... end)
     (for-each (lambda (pair)
                 (write-pair pair)
                 (display "," port))
          front)
     (write-pair end)))
  (write-padding indent level port)
  (display "}" port))

(define* (pprint-array lst port
                       #:key (indent default-pprint-indent) (level 0))
  "Write LST to PORT in JSON array format."
  (define next-level (1+ level))

  (write-padding indent level port #t)
  (display "[" port)
  (match lst
    (() #f)
    ((front ... end)
     (for-each (lambda (val)
                 (write-padding indent next-level port)
                 (pprint-json val port
                              #:indent indent
                              #:level next-level)
                 (display "," port))
               front)
     (write-padding indent next-level port)
     (pprint-json end port
                  #:indent indent
                  #:level next-level)))
  (write-padding indent level port)
  (display "]" port))

(define* (pprint-json exp port
                      #:key (indent default-pprint-indent) (level 0))
  "Write EXP to PORT in JSON format."
  (define next-level (1+ level))

  (match exp
    (#t (display "true" port))
    (#f (display "false" port))
    ;; Differentiate #nil from '().
    ((and (? boolean? ) #nil) (display "null" port))
    ((? string? s) (write-string s port))
    ((? real? n) (display n port))
    (('@ . alist) (pprint-object alist port
                                 #:indent indent
                                 #:level level))
    ((vals ...) (pprint-array vals port
                              #:indent indent
                              #:level level))))
