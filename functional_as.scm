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

(use-modules (srfi srfi-9)
             (srfi srfi-9 gnu)
             (ice-9 match)
             (ice-9 vlist))

(define-record-type <as-type>
  (make-as-type uri parents props)
  as-type?
  (uri as-type-uri)
  (parents as-type-parents)
  (props as-type-props))

(set-record-type-printer!
 <as-type>
 (lambda (record port)
   (format port "#<as-type: ~s>" (as-type-uri record))))

;; ActivityStreams type Object (or Link)
(define-record-type <as-obj>
  (make-as-obj-internal type fields)
  as-obj?
  (type as-type)
  (fields as-fields)
  ;; Not public
  (--json-promise-- as--json-promise as--set-json-promise))

(set-record-type-printer!
 <as-obj>
 (lambda (record port)
   (format port "#<<as-obj> type: ~a>" (as-type record))))

;; TODO
(define (as-to-json-internal as-obj)
  #nil)

(define (make-as-obj type fields)
  "Constructor for making activitystreams objects

TYPE is an <as-type> and FIELDS is a vhash of fields.

In general it is recommended that you use (make-as) instead,
which has a much more friendly syntax using keywords."
  (let ((as-obj (make-as-obj-internal type fields)))
    ;; Yes, its promise field references itself ;p
    ;; 
    ;; Alternately, we could make the promise reference just the type
    ;; and fields?
    (as--set-json-promise
     as-obj (delay (as-to-json-internal as-obj)))
    as-obj))

(define (make-as type . fields)
  "Easily make an activitystreams object from keyword arguments

The first argument after type may be a string for the @id uri.  All
remaining arguments are keyword arguments used to construct the object
fields.

Example usage:
  (define root-beer-note
    (make-as <Post>
      ;; Putting the string first is the same thing as #:@id
      \"http://tsyesika.co.uk/act/foo-id-here/\"
      #:actor (make-as <Person>
                #:@id \"http://tsyesika.co.uk\"
                #:displayName \"Jessica Tallon\")
      #:to (list \"acct:cwebber@identi.ca\")
      #:object (make-as <Note>
                 \"http://tsyesika.co.uk/chat/sup-yo/\"
                 #:content \"Up for some root beer floats?\")))
"
  (define (vhash-fields fields)
    (let loop ((fields fields)
               (hashed-fields vlist-null))
      (match fields
        (((? keyword? key) val . rest)
         (loop rest
               (vhash-cons key val hashed-fields)))
        (()
         hashed-fields))))

  (match fields
    (((? string? id) rest ...)
     (make-as-obj type
                  (vhash-cons "@id" id
                              (vhash-fields rest))))
    ((. fields)
     (make-as-obj type (vhash-fields fields)))))

(define-syntax-rule (make-as-obj-factory proc-name type)
  (define (proc-name . args)
    (apply make-as type args)))

(define (as-to-json as-obj)
  (force (as--json-promise as-obj)))


;; TODO: Expand to handle function sugar with make-as-obj-factory

(define-syntax define-asclass
  (syntax-rules ()
    "Define an activitystream class / type"
    ((define-asclass asclass (parent ...)
       as-uri as-properties)
     (define asclass (make-as-type as-uri (list parent ...)
                                   as-properties)))
    ;; invoke the macro above, but also add an object factory function
    ((define-asclass asclass (parent ...)
       as-uri as-properties factory-name)
     ;; invoke the function
     (begin
       (define-asclass asclass (parent ...)
         as-uri as-properties)
       ;; but also make a factory function
       (make-as-obj-factory factory-name asclass)))))

;;; ******************
;;; * Standard vocab *
;;; * ============== *
;;; ******************
