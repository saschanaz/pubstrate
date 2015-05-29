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

(define-syntax-rule (make-as-obj-factory proc-name type)
  (define (proc-name . args)
    (apply make-as type args)))

;; ActivityStreams type Object (or Link)
(define-record-type <as-obj>
  (make-as-obj-internal type fields)
  as-obj?
  (type as-type)
  (fields as-fields)
  ;; Not public
  (--json-promise-- as--json-promise as--set-json-promise))

;; TODO
(define (as-to-json-internal as-obj)
  #nil)

(define (make-as-obj type fields)
  (let ((as-obj (make-as-obj-internal type fields)))
    ;; Yes, its promise field references itself ;p
    ;; 
    ;; Alternately, we could make the promise reference just the type
    ;; and fields?
    (as--set-json-promise
     as-obj (delay (as-to-json-internal as-obj)))
    as-obj))

(define (make-as type . fields)
  (define (convert-fields-to-vhash fields)
    ;; TODO
    #nil)
  (make-as type (convert-fields-to-vhash fields)))

(define (as-to-json as-obj)
  (force (as--json-promise as-obj)))


(define-syntax-rule (define-asclass asclass (parent ...)
                      as-uri as-properties)
  (define asclass (make-as-type as-uri (list parent ...) as-properties)))
