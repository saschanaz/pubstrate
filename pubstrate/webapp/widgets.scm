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

(define-module (pubstrate webapp widgets)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (oop goops))

;;; Form handling library based on GOOPS with validation support,
;;; and a functional interface
;;;


;;; Utilities
;;; =========

(define (not-nil? x)
  (not (eq? x #nil)))

(define (maybe-render exp)
  (if exp (list exp) '()))

(define (maybe-render-inline exp)
  (if exp exp '()))

(define-syntax-rule (render-if test exp)
  (if test
      (list exp) '()))

(define-syntax-rule (render-inline-if test exp)
  (if test
      exp '()))


;;; Fields
;;; ======

;; For all the built-in widgets, this is the *only* class we inherit from!
(define-class <field> ()
  ;; The name as used in the actual form.
  (name #:init-keyword #:name
        #:getter field-name)
  (label #:init-keyword #:label
         #:init-value #f)
  (default #:init-keyword #:default
           #:init-value #nil
           #:getter field-default)
  ;; List of validators.
  ;; A validator returns a list of errors.
  (validators #:init-keyword #:validators
              #:init-value '()
              #:getter field-validators)
  (submitted #:init-value #f
             #:getter field-submitted?)

  ;; These are only used for submitted fields
  (data #:init-value #nil
        #:getter field-data)
  (errors #:init-value '()
          #:getter field-errors))

(define-method (initialize (field <field>) initargs)
  (next-method)
  ;; Make sure mandatory #:name field is provided
  (apply
   (lambda* (#:key name #:allow-other-keys #:rest r)
     (if (not (string? name))
         (throw 'field-missing-name field initargs)))
   initargs))

(define-method (field-copy-for-submit (field <field>))
  "Copy a field object so we can put the new one through the submit process."
  (make (class-of field)
    #:name (field-name field)
    #:label (slot-ref field 'label)
    #:default (field-default field)
    #:validators (field-validators field)))

(define-method (write (field <field>) port)
  (format port "#<~s #:name: ~s>"
          (class-name (class-of field)) (field-name field)))

(define-method (field-label (field <field>))
  "Get the field's label.

If label is a string, use that.  If it's a thunk, apply it to get a string.
Otherwise use the name field."
  (match (slot-ref field 'label)
    ((? procedure? thunk)
     (thunk))
    ((? string? str)
     str)
    (_ (field-name field))))

(define-method (field-has-data? (field <field>))
  (not-nil? (field-data field)))

(define-method (field-value (field <field>))
  (if (field-has-data? field)
      (field-data field)
      (field-default field)))

(define-method (field-has-value? (field <field>))
  (not-nil? (field-value field)))

(define-record-type <parse-failure>
  (parse-failure error)
  parse-failure?
  (error parse-failure-error))

(define-method (field-parse (field <field>) data)
  ;; The default is basically the identity function.
  ;; If we did hit an error, we would wrap an error description in a <parse-failure>
  data)

(define-method (field-valid? (field <field>))
  (cond ((not (field-submitted? field))
         'not-applicable)
        ((eq? (field-errors field) '())
         #t)
        (else #f)))

(define-method (field-submit (field <field>) data)
  (define new-field (field-copy-for-submit field))
  (slot-set! new-field 'submitted #t)
  (match (field-parse field data)
    ;; Look like there was a problem parsing, so we can stop
    ((? parse-failure? parse-failure)
     (slot-set! new-field 'data #:nil)
     (slot-set! new-field 'errors
                (list (parse-failure-error
                       parse-failure))))
    (val
     (let* ((errors
             (fold
              (lambda (validator prev)
                (match (validator val)
                  ;; no news is good news
                  ('() prev)
                  ((errors ...)
                   (append errors prev))))
              '() (field-validators field))))
       (slot-set! new-field 'data data)
       (slot-set! new-field 'errors
                  errors))))
  ;; return the new field
  new-field)



;;; Field types
;;; 

(define* (simple-input type field #:key (extra-attribs '()))
  (let ((value (field-value field)))
    `(input (@ (type ,type)
               (name ,(field-name field))
               ,@(render-if (not-nil? value)
                            `(value ,value))
               ,@extra-attribs))))

;; @@: *-field is appended in the case of ambiguity.
;;   Should it be in all cases?
(define-class <text-field> (<field>))

(define-method (field-render-html (field <text-field>))
  (simple-input "text" field))
(define-method (field-parse (field <text-field>) raw-data)
  raw-data)

(define-class <checkbox> (<field>))
(define-method (field-render-html (field <checkbox>))
  'TODO)
(define-method (field-parse (field <checkbox>) raw-data)
  'TODO)

(define-class <textarea> (<field>)
  (cols #:init-keyword #:cols
        #:init-value #f
        #:getter textarea-cols)
  (rows #:init-keyword #:rows
        #:init-value #f
        #:getter textarea-rows))

(define-method (field-render-html (field <textarea>))
  `(textbox (@ (name ,(field-name field))
               ,@(render-if (textarea-cols field)
                            `(cols ,(textarea-cols field)))
               ,@(render-if (textarea-rows field)
                            `(cols ,(textarea-rows field))))
            ,(let ((val (field-value field)))
               (if val
                   val ""))))

(define (render-number field)
  (simple-input "number" field))

(define-class <integer-field> (<field>))
(define-method (field-parse (field <integer-field>) raw-data)
  (define to-number
    (string->number raw-data))
  (if (integer? to-number)
      to-number
      (parse-failure "not an integer")))
(define-method (field-render-html (field <integer-field>))
  (render-number field))

(define-class <float-field> (<field>))
(define-method (field-parse (field <float-field>) raw-data)
  (define to-number
    (string->number raw-data))
  (if to-number
      to-number
      (parse-failure "neither an integer or floating point number")))
(define-method (field-render-html (field <float-field>))
  (render-number field))


;;; The Form (TM)
;;; =============

(define-class <form> ()
  (fields #:init-keyword #:fields
          #:getter form-fields)
  (data #:init-value #f
        #:getter form-data)
  ;; TODO: This isn't used yet, but should be for form-level errors.
  (validators #:init-keyword #:validators
              #:getter form-validators
              #:init-value '())
  (errors #:init-value '()
          #:getter form-errors)
  ;; We'll compute this later
  (%fields-table-promise)
  (%data-table #:init-value vlist-null))

(define (make-form . fields)
  (make <form> #:fields fields))

(define (form-fields-table-compute form)
  (fold
   (lambda (field prev)
     (vhash-cons (field-name field) field prev))
   vlist-null (form-fields form)))

(define-method (initialize (form <form>) initargs)
  (next-method)
  (slot-set! form '%fields-table-promise
             (delay (form-fields-table-compute form))))

(define-method (form-fields-table (form <form>))
  (force (slot-ref form '%fields-table-promise)))

(define* (form-fields-ref form name #:optional dflt)
  (match (vhash-assoc name (form-fields-table form))
    ((key . val)
     val)
    (#f dflt)))

(define* (form-data-ref form name #:optional (dflt #nil))
  (match (vhash-assoc name (slot-ref form '%data-table))
    ((key . val)
     val)
    (#f dflt)))

(define-method (form-submit (form <form>) data)
  "Return new form with submitted and processed DATA"
  (define data-table
    (alist->vhash data))
  (define new-fields
    (map-in-order
     (lambda (field)
       (define datum
         (match (vhash-assoc (field-name field) data-table)
           (#f #nil)
           ((key . val) val)))
       (field-submit field datum))
     (form-fields form)))
  (define new-form
    (make <form>
      #:fields new-fields))
  (slot-set! new-form 'data data)
  (slot-set! new-form '%data-table data-table)
  new-form)

(define-method (form-results-compute (form <form>))
  (map-in-order
   (lambda (field)
     (cons field
           (field-process field (form-data-ref form (field-name field)))))
   (form-fields form)))

(define-method (form-submitted? (form <form>))
  (if (slot-ref form 'data)
      #t #f))

;; TODO: Memoize this
(define-method (form-valid? (form <form>))
  "Determine whether a form currently contains valid data.

This only applies to submitted forms.  Non-submitted forms will receive
the symbol 'not-applicable as the value... which is truthy, but not #t"
  (call/ec
   (lambda (return)
     ;; If it's not submitted, it's not-applicable... which isn't #f...
     (if (not (form-submitted? form))
         (return 'not-applicable))
     ;; Are there any toplevel errors?
     (if (not (eq? (form-errors form)
                   '()))
         (return #f))
     ;; Check each of the fields
     (for-each
      (lambda (field)
        (if (not (field-valid? field))
            (return #f)))
      (form-fields form))
     ;; Ok, return #t... must be fine
     #t)))

(define-method (form-render-table (form <form>))
  "Renders the inner part of a table."
  (map
   (lambda (field)
     (define errors (field-errors field))
     `(tr (th ,(field-label field))
          (td ,(field-render-html field)
              ,@(render-if (not (null? errors))
                           `(ul (@ (class "field-errors"))
                                ,(map
                                  (lambda (error)
                                    `(li ,error))
                                  errors))))))
   (form-fields form)))
