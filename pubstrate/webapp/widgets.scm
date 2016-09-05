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


;;; Utilities
;;; =========

(define (not-nil? x)
  (not (eq? x #nil)))


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
              #:getter field-validators))

(define-method (initialize (field <field>) initargs)
  (next-method)
  ;; Make sure mandatory #:name field is provided
  (apply
   (lambda* (#:key name #:allow-other-keys #:rest r)
     (if (not (string? name))
         (throw 'field-missing-name field initargs)))
   initargs))

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

(define-record-type <parse-failure>
  (parse-failure error)
  parse-failure?
  (error parse-failure-error))

(define-method (field-parse (field <field>) data)
  ;; The default is basically the identity function.
  ;; If we did hit an error, we would wrap an error description in a <parse-failure>
  data)

(define-record-type <field-result>
  (make-field-result data errors)
  field-result?
  (data field-result-data)
  (errors field-result-errors))

(define (field-result-valid? field-result)
  (eq? (field-result-errors field-result)
       '()))

(define (field-result-data? field-result)
  "Check to see if field-result contains data.
It might contain #nil instead."
  (not (eq? (field-result-data field-result)
            #nil)))

(define-method (field-process (field <field>) data)
  (match (field-parse field data)
    ;; Look like there was a problem parsing, so we can stop
    ((? parse-failure? parse-failure)
     (make-field-result #nil
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
       (make-field-result data errors)))))



;;; Field types
;;; 

(define* (simple-input type name #:optional (value #nil))
  `(input (@ (type ,type)
             (name ,name)
             ,@(if (not-nil? value)
                   `((value value))
                   '()))))

(define-class <text-field> (<field>))

(define-method (field-render-html (field <text>) value)
  (simple-input "text" (field-name field) value))
(define-method (field-parse (field <text>) data)
  data)

(define-class <checkbox-field> (<field>))

(define-method (field-render-html (field <checkbox>) data)
  'TODO)
(define-method (field-parse (field <checkbox>) data)
  'TODO)



;;; The Form (TM)
;;; =============

(define-class <form> ()
  (fields #:init-keyword #:fields
          #:getter form-fields)
  (data #:init-value #f
        #:getter form-data)
  (results #:init-value #f
           #:getter form-results)
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
  (define new-form
    (make <form>
      #:fields (form-fields form)))
  (slot-set! new-form 'data data)
  (slot-set! new-form '%data-table (alist->vhash data))
  (slot-set! new-form 'results
             (form-results-compute new-form))
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
     ;; Check each of the field results
     (for-each
      (match-lambda
        ((field . result)
         (if (not (field-result-valid? result))
             (return #f))))
      (form-results form))
     ;; Ok, return #t... must be fine
     #t)))

(define-method (form-render-table (form <form>))
  "Renders the inner part of a table."
  (map
   
   
   )
  )
