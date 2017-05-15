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

(define-module (pubstrate asobj)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (sjson)
  #:use-module (sjson utils)
  #:use-module (pubstrate json-ld)
  #:export (make-asobj asobj?
            asobj-sjson asobj-env asobj-private

            asobj-types asobj-expanded asobj-inherits
            asobj-id asobj-is-a?

            asobj-assoc asobj-ref asobj-ref-id asobj-sjson-assoc
            asobj-set asobj-delete
            asobj-from-json-string

            asobj-set-private asobj-set-private*
            asobj-private-assoc asobj-private-ref asobj-private-set

            asobj-pprint asobj-pprint-private asobj-pprint-combined

            asobj->string string->asobj
            asobj->combined-sjson combined-sjson->asobj
            asobj->combined-string combined-string->asobj

            make-astype astype?
            astype-uri astype-parents astype-short-id astype-notes
            astype-inheritance

            make-asenv asenv?
            asenv-implied-context asenv-vocabs asenv-methods
            asenv-short-ids asenv-extra-context asenv-document-loader
            asenv-uri-map

            make-as as-maker))



;;; <asobj>
;;; =======

;; Note that currently, this is effectively immutable (we don't expose
;; the mutation methods) but we aren't using (srfi srfi-9 gnu) because
;; we want to attach the promises.

(define-record-type <asobj>
  (make-asobj-intern sjson env private)
  asobj?
  (sjson asobj-sjson)
  (env asobj-env)
  (private asobj-private)
  (types-promise asobj-types-promise set-asobj-types-promise!)
  (expanded-promise asobj-expanded-promise set-asobj-expanded-promise!)
  (inherits-promise asobj-inherits-promise set-asobj-inherits-promise!))

;; @@: maybe have env be a kwarg?  Maybe use some kind of default-env?
(define* (make-asobj sjson env #:optional (private atlist-nil))
  (let* ((asobj
          (make-asobj-intern sjson env private))
         (types-promise
          (delay (asobj-calculate-types asobj)))
         (expanded-promise
          (delay (asobj-calculate-expanded asobj)))
         (inherits-promise
          (delay (asobj-calculate-inherits asobj))))
    ;; set promises
    (set-asobj-types-promise! asobj types-promise)
    (set-asobj-expanded-promise! asobj expanded-promise)
    (set-asobj-inherits-promise! asobj inherits-promise)

    ;; and return!
    asobj))

;; Where we actually calculate these things
(define (asobj-calculate-types asobj)
  (let* ((types-from-sjson
          (asobj-type-field asobj))
         (types-as-list
          (match types-from-sjson
            ((? string? _)
             (list types-from-sjson))
            (((? string? _) ...)
             types-from-sjson)
            (#f '())))
         (env (asobj-env asobj)))
    
    (define (try-type-without-expanding)
      ;; Here's where you turn into list of <astype>s
      ;; (and strings if unknown?)
      (fold
       (lambda (type-str prev)
         (cond
          ;; Try looking up without expanding
          ((asenv-type-by-str env type-str) =>
           (lambda (astype)
             (cons astype prev)))
          ;; TODO: Add expanding... but make it optional, depending
          ;;   on an object set in the <asenv>.
          ;; So eventually:
          ;;   (call/ec '*gotta-expand*)
          (else
           ;; Otherwise, just fold forward and skip this one
           prev)))
       '()
       types-as-list))

    ;; Not used... yet!
    (define (try-expanding)
      'TODO)

    (try-type-without-expanding)))

(define (asobj-calculate-expanded asobj)
  'TODO)

(define (asobj-calculate-inherits asobj)
  (astype-list-inheritance
   (asobj-types asobj)))

(define (asobj-id asobj)
  (match (or (asobj-assoc asobj "id")
             (asobj-assoc asobj "@id"))
    ((_ . val)
     val)
    (#f #f)))

(define (asobj-type-field asobj)
  (match (or (asobj-assoc asobj "type")
             (asobj-assoc asobj "@type"))
    ((_ . val)
     val)
    (#f #f)))

(define (jsobj-assoc-recursive sjson key-list)
  "Recursively traverse an sjson structure, and try to extract a value
from a key list"
  (define (traverse)
    (let lp ((key-list key-list)
             (sjson sjson))
      (cond 
       ;; found it!
       ((eq? key-list '())
        (cons '*got-it* sjson))
       ;; Okay, try matching the next part
       ((jsobj? sjson)
        (match (jsobj-assoc sjson (car key-list))
          ((_ . val)
           (lp (cdr key-list) val))
          (#f #f)))
       ;; Well it's not a json-object, so no sense searching
       (else #f))))
  (match (traverse)
    (('*got-it* . val)
     (cons key-list val))
    (#f #f)))

(define (asobj-sjson-assoc asobj key)
  "Pull the value out of ASOBJ that matches KEY

If KEY is a list, recursively look up keys until we (hopefully) find a value."
  (if (pair? key)
      (jsobj-assoc-recursive (asobj-sjson asobj) key)
      (jsobj-assoc (asobj-sjson asobj) key)))

(define (asobj-assoc asobj key)
  "Pull the value out of ASOBJ that matches KEY, and return it as an asobj

If it isn't a javascript object with a 'type' key, we return it as-is though.

If KEY is a list, recursively look up keys until we (hopefully) find a value."
  (define (asobj-style-json-object? obj)
    (and (jsobj? obj)
         (or (jsobj-assoc obj "type")
             (jsobj-assoc obj "@type"))))

  (define (convert-obj obj)
    (match obj
      ;; Looks like an asobj?
      ((? asobj-style-json-object? _)
       (make-asobj obj (asobj-env asobj)))
      ;; Return a list of possibly asobj obj as a list of asobjs
      ((? json-array? lst)
       (map convert-obj lst))
      (_ obj)))

  (match (asobj-sjson-assoc asobj key)
    ((key . val)
     (cons key (convert-obj val)))
    (#f #f)))

(define* (asobj-ref asobj key #:optional dflt)
  (match (asobj-assoc asobj key)
    ((_ . val)
     val)
    (#f dflt)))

(define* (asobj-ref-id asobj key)
  "Returns the id of whatever asobj's key is as a string, regardless
of whether the object matching the key is just a uri or is a full object. 

For example, for either of the following structures

  {\"type\": \"Create\",
   \"id\": \"https://example.org/foo/\",
   \"actor\": \"https://example.org/somebody/\",
   \"object\": {\"id\": \"https://example.org/bar/\",
                \"type\": \"Note\",
                \"content\": \"blah blah\"}}

or even:

  {\"type\": \"Create\",
   \"id\": \"https://example.org/foo/\",
   \"actor\": \"https://example.org/somebody/\",
   \"object\": \"https://example.org/bar/\"}

If we did:

  (asobj-ref-id asobj \"object\")

we'd get back:

  \"https://example.org/bar/\""
  (match (asobj-ref asobj key)
    ;; Got nothing?  return nothing
    (#f #f)
    ((? string? id-str)
     id-str)
    ((? asobj? obj)
     (asobj-id obj))))

;; User exposed methods
(define (asobj-types asobj)
  (force (asobj-types-promise asobj)))
(define (asobj-expanded asobj)
  (force (asobj-expanded-promise asobj)))
(define (asobj-inherits asobj)
  (force (asobj-inherits-promise asobj)))

(define (asobj-is-a? asobj astype)
  "Check whether or not ASOBJ is of ASTYPE."
  (member astype (asobj-inherits asobj)))

;; TODO: Document that if you asobj-set another asobj in,
;;   any private data in the to-be-nested asobj will be dropped!
(define* (asobj-set asobj key value #:key (delete #t))
  "Return a new asobj with FIELD set to VALUE.
Field can be a string for a top-level field "
  (let ((jsobj (if delete
                   (jsobj-delete (asobj-sjson asobj) key)
                   (asobj-sjson asobj))))
    (make-asobj
     (jsobj-acons jsobj
                  key (convert-sjson-with-maybe-asobj value))
     (asobj-env asobj)
     (asobj-private asobj))))

(define* (asobj-delete asobj key)
  (make-asobj
   (jsobj-delete (asobj-sjson asobj) key)
   (asobj-env asobj)
   (asobj-private asobj)))

(define (asobj-set-private asobj private)
  "Return a new <asobj> based on ASOBJ with private field set to PRIVATE"
  (make-asobj (asobj-sjson asobj)
              (asobj-env asobj)
              private))

(define (asobj-private-assoc asobj key)
  (if (pair? key)
      (jsobj-assoc-recursive (asobj-private asobj) key)
      (jsobj-assoc (asobj-private asobj) key)))

(define* (asobj-private-ref asobj key #:optional dflt)
  (match (asobj-private-assoc asobj key)
    ((_ . val)
     val)
    (#f dflt)))

(define* (asobj-private-set asobj key value #:key (delete #t))
  "Append KEY and VALUE to ASOBJ's private field.

If #:delete is provided, make sure this is the only item with this key."
  ;; @@: TODO: this is super similar to asobj-set, could probably
  ;;  do with a shared abstraction!
  (let ((jsobj (if delete
                   (jsobj-delete (asobj-private asobj) key)
                   (asobj-private asobj))))
    (make-asobj
     (asobj-sjson asobj)
     (asobj-env asobj)
     ;; @@: Do we need convert-sjson-with-maybe-asobj here?
     ;;   Will people really whack asobjs into the private field?
     (jsobj-acons jsobj key (convert-sjson-with-maybe-asobj value)))))

(define (asobj-set-private* asobj . kwargs)
  "Like asobj-set-private, but uses keyword argument fanciness instead of sjson"
  (asobj-set-private asobj (kwargs-to-sjson kwargs)))

(define (asobj->combined-sjson asobj)
  "Return an sjson object combining ASOBJ's sjson and private fields

Will look something like:
  {\"sjson\": {\"sjson-goes\": \"here\"},
   \"private\": {\"private\": \"stuff\"}}"
  `(@ ("sjson" ,(asobj-sjson asobj))
      ("private" ,(asobj-private asobj))))

(define (combined-sjson->asobj combined-sjson env)
  (let* ((sjson (jsobj-ref combined-sjson "sjson"))
         (private (jsobj-ref combined-sjson "private")))
    (make-asobj sjson env private)))

(define (asobj->combined-string asobj)
  (write-json-to-string (asobj->combined-sjson asobj)))

(define (combined-string->asobj combined-string env)
  (combined-sjson->asobj (read-json-from-string combined-string)
                         env))

(define (asobj->string asobj)
  (write-json-to-string (asobj-sjson asobj)))

(define (string->asobj json-string env)
  (make-asobj (read-json-from-string json-string)
              env))

(define* (asobj-pprint asobj #:key (port (current-output-port)))
  (json-pprint (asobj-sjson asobj) port))

(define* (asobj-pprint-private asobj #:key (port (current-output-port)))
  (json-pprint (asobj-private asobj) port))

(define* (asobj-pprint-combined asobj #:key (port (current-output-port)))
  (json-pprint (asobj->combined-sjson asobj) port))


;;; ============
;;; The <astype>
;;; ============

;; A @type than an ActivityStreams object might take on.
(define-record-type <astype>
  (make-astype-internal
   uri parents short-id notes)
  astype?
  (uri astype-uri)
  (parents astype-parents)
  (short-id astype-short-id)
  (notes astype-notes)
  
  (inheritance-promise astype-inheritance-promise
                       set-astype-inheritance-promise!))

(define* (make-astype id-uri parents
                      #:optional short-id notes)
  (let* ((astype (make-astype-internal
                  id-uri parents short-id notes))
         (inheritance-promise
          (delay (astype-calculate-inheritance astype))))
    (set-astype-inheritance-promise! astype inheritance-promise)
    astype))

;; @@: This might be TOTALLY unnecessary, if astype-list-inheritance
;;   is enough!
;;   At least, it's short enough of a calculation to do it immediately
;;   during make-astype so as to not use a promise

(define (astype-calculate-inheritance astype)
  'TODO)

;;; User exposed methods
(define (astype-inheritance astype)
  (force (astype-inheritance-promise astype)))

(define (astype-list-inheritance astype-list)
  "Calculate inheritance for a list of astypes"
  (define (traverse astype cur-family)
    (fold
     (lambda (parent prev)
       (traverse parent prev))
     (cons astype cur-family)
     (astype-parents astype)))

  (define (traverse-all)
    (fold
     (lambda (astype cur-family)
       (traverse astype cur-family))
     '()
     astype-list))

  (define (de-dupe family)
    (define seen (make-hash-table))
    (fold
     (lambda (astype prev)
       (if (hashq-ref seen astype)
           ;; If we've already seen it, continue
           prev
           ;; Otherwise, mark it seen and add it to the list
           (begin
             (hashq-set! seen astype #t)
             (cons astype prev))))
     '()
     family))

  (de-dupe (traverse-all)))



;;; =======================
;;; The <asenv> environment
;;; =======================

(define-record-type <asenv>
  (make-asenv-intern
   implied-context vocabs methods short-ids
   extra-context document-loader uri-map
   short-ids-map short-ids-reverse-map)
  asenv?

  (implied-context asenv-implied-context)
  (vocabs asenv-vocabs)
  (methods asenv-methods)
  (short-ids asenv-short-ids)
  (extra-context asenv-extra-context)
  (document-loader asenv-document-loader)
  (uri-map asenv-uri-map)
  (short-ids-map asenv-short-ids-map)
  (short-ids-reverse-map asenv-short-ids-reverse-map))

(define (build-astype-map vocabs get-key)
  "Build a uri map (a hash table) from vocabs"
  (define uri-map (make-hash-table))

  (for-each
   (lambda (vocab)
     (for-each
      (lambda (astype)
        (let ((key (get-key astype)))
          (if key
              (hash-set! uri-map (get-key astype) astype))))
      vocab))
   vocabs)
  uri-map)

(define (reversed-hash-table hash-table)
  (define new-hash-table (make-hash-table))
  (hash-for-each
   (lambda (key val)
     (hash-set! new-hash-table val key))
   hash-table)
  new-hash-table)

(define* (make-asenv
          #:key (vocabs '()) (methods '()) (short-ids '())
          extra-context
          ;; TODO: use %default-implied-context, %default-document-loader
          implied-context document-loader)
  (let* ((uri-map
          (build-astype-map vocabs astype-uri))
         (short-ids-map
          (build-astype-map vocabs astype-short-id))
         (short-ids-reverse-map
          (reversed-hash-table short-ids-map)))
    (make-asenv-intern implied-context vocabs methods short-ids
                       extra-context document-loader uri-map
                       short-ids-map short-ids-reverse-map)))

(define (asenv-type-by-str asenv type-str)
  "Try to get a type from ASENV looking up TYPE-STR, *without* expanding"
  (or (asenv-type-by-short-id asenv type-str)
      (asenv-type-by-uri asenv type-str)))

(define (asenv-type-by-short-id asenv type-str)
  (hash-ref (asenv-short-ids-map asenv)
            type-str))

(define (asenv-type-by-uri asenv type-str)
  (hash-ref (asenv-uri-map asenv)
            type-str))

(define (convert-sjson-with-maybe-asobj sjson)
  "Take some sjson that might have asobj objects embedded
and convert to sjson"
  (define (convert-item item)
    (match item
      ;; convert dictionaries/json objects
      ;; TODO: use json-object? instead
      (('@ . rest)
       (cons '@
        ;; @@: Maybe use jsobj-fold-unique?
        (map
         (match-lambda
           ((key val)
            (list key (convert-item val))))
         rest)))
      ;; Convert lists
      ((lst ...)
       (map convert-item lst))
      ;; Extract sjson from an asobj
      ((? asobj? asobj)
       (asobj-sjson asobj))
      ;; Otherwise return item as-is
      (_ item)))
  (convert-item sjson))

(define (kwargs-to-sjson kwargs)
  (define (convert-kwmap kwargs)
    (cons '@
     (let lp ((kwargs kwargs)
              (current-lst '()))
       (match kwargs
         (((? keyword? key) val . rest)
          (lp rest
              (cons (list (symbol->string
                           (keyword->symbol key))
                          (convert-sjson-with-maybe-asobj val))
                    current-lst)))
         ('() current-lst)))))
  (convert-kwmap kwargs))

(define (kwmap . kwargs)
  (kwargs-to-sjson kwargs))

(define (make-as astype asenv . kwargs)
  ;; TODO: Add the type from asenv, and add to the sjson
  (let* ((initial-sjson (kwargs-to-sjson kwargs))
         (sjson-with-type (jsobj-acons initial-sjson
                                       "type" (or (astype-short-id astype)
                                                  (astype-uri astype)))))
    (make-asobj sjson-with-type asenv)))

(define (as-maker asenv)
  (lambda (astype . kwargs)
    (apply make-as astype asenv kwargs)))



;;; Printers
;;; ========

;; Note, we had to move the asobj printer until *after* the
;; astype record definition, or it wasn't able to make use of
;; astype-short-id.  Weird, right?

(set-record-type-printer!
 <asobj>
 (lambda (asobj port)
   (format port "#<asobj [~a] ~s~a>"
           (string-join
            (map (lambda (type)
                   (or (astype-short-id type)
                       (astype-uri type)))
                 (asobj-types asobj))
            ", ")
           (asobj-id asobj)
           (if (jsobj-null? (asobj-private asobj))
               ""
               " +private"))))

(set-record-type-printer!
 <astype>
 (lambda (astype port)
   (cond
    ((astype-short-id astype) =>
     (lambda (short-id)
       (format port "#<astype ~a>" short-id)))
    (else (format port "#<astype ~s>"
                  (astype-uri astype))))))
