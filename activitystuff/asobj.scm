(define-module (activitystuff asobj)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (activitystuff contrib json)
  #:use-module (activitystuff json-utils)
  #:use-module (activitystuff json-ld)
  #:export (make-asobj asobj?
            asobj-sjson asobj-env

            asobj-types asobj-expanded asobj-inherits
            asobj-id

            asobj-get asobj-set-field
            asobj-from-sjson
            asobj-from-json-string

            make-astype astype?
            astype-uri astype-parents astype-short-id astype-notes
            astype-inheritance

            make-asenv asenv?
            asenv-implied-context asenv-vocabs asenv-methods
            asenv-short-ids asenv-extra-context asenv-document-loader
            asenv-uri-map))



;;; <asobj>
;;; =======

;; Note that currently, this is effectively immutable (we don't expose
;; the mutation methods) but we aren't using (srfi srfi-9 gnu) because
;; we want to attach the promises.

(define-record-type <asobj>
  (make-asobj-intern sjson env)
  asobj?
  (sjson asobj-sjson)
  (env asobj-env)
  (types-promise asobj-types-promise set-asobj-types-promise!)
  (expanded-promise asobj-expanded-promise set-asobj-expanded-promise!)
  (inherits-promise asobj-inherits-promise set-asobj-inherits-promise!))

(set-record-type-printer!
 <asobj>
 (lambda (record port)
   (display "#<asobj>" port)))

;; @@: maybe have env be a kwarg?  Maybe use some kind of default-env?
(define (make-asobj sjson env)
  (let* ((asobj
          (make-asobj-intern sjson env))
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
      (call/ec
       (lambda (return)
         (fold
          (lambda (type-str prev)
            (cond
             ;; Try looking up by short id
             ((asenv-type-by-short-id env type-str) =>
              (lambda (astype)
                (cons astype prev)))
             ;; Try looking up by URI
             ((asenv-type-by-uri env type-str) =>
              (lambda (astype)
                (cons astype prev)))
             ;; Try expanded, which means breaking out of this...
             (else
              ;; TODO: Add expanding... but make it optional, depending
              ;;   on an object set in the <asenv>.
              ;; So eventually:
              ;;   (call/ec '*gotta-expand*)
              ;; For now though, just fold forward without this one
              prev)))
          '()
          types-as-list))))

    ;; Not used... yet!
    (define (try-expanding)
      'TODO)

    (try-type-without-expanding)))

(define (asobj-calculate-expanded asobj)
  'TODO)

(define (asobj-calculate-inherits asobj)
  'TODO)

(define (asobj-id asobj)
  (match (or (asobj-assoc "id" asobj)
             (asobj-assoc "@id" asobj))
    ((_ . val)
     val)
    (#f #f)))

(define (asobj-type-field asobj)
  (match (or (asobj-assoc "type" asobj)
             (asobj-assoc "@type" asobj))
    ((_ . val)
     val)
    (#f #f)))

(define (asobj-assoc key asobj)
  "Pull the value out of ASOBJ that matches KEY"
  (jsmap-assoc key (asobj-sjson asobj)))

;; User exposed methods
(define (asobj-types asobj)
  (force (asobj-types-promise asobj)))
(define (asobj-expanded asobj)
  (force (asobj-expanded-promise asobj)))
(define (asobj-inherits asobj)
  (force (asobj-inherits-promise asobj)))


(define* (asobj-get asobj field
                    #:key (default '*nothing*)
                    (as-asobj #f))
  'TODO)

(define (asobj-set-field asobj field value)
  "Return a new asobj with FIELD set to VALUE.
Field can be a string for a top-level field "
  'TODO)

(define (asobj-from-sjson sjson env)
  'TODO)

(define (asobj-from-json-string json-string env)
  'TODO)



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

(define (astype-calculate-inheritance astype)
  'TODO)

;;; User exposed methods
(define (astype-inheritance astype)
  (force (astype-inheritance-promise astype)))



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

(define (asenv-type-by-short-id asenv type-str)
  (hash-ref (asenv-short-ids-map asenv)
            type-str))

(define (asenv-type-by-uri asenv type-str)
  (hash-ref (asenv-uri-map asenv)
            type-str))

(define (kwargs-to-sjson kwargs)
  (define (convert-kwmap kwargs)
    (cons '@
     (let lp ((kwargs kwargs)
              (current-lst '()))
       (match kwargs
         (((? keyword? key) val . rest)
          (lp rest
              (cons (cons (symbol->string
                           (keyword->symbol key))
                          (convert-item val))
                    current-lst)))
         ('() current-lst)))))
  (define (convert-item item)
    (match item
      ;; convert dictionaries/json objects
      (('@ . rest)
       (cons '@
        (map
         (match-lambda
           ((key . val)
            (cons key (convert-item val))))
         rest)))
      ;; Convert lists
      ((lst ...)
       (map convert-item lst))
      ;; Extract sjson from an asobj
      ((? asobj? asobj)
       (asobj-sjson asobj))
      ;; Otherwise return item as-is
      (_ item)))
  (convert-kwmap kwargs))

(define (kwmap . kwargs)
  (kwargs-to-sjson kwargs))

(define (make-as astype asenv . kwargs)
  ;; (kwargs-to-sjson kwargs)
  'TODO)

