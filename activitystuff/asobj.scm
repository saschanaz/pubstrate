(define-module (activitystuff asobj)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (activitystuff contrib json)
  #:use-module (activitystuff json-utils)
  #:use-module (activitystuff json-ld)
  #:export (make-asobj asobj?
            asobj-sjson asobj-env

            asobj-types asobj-expanded asobj-inherits

            asobj-get asobj-set-field
            asobj-from-sjson
            asobj-from-json-string
            ))




;; Note that currently, this is effectively immutable (we don't expose
;; the mutation methods) but we aren't using (srfi srfi-9 gnu) because
;; we want to attach the promises.

(define-record-type <asobj>
  (make-asobj-intern sjson env)
  asobj?
  (sjson asobj-sjson)
  (env sjson-env)
  (types-promise asobj-types-promise set-asobj-types-promise!)
  (expanded-promise asobj-expanded-promise set-asobj-expanded-promise!)
  (inherits-promise asobj-inherits-promise set-asobj-inherits-promise!))


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
  (let* ((sjson
          (asobj-sjson asobj))
         (types-from-sjson
          (or (json-alist-assoc "@type" sjson)
              (json-alist-assoc "type" sjson)
              ;; @@: Should this be '("Object") ?
              '()))
         (types-as-list
          (match types-from-sjson
            ((? string? _)
             (list types-from-sjson))
            (((? string? _) ...)
             types-from-sjson))))
    ;; Here's where you turn into list of <astype>s
    ;; (and strings if unknown?)
    'TODO))

(define (asobj-calculate-expanded asobj)
  'TODO)

(define (asobj-calculate-inherits asobj)
  'TODO)

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
  "Return a new asobj with `field' set to `value'
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
   id-uri parents id-short notes)
  astype?
  (id-uri astype-id-uri)
  (parents astype-parents)
  (id-short astype-id-short)
  (notes astype-notes)
  
  (inheritance-promise astype-inheritance-promise
                       set-astype-inheritance-promise!))

(define* (make-astype id-uri parents
                      #:key id-short notes)
  (let* ((astype (make-astype-internal
                  id-uri parents id-short notes))
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
   implied-context vocabs methods shortids
   extra-context document-loader uri-map)
  asenv?

  (implied-context asenv-implied-context)
  (vocabs asenv-vocabs)
  (methods asenv-methods)
  (shortids asenv-shortids)
  (extra-context asenv-extra-context)
  (document-loader asenv-document-loader)
  (uri-map asenv-uri-map))
