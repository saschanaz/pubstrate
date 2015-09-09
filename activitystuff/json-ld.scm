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

;; See http://www.w3.org/TR/json-ld-api/

;; Note that this module uses alists, specifically because the json library
;; we're using also uses alists.  But is this a good idea?
;;
;; For small lists (~20 or so items) which is what I think most json
;; docs average out to, alists are just fine.  I didn't believe this,
;; but I was proven wrong by benchmarking myself: alists at this size
;; are much faster than vhashes and even slightly faster than hashmaps.
;; 
;; ... but we're planning on using these tools with contexts as big
;; as http://www.w3.org/ns/activitystreams and that might turn out to be
;; a real nightmare?
;;
;; So in the future we might move this into more efficient
;; datastructures ;P

(define-module (activitystuff json-ld)
  #:use-module (activitystuff json-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 vlist)
  #:use-module (web uri)
  #:export (update-context))


;; Special meaning in <active-context>, where 
(define undefined 'undefined)

(define (undefined? val)
  "Used to see if some record type fields have been defined yet"
  (eq? val undefined))

(define (defined? val)
  "Used to see if some record type fields have been defined yet"
  (not (undefined? val)))

;; An active context should not be confused with a json-ld @context.
;;
;; Instead, it is a transformation of json-ld contexts into a set of
;; term definitions and other data, the state information from
;; processing that context.

(define-immutable-record-type <active-context>
  (make-active-context base mapping inverse language vocab)
  active-context?
  ;; Base URI, if any
  ;;   equiv to "@base" in jsonld.py
  (base active-context-base)
  ;; Term mappings, aka a vhash mapped to a term definition
  ;;   equiv to "mappings" in json-ld.py
  ;; @@: maybe rename to terms
  (mapping active-context-mapping)
  ;; Inverse context I guess?  I don't really know what this is yet
  ;;   equiv to "inverse" in json-ld.py
  (inverse active-context-inverse)
  ;; Default language for this thing
  ;;   equiv to "@language" in json-ld.py
  (language active-context-language)
  ;; Vocabulary mapping
  ;;   equiv to "@vocab" in json-ld.py
  (vocab active-context-vocab))

(define initial-active-context
  (make-active-context #nil vlist-null #nil
                       undefined undefined))

;; @@: Should we have a make-initial-active-context that looks at
;;   options (whatever that is) so it might append
;;   the options' base to the initial-active-context base field?

;; So this is a term definition... it's the information about
;; various fields we build up in an <active-context> as we
;; process local contexts.

;; However, our code isn't *using* this one yet...

;; (define-immutable-record-type <term-def>
;;   (make-term-def id reverse type)
;;   term-def?
;;   ;; iri mapping
;;   ;;   equiv to "@id" in json-ld.py
;;   (id term-def-id)
;;   ;; boolean flag (TODO: meaning?)
;;   ;;   equiv to "reverse" in json-ld.py
;;   (reverse term-def-reverse)
;;   ;; (optional) type mapping
;;   ;;   equiv to "@type" in json-ld.py
;;   (type term-def-type)
;;   ;; (optional) language mapping
;;   ;;   equiv to "@language" in json-ld.py
;;   (language term-def-language))



;; I mix up IRI, URI, and URL all over this document just like everyone else.
;; <sunglasses>DEAL WITH IT</sunglasses>

(define (basic-deref-remote-context iri)
  (throw 'json-ld-error "remote resolver not implemented yet :)"))


;; This is effectively a check to see if something's an asbolute uri anyway...
(define absolute-uri? string->uri)

(define (string-startswith? string start-string)
  "Does STRING start with START-STRING ?"
  (string-contains string start-string 0))

(define (blank-node? obj)
  "See if OBJ is a blank node (a string that starts with \"_:\")"
  (and (string? obj)
       (> (string-length obj) 2)
       (string-startswith? obj "_:")))

(define (list-object? obj)
  "A list object is a JSON object that has a @list member"
  (and (jsmap? obj)
       (jsmap-assoc "@list" obj)))


(define (maybe-append-uri-to-base uri base)
  "A sorta-correct way to join a URI to BASE, assuming there is a BASE,
and assuming URI isn't a URI on its own.

If not, it just returns URI.

Does the dumbest possible thing: string-appends together the base and URI.
This might not be the best way to do it, further reading into
  http://tools.ietf.org/html/rfc3986#section-5.1
should be done.

TODO: It loooks like the correct version of this is done in jsonld.py
"
  (if (and (string? base)
           (not (absolute-uri? uri)))
      (string-append base uri)
      uri))


;; @@: Should we also include ":"?
(define json-ld-keywords
  '("@context" "@id" "@value" "@language" "@type"
    "@container" "@list" "@set" "@reverse"
    "@index" "@base" "@vocab" "@graph"))

(define (json-ld-keyword? obj)
  "See if OBJ is a json-ld special keyword

As a mild speed optimization, returns the remainder of json-ld-keywords
rathr than #t if true (#f of course if false)"
  (member obj json-ld-keywords))

(define (scalar? obj)
  (or (eq? obj #t)
      (eq? obj #f)
      (number? obj)
      (string? obj)))

;; ... helper func
(define (active-context-mapping-assoc key active-context)
  "Pull key out of a active-context's mapping"
  (vhash-assoc key (active-context-mapping active-context)))

(define (active-context-mapping-cons key val active-context)
  "Assign key to value in a active-context's mapping and return new active-context"
  (set-field
   active-context
   (active-context-mapping)
   (vhash-cons key val
               (active-context-mapping active-context))))

(define (active-context-mapping-delete key active-context)
  (set-field
   active-context
   (active-context-mapping)
   (vhash-delete key (active-context-mapping active-context))))



(define-syntax-rule (chain-value-calls proc1 proc2 ...)
  "Chain procedures which accept and multi-value-return the
same number of values/args together.

The chained function expects a producer function.

Example:

  (let ((proc1 (lambda (animals foods)
                 (values (cons 'hippos animals) (cons 'pizza foods))))
        (proc2 (lambda (animals foods)
                 (values (cons 'tigers animals) (cons 'seitan foods))))
        (proc3 (lambda (animals foods)
                 (values (cons 'rats animals) (cons 'mints foods)))))
    ((chain-value-calls proc1 proc2 proc3)
     (lambda () (values '(cats dogs) '(tofu cookies)))))"
  (lambda (producer)
    (call-with-values 
        (lambda () 
          (call-with-values producer proc1))
      proc2) ...))

(define-syntax-rule (chain-values-with-input (proc1 proc2 ...) input ...)
  "Chain procedures together, but then call with input

Example:

  (let ((proc1 (lambda (animals foods)
                 (values (cons 'hippos animals) (cons 'pizza foods))))
        (proc2 (lambda (animals foods)
                 (values (cons 'tigers animals) (cons 'seitan foods))))
        (proc3 (lambda (animals foods)
                 (values (cons 'rats animals) (cons 'mints foods)))))
    (chain-values-with-input (proc1 proc2 proc3)
     '(cats dogs) '(tofu cookies)))"
  ((chain-value-calls proc1 proc2 ...)
   (lambda () (values input ...))))


(define-syntax-rule (compose-forward func1 func2 ...)
  "Like compose, but run arguments first to last"
  (apply compose
         (reverse (list func1 func2 ...))))

;; Algorithm 6.1

(define* (process-context active-context local-context
                          #:optional (remote-contexts '())
                          #:key
                          (deref-context basic-deref-remote-context)
                          (base-iri #nil))
  "This function builds up a new active-context based on the
remaining context information to process from local-context"
  ;; TODO: break this out into process-context-loop
  ;; TODO: "result" is just "active-context", so just
  ;;   replace with that once we have the algorithm working?
  (let loop ((result active-context)
             ;; contexts to process, basically...
             ;; @@: Maybe should be called remaining-contexts?
             (local-context
              ;; If local-context is not in a sequence,
              ;; make it the only element in one
              (if (pair? local-context)
                  local-context
                  (list local-context)))
             (remote-contexts remote-contexts))
    ;; Some helper functions...
    ;; (the variables these reference get overriden
    ;; with let later, but we only use this early on)
    (define (append-to-base uri)
      "Append to the current base (if appropriate)"
      ;; Not useful if this is the first invocation of result,
      ;; but we don't use it there, so no biggie
      (maybe-append-uri-to-base uri (active-context-base result)))

    (define (equal-including-checking-base? uri1 uri2)
      "A check method to see if A matches B, or B with base apended"
      (or (equal? uri1 uri2)
          (equal? uri1 (append-to-base uri2))))

    ;; Are we done processing contexts?
    (if (null? local-context)
        ;; then return result immediately
        result
        ;; otherwise, process on!
        (let ((context (car local-context))
              (next-contexts (cdr local-context)))
          (match context
            ;; If null, result is a newly-initialized active context 
            (#nil
             (loop
              ;; new active context based on initial-active-context
              ;; but setting base-iri
              (set-field initial-active-context
                         (active-context-base) base-iri)
              next-contexts remote-contexts))

            ;; Okay it's a string, great, that means it's an iri
            ((? string? (= append-to-base context))
             (if (member context remote-contexts equal-including-checking-base?)
                 (throw 'json-ld-error
                        #:code "recursive context inclusion"
                        #:context context))
             (let ((derefed-context (deref-context context))
                   (remote-contexts (cons context remote-contexts)))
               (if (not (and (jsmap? derefed-context)
                             (jsmap-ref derefed-context "@context")))
                   (throw 'json-ld-error #:code "invalid remote context"
                          #:context context))
               ;; We made it this far, so recurse on the derefed context
               ;; then continue with that updated result
               (let* ((context derefed-context)
                      (result (process-context result context
                                               remote-contexts
                                               #:deref-context deref-context)))
                 (loop result next-contexts remote-contexts))))

            ((? jsmap? context)
             ;; Time to process over a json object of data.  Yay!
             ;; We're really just folding over this object here,
             ;; but three keys are special:
             ;; "@base", "@vocab", and "@language".
             ;; Otherwise, we process using the "Create term definition"
             ;; algorithm.
             ;;
             ;; Because that's a lot of steps, for readability
             ;; we break these out into functions then do the fold.
             (define (modify-result-from-base result base)
               (if (and base
                        (null? remote-contexts))
                   ;; In this case we'll adjusting the result's "@base"
                   ;; depending on what this context's @base is
                   (match base
                     ;; If the @base in this context is null, remove
                     ;; whatever current @base is in the result
                     ((? null? _)
                      ;; Remove base iri from result
                      (set-field result (active-context-base) undefined))

                     ;; If it's an absolute URI, let's set that as the result's
                     ;; @base
                     ((? absolute-uri? base-uri)
                      (set-field result (active-context-base) base-uri))

                     ;; Otherwise... if it's a string, we assume it's
                     ;; still a relative URI
                     ;; @@: Are more precise ways to define a relative URI at
                     ;;   this point?
                     ((? string? relative-base-uri)
                      ;; If the current *result's* base-uri is not null....
                      ;; resolve it against current base URI of result
                      (if (string? (jsmap-ref result "@base"))
                          (set-field result
                                     (active-context-base)
                                     (maybe-append-uri-to-base
                                      relative-base-uri (active-context-base result)))
                          ;; Otherwise, this is an error...
                          ;; "Value of @base in a @context must be an
                          ;;  absolute IRI or empty string."
                          (throw 'json-ld-error
                                 ;; @@: context vs result seems kinda vague
                                 ;;   to a user through this whole function, maybe
                                 #:code "invalid base IRI"
                                 #:context context
                                 #:result result
                                 #:base-iri relative-base-uri)))
                     (invalid-base-value
                      (throw 'json-ld-error
                             #:code "invalid base IRI"
                             #:base-iri invalid-base-value)))
                   ;; Otherwise, return unmodified result
                   result))

             (define (modify-result-from-vocab result vocab)
               (cond ((null? vocab)
                      ;; remove vocabulary mapping from result
                      (set-field result (active-context-vocab) undefined))
                     ;; If either an absolute IRI or blank node,
                     ;; @vocab of result is set to vocab
                     ((or (absolute-uri? vocab)
                          (blank-node? vocab))
                      (set-field result (active-context-vocab) vocab))
                     (else
                      (throw 'json-ld-error #:code "invalid vocab mapping"))))

             (define (modify-result-from-language result language)
               (cond ((null? language)
                      ;; remove vocabulary mapping from result
                      (set-field result (active-context-language) undefined))
                     ((string? language)
                      (set-field result (active-context-language)
                                 (string-downcase language)))
                     (else
                      (throw 'json-ld-error #:code "invalid default language"))))

             (define (build-result)
               (car
                (jsmap-fold-unique
                 (lambda (ctx-key ctx-val prev)
                   (match prev
                     ((result . defined)
                      (match ctx-key
                        ("@base"
                         (cons (modify-result-from-base result ctx-val)
                               defined))
                        ("@vocab"
                         (cons (modify-result-from-vocab result ctx-val)
                               defined))
                        ("@language"
                         (cons (modify-result-from-language result ctx-val)
                               defined))
                        (_
                         ;; Notably we aren't passing ctx-ctx-key here because
                         ;; (I suppose) create-term-definition has the whole context
                         ;; and so can look it up anyway...
                         (receive (result defined)
                             (create-term-definition
                              result context ctx-key defined)
                           (cons result defined)))))))
                 (cons result vlist-null) ;; second value here is "defined"
                 context)))

             (loop
              (build-result)
              next-contexts remote-contexts))

            ;; 3.3: Anything else at this point is an error...
            (_ (throw 'json-ld-error
                      #:code "invalid local context"
                      #:context context)))))))

;; Algorithm 6.2
;; -------------

(define* (create-term-definition active-context local-context term defined)
  ;; Let's see, has this term been defined, or started to be
  ;; defined yet?...
  (match (vhash-assoc term defined)
    ;; If term definition already was created, we do nothing
    ;; so return what we got!
    ((_ . #t)
     (values active-context defined))
    ;; If term definition is false, that means term definition
    ;; started but never completed... a cycle!  Abort, abort!
    ((_ . #f)
     (throw 'json-ld-error #:code "cyclic IRI mapping"))
    ;; Not referenced yet in defined, continue
    (#f
     (if (json-ld-keyword? term)
         (throw 'json-ld-error
                #:code "keyword redefinition"))

     (let (;; Set defined's value for this key to false, indicating
           ;; that we started processing 
           (defined
             (vhash-cons term #f defined))
           ;; @@: Do we really need to remove any existing term
           ;; definition for term in active context?  The spec says so,
           ;; but might it just be overridden?
           (active-context
            (context-mapping-delete term active-context))
           (value (jsmap-ref local-context term)))
       (cond
        ;; If value is null or a json object with "@id" mapping to null,
        ;; then mark term as defined and set term in
        ;; resulting context to null
        ((or (null? value)
             (and (jsmap? value)
                  (eq? (jsmap-ref value "@id") #nil)))
         (values
          (context-mapping-cons term #nil active-context)
          (vhash-cons term #t defined)))
        ;; otherwise, possibly convert value and continue...
        (else
         (let* ((value (cond ((string? value)
                              (jsmap-cons "@id" value jsmap-nil))
                             ((jsmap? value)
                              value)
                             (else
                              (throw 'json-ld-error
                                     #:code "invalid term definition"))))
                (value-reverse (jsmap-assoc "@reverse" value))
                ;; Initialize definition, and maybe add @type to it
                (value-type (jsmap-assoc "@type" value))
                ;; We'll expand on this definition below,
                ;; but might as well handle the "@type" stuff now
                ;; @@: we could refactor here and use the commented out
                ;;   term-definition...
                (definition 
                  (match value-type
                    ;; Start out with an empty definition if no @type
                    (#f jsmap-nil)
                    ;; Otherwise, if it's a string
                    (("@type" . (? string? type))
                     ;; @@: alist->jsmap would be more readable but mildly
                     ;;   less efficient here and elsewhere.  Does it matter?
                     (jsmap-cons "@type"
                                 (iri-expansion active-context type
                                                #:document-relative #t
                                                #:vocab #f
                                                #:local-context local-context
                                                #:defined defined)
                                 jsmap-nil))
                    ;; Otherwise, that's an error :(
                    (_
                     (throw 'json-ld-error
                                   #:code "invalid type mapping")))))
           ;; 11.3,
           ;; but also used in more-definition-and-active-context-adjustments
           (define* (definition-expand-iri definition
                      #:optional ensure-not-equal-context)
             (let* ((id-expansion
                     (iri-expansion active-context (cdr value-reverse)
                                    #:document-relative #t #:vocab #f
                                    #:local-context local-context
                                    #:defined defined))
                    (new-definition
                     (jsmap-cons "@id" id-expansion definition)))
               (if (not (or (absolute-uri? id-expansion)
                            (blank-node? id-expansion)))
                   ;; Uhoh
                   (throw 'json-ld-error
                          #:code "invalid IRI mapping"))
               (if (and ensure-not-equal-context
                        (equal? id-expansion "@context"))
                   ;; also uhoh
                   (throw 'json-ld-error
                          #:code "invalid keyword alias"))
               ;; oh okay!
               new-definition))

           (define (definition-handle-container-reverse definition)
             ;; 11.4
             (match (jsmap-assoc "@container" value)
               ;; just return original efinition if no @container
               (#f definition)
               ;; Otherwise make sure it's @set or @index or @nil
               ;; and set @container to this
               ((_ . (? (cut member <> '("@set" "@index" #nil)) container))
                (jsmap-cons "@container" container definition))
               ;; Uhoh, looks like that wasn't valid...
               (_
                (throw 'json-ld-error
                                #:code "invalid reverse property"))))

           ;; This one is an adjustment deluxe, it does a significant
           ;; amount of adjustments to the definition and builds
           ;; up an active context to be used as well.
           ;; @@: I wish I had a better name for this.
           (define (more-definition-and-active-context-adjustments
                    definition active-context)
             (let ((definition
                     (jsmap-cons "reverse" #f definition)))
               (define (set-iri-mapping-of-def-to-term)
                 (values (jsmap-cons "@id" term definition)
                         active-context))
               (cond
                ;; sec 13
                ((and (jsmap-assoc "@id" value)
                      (not (equal? (jsmap-ref value "@id")
                                   term)))
                 (values (definition-expand-iri definition #t)
                         active-context))
                ;; sec 14
                ((string-index term #\:)
                 ;; we cop out and go this route enough so
                 (if (or (absolute-uri? term)
                         (blank-node? term))
                     ;; set iri mapping of definition to term
                     (set-iri-mapping-of-def-to-term)
                     ;; otherwise, we've entered compact uri territory
                     ;; we'll recurse into this function to build up
                     ;; a new active context
                     (match (string-split term #\:)
                       ((prefix suffix-list ...)
                        (receive (active-context defined)
                            (create-term-definition
                             active-context local-context
                             prefix defined)
                          (let ((prefix-in-context
                                 (jsmap-assoc prefix active-context)))
                            (if prefix-in-context
                                (values (jsmap-cons
                                         "@id"
                                         (string-append
                                          (jsmap-ref (cdr prefix-in-context) "@id")
                                          (string-join suffix-list ":"))
                                         definition))
                                ;; okay, yeah, it's set-iri-mapping-of-def-to-term
                                ;; but we want to return the new active-context
                                (values (jsmap-cons "@id" term definition)
                                        active-context)))))
                       (_
                        ;; we originally threw an error, but meh...
                        ;; anyway, must not have been a compact uri after all...
                        (set-iri-mapping-of-def-to-term)))))

                ;; sec 15
                ((jsmap-assoc "@vocab" active-context)
                 (values (jsmap-cons
                          "@id"
                          (string-append
                           (jsmap-ref active-context "@vocab")
                           term) definition)
                         active-context))

                (else
                 (throw 'json-ld-error
                        #:code "invalid IRI mapping")))))

           (define (definition-handle-container-noreverse definition)
             (let ((value-container (jsmap-assoc "@container" value)))
               (if value-container
                   ;; Make sure container has an appropriate value,
                   ;; set it in the definition
                   (let ((container (cdr value-container)))
                     (if (not (member container '("@list" "@set"
                                                  "@index" "@language")))
                         (throw 'json-ld-error
                                #:code "invalid container mapping"))
                     (jsmap-cons "@container" container definition))
                   ;; otherwise, no adjustment needed apparently
                   definition)))

           (define (definition-handle-language definition)
             (let ((value-language (jsmap-assoc "@language" value)))
               (if value-language
                   ;; Make sure language has an appropriate value,
                   ;; set it in the definition
                   (let ((language (cdr value-language)))
                     (if (not (or (null? language) (string? language)))
                         (throw 'json-ld-error
                                #:code "invalid language mapping"))
                     (jsmap-cons "@language" language definition))
                   ;; otherwise, no adjustment needed apparently
                   definition)))

           (if value-reverse
               (begin
                 (if (jsmap-assoc "@id" value)
                     (throw 'json-ld-error
                            #:code "invalid reverse property"))
                 (if (not (string? (cdr value-reverse)))
                     (throw 'json-ld-error
                            #:code "invalid IRI mapping"))

                 (let* ((definition
                          ;; 11.5, set the definition's reverse property to true
                          (jsmap-cons
                           "reverse" #t
                           ;; 11.4
                           (definition-handle-container-reverse
                             ;; 11.3 goes into here...
                             (definition-expand-iri definition))))
                        ;; Set term definition of term in active-context to definition
                        (active-context (jsmap-cons term definition active-context)))
                   ;; return active-context and defined with term marked as #t
                   (values active-context (vhash-cons term #t defined))))
               (begin
                 ;; naming things is hard, especially when you're implementing
                 ;; the json-ld api
                 ;; Anyway this does a multi-value return of definition
                 ;; and active-context, adjusted as need be
                 (receive (definition active-context)
                     (more-definition-and-active-context-adjustments
                      definition active-context)
                   ;; TODO: resume at 16 here
                   (let* ((definition
                            (definition-handle-language
                              (definition-handle-container-noreverse definition)))
                          (active-context (jsmap-cons term definition active-context)))
                     (values active-context (vhash-cons term #t defined)))))))))))))

;; TODO: We have to redefine *ALL* entries that call iri-expansion
;;   to accept multiple value binding where the second value is defined
;;   because this function itself may call (create-term-definition)!
;;   ... maybe a good time to refactor the end of (create-term-definition)
;;   to be a bit more compose-y

;; Algorithm 6.1
(define* (iri-expansion active-context value
                        #:key
                        (document-relative #f) (vocab #f)
                        (local-context #nil)
                        ;; @@: spec says defined should be null, but
                        ;;   vlist-null seems to make more sense in our case
                        (defined vlist-null))
  "IRI expansion on VALUE within ACTIVE-CONTEXT

Does a multi-value-return of (expanded-iri active-context defined)"
  (define (maybe-update-active-context)
    (let ((context-matching-value (jsmap-assoc value local-context)))
      (if (and (not (null? local-context))       ; would jsmap? be better?
               context-matching-value
               (not (eq? (cdr context-matching-value)
                         #t)))
          ;; Okay, we're updating the context even further...
          (create-term-definition active-context local-context value defined)
          ;; nope, return as-is
          (values active-context defined))))

  (if (or (null? value)
          (json-ld-keyword? value))
      ;; keywords / null are just returned as-is
      (values value active-context defined)
      ;; Otherwise, see if we need to update the active context
      ;; and continue with expansion...
      (receive (active-context defined)
          (maybe-update-active-context)
        (cond
         ;; 3
         ((and (eq? vocab #t)
               (active-context-mapping-assoc value active-context))
          (values
           (jsmap-ref (cdr (active-context-mapping-assoc value active-context)) "@id")
           active-context
           defined))
         ;; 4
         ((string-index value #\:)
          (let* ((split-string (string-split value #\:))
                 (prefix (car split-string))
                 (suffix (string-join prefix ":")))
            (if (or (equal? prefix "_")
                    (string-startswith? suffix "//"))
                ;; It's a blank node or absolute IRI, return!
                (values value active-context defined)
                ;; otherwise, carry on to 4.3...
                (receive (active-context defined)
                    (if (and (not (null? local-context))
                             (jsmap-assoc prefix local-context)
                             (not (eq? (jsmap-ref local-context prefix))))
                        ;; ok, update active-context and defined
                        (create-term-definition active-context local-context
                                                prefix defined)
                        ;; nah leave them as-is
                        (values active-context defined))
                  (match (active-context-mapping-assoc prefix active-context)
                    ;; We've got a match, which means we're returning
                    ;; the term definition uri for prefix concatenated
                    ;; with the suffix
                    ((_ . prefix-term-def)
                     (values
                      (string-concatenate
                       (list (jsmap-ref prefix-term-def "@id") suffix))
                      active-context
                      defined))
                    ;; otherwise, return the value; it's already an absolute IRI!
                    (_ (values value active-context defined)))))))
         ;; 5
         ((and (eq? vocab #t)
               (defined? (active-context-vocab active-context)))
          (values
           (string-concatenate
            (list (active-context-vocab active-context) value))
           active-context
           defined))
         ;; 6
         ((eq? document-relative #t)
          (values
           (maybe-append-uri-to-base
            value (active-context-base active-context))
           active-context
           defined))

         ;; 7
         (else (values value active-context defined))))))


;; 7.1, expansion algorithm

;; Oh boy....

(define (expand-json-array active-context active-property json-array)
  (define (expand-items)
    (fold-right
     (lambda (item prev)
       (match prev
         ((result . active-context)
          (receive (expanded-item active-context)
              (expand-element active-context active-property item)
            (let ((active-property-term-result
                   (active-context-mapping-assoc active-property active-context)))
              ;; Boo, it's sad that json-ld prevents lists of lists.
              ;; ... but we're doing as the spec says :\
              (if (and (or (equal? active-property "@list")
                           (and active-property-term-result
                                (equal?
                                 (jsmap-ref (cdr active-property-term-result)
                                            "@container")
                                 "@list")))
                       (or (json-array? expanded-item)
                           (list-object? expanded-item)))
                  (throw 'json-error
                         #:code "list of lists"))

              (match expanded-item
                ((? json-array? _)
                 (cons (append expanded-item result)
                       active-context))
                ((? null? _)
                 (cons #nil active-context))
                (_
                 (cons (cons expanded-item result) active-context))))))))
     (cons '() active-context)
     json-array))

  (match (expand-items)
    ((expanded-array . active-context)
     (values expanded-array active-context))))

(define (expand-json-object active-context active-property jsmap)
  (define (process-pair key value result active-context)
    "Process a KEY VALUE pair, building up from RESULT within ACTIVE-CONTEXT"
    (let* ((term-mapping
            (delay (active-context-mapping-assoc key active-context)))
           (container-mapping
            (delay
              (if (force term-mapping)
                  (jsmap-ref (cdr (force term-mapping))
                             "@container")))))
      (define (get-expanded-value return)
        "Get expanded value; return is a prompt to bail out early"
        (receive (expanded-property active-context)
            (iri-expansion active-context key #:vocab #t)
          (cond
           ;; 7.4... get ready for a doosy
           ((or (eq? #nil expanded-property)
                (not (string-index expanded-property #\:))
                (not (keyword? expanded-property)))
            ;; carry on to the next key
            (return result active-context))
           ((keyword? expanded-property)
            (if (equal? active-property "@reverse")
                (throw 'json-ld-error
                       #:code "invalid reverse property map"))
            ;; already defined, uhoh
            (if (jsmap-assoc expanded-property result)
                (throw 'json-ld-error
                       #:code "colliding keywords"))

            (receive (expanded-value active-context)
                (match expanded-property
                  ;; 7.4.3
                  ("@id"
                   (if (not (string? value))
                       (throw 'json-ld-error
                              #:code "invalid @id value"))
                   ;; calls to iri-expansion also multi-value-return "defined"
                   ;; as well, but as the third argument, that's ignored by our receive
                   (iri-expansion active-context value
                                  #:document-relative #t))
                  ;; 7.4.4
                  ("@type"
                   (match value
                     ((? string? _)
                      (iri-expansion active-context value
                                     #:vocab #t #:document-relative #t))
                     (((? string? _) ...)
                      (match 
                        (fold-right
                         (lambda (item prev)
                           (match prev
                             ((array-result . active-context)
                              (receive (active-context expanded)
                                  (iri-expansion active-context item
                                                 #:document-relative #t)
                                (cons (cons expanded array-result) active-context)))))
                         (cons '() active-context)
                         value)
                        ((array-result . active-context)
                         (values array-result active-context))))
                     (_ (throw 'json-ld-error #:code "invalid type value"))))

                  ;; 7.4.5
                  ("@graph"
                   (expand-element active-context "@graph" value))

                  ;; 7.4.6
                  ("@value"
                   (match value
                     ((? null? _)
                      ;; jump out of processing this pair
                      (return (jsmap-cons "@value" #nil result)
                              active-context))
                     ;; otherwise, expanded value *is* value!
                     ((? scalar? _)
                      (values value active-context))
                     (_ (throw 'json-ld-error #:code "invalid value object"))))

                  ;; 7.4.7
                  ("@language"
                   (match value
                     ((? string? _)
                      (values (string-downcase value) active-context))
                     (_ (throw 'json-ld-error #:code "invalid language-tagged string"))))

                  ;; 7.4.8
                  ("@index"
                   (match value
                     ((? string? _)
                      (values value active-context))
                     (_ (throw 'json-ld-error #:code "invalid @index value"))))

                  ;; 7.4.9
                  ("@list"
                   ;; Bail out early if null or @graph to remove free-floating list
                   (if (member active-property '(#nil "@graph"))
                       (return result active-context))
                   (receive (expanded-value active-context)
                       (expand-element active-context active-property value)
                     ;; oops!  no lists of lists
                     (if (list-object? expanded-value)
                         (throw 'json-ld-error #:code "list of lists"))
                     ;; otherwise, continue with this as expanded value
                     (values expanded-value active-context)))

                  ;; 7.4.10
                  ("@set"
                   (expand-element active-context active-property element))

                  ;; 7.4.11
                  ;; I'm so sorry this is so complicated
                  ("@reverse"
                   (if (not (jsmap? value))
                       (throw 'json-ld-error "invalid @reverse value"))

                   (receive (expanded-value active-context)
                       (expand-elment active-context "@reverse" value)
                     (return
                      ;; here might be a great place to break out
                      ;; another function
                      (cond
                       ((jsmap-assoc "@reverse" expanded-value)
                        (jsmap-fold-unique
                         (lambda (property item result)
                           (let ((property-in-result
                                  (jsmap-assoc result property)))
                             (jsmap-cons
                              property
                              (if property-in-result
                                  (cons item (cdr property-in-result))
                                  (list item))
                              result)))
                         result
                         (jsmap-ref expanded-value "@reverse")))
                       ((pair? expanded-value)
                        (jsmap-fold-unique
                         (lambda (property items result)
                           (if (equal? property "@reverse")
                               ;; skip this one
                               result
                               ;; otherwise, continue
                               (fold
                                (lambda (item result)
                                  (let ((reverse-map (jsmap-ref result "@reverse")))
                                    (if (or (value-object? item)
                                            (list-object? item))
                                        (throw 'json-ld-error
                                               #:code "invalid reverse property value"))
                                    (jsmap-cons
                                     "@reverse"
                                     (jsmap-cons
                                      key
                                      (cons item
                                            (if (jsmap-assoc property reverse-map)
                                                (jsmap-ref property reverse-map)
                                                '()))
                                      reverse-map)
                                     result)))
                                result
                                items)))
                         (if (jsmap-assoc "@reverse" result)
                             result
                             (jsmap-cons "@reverse" jsmap-nil))
                         expanded-value))
                       (else result))
                      active-context))))
              (values
               (if (eq? expanded-value #nil)
                   ;; return as-is
                   result
                   ;; otherwise, set expanded-property member of result
                   ;; to expanded-value
                   (jsmap-cons expanded-property expanded-value result))
               expanded-property
               active-context)))

           ;; 7.5
           ;; If key's container mapping in active-context is @language and
           ;; value is a jsmap then value is expanded from a language map
           ((and (equal? (force container-mapping) "@language")
                 (jsmap? value))
            (values
             (fold
              (lambda (x expanded-value)
                (match x
                  ((language . language-value)
                   (fold-right
                    (lambda (item expanded-value)
                      (if (not (string? item))
                          (throw 'json-ld-error #:code "invalid language map value"))
                      (cons
                       (alist->jsmap
                        `(("@value" . ,item)
                          ("@language" . ,(string-downcase language))))
                       expanded-value))
                    expanded-value
                    (if (json-array? language-value)
                        language-value
                        (list language-value))))))
              '()
              ;; As a hack, this is sorted in REVERSE!
              ;; This way we can use normal fold instead of fold right.
              ;; Mwahahaha!
              (jsmap->sorted-unique-alist value string>?))
             expanded-property
             active-context))
           
           ;; 7.6
           ((and (equal? (force container-mapping) "@index")
                 (jsmap? value))
            ;; @@: In reality the code here is very similar to 
            ;;   in 7.5, but I think this is much more readable...
            (let loop ((l (jsmap->sorted-unique-alist value string>?))
                       (active-context active-context)
                       (result result))
              (match l
                ('()
                 (values result active-property active-context))
                (((index . index-value) rest ...)
                 (receive (index-value active-context)
                     (expand-element active-context key
                                     (if (json-array? index-value)
                                         index-value
                                         (list index-value)))
                   (loop rest active-context
                         (fold-right
                          (lambda (item expanded-value)
                            (cons
                             (if (jsmap-assoc "@index" item)
                                 item
                                 (jsmap-cons "@index" index item))
                             expanded-value))
                          expanded-value
                          index-value)))))))
           
           ;; 7.7
           (else
            (receive (expanded-value active-context)
                (expand-element active-context key value)
              (values expanded-value expanded-property active-context))))))

      (call/ec
       (lambda (return)
         (receive (expanded-value expanded-property active-context)
             (get-expanded-value return)
           (define (append-prop-val-to-result expanded-property expanded-value
                                              result)
             (jsmap-cons
              expanded-property
              (cons expanded-value
                    (if (jsmap-assoc expanded-property result)
                        (jsmap-ref result expanded-property)
                        '()))))

           ;; 7.8
           ;; if expanded value is null, ignore key by continuing
           ;; @@: could just be in the cond, right?
           (if (eq? expanded-value #nil)
               (return result active-context))
           
           ;; Augh, these 7.9-7.11 sections are frustrating
           ;; continue with line 1927 in jsonld.py
           ;; # convert expanded value to @list if container specifies it
           
           (cond
            ;; 7.9
            ((and (equal? (force container-mapping) "@list")
                  (not (list-object? expanded-value)))
             (values
              (append-prop-val-to-result
               expanded-property
               (alist->jsmap
                `(("@list" . ,(if (json-array? expanded-value)
                                  expanded-value
                                  (list expanded-value)))))
               result)
              active-context))

            ;; 7.10
            ;; Looks like a reverse property?
            ((and (force term-mapping)
                  (jsmap-ref (cdr (force term-mapping)) "reverse"))
             (let* ((result (if (jsmap-assoc "@reverse" result)
                                result
                                (jsmap-cons "@reverse" '() result)))
                    (reverse-map (jsmap-ref result "@reverse"))
                    (expanded-value (if (json-array? expanded-value)
                                        expanded-value
                                        (list expanded-value))))
               
               (values
                (fold
                 (lambda (item result)
                   (if (or (value-object? item)
                           (list-object? item))
                       (throw 'json-ld-error #:code "invalid reverse property value"))
                   (jsmap-cons "@reverse"
                               (jsmap-cons expanded-property
                                           (cons item
                                                 (if (jsmap-assoc expanded-property reverse-map)
                                                     (jsmap-ref reverse-map expanded-property)
                                                     '()))
                                           reverse-map)
                               result))
                 result
                 expanded-value)
                active-context)))

            ;; 7.11
            (else
             (values (append-prop-val-to-result
                      expanded-property expanded-value result)
                     active-context))))))))

  (define (build-result active-context)
    (reverse
     ;; We don't do the reverse hack here as above because of the
     ;; (admittedly unlikely?) chance that builing up the active-context
     ;; in the other order would be wrong?
     (let loop ((l (jsmap->sorted-unique-alist jsmap))
                (active-context active-context)
                (result jsmap-nil))
       (match l
         ('()
          (values result active-context))
         (((key . val) rest ...)
          (receive (result active-context)
              (process-pair key val result active-context)
            (loop rest active-context result)))))))

  (let* ((jsmap-context (jsmap-assoc "@context" jsmap))
         (active-context
          (if jsmap-context
              (process-context active-context (cdr jsmap-context))
              active-context))
         (permitted-value-results '("@value" "@language" "@type" "@index")))
    (receive (result active-context)
        (build-result active-context)
      (call/ec
       (lambda (return)
         (define (adjust-result-1 result)
           (cond
            ;; sec 8
            ((jsmap-assoc "@value" result)
             ;; 8.1, make sure result does not contain keys outside
             ;;   of permitted set
             (if (or (not (match (jsmap->alist result)
                            ((((? (cut member <> permitted-value-results)
                                  key) . val) ...)
                             #t)
                            (_ #f)))
                     (and (jsmap-assoc "@language" result)
                          (jsmap-assoc "@type" result)))
                 (throw 'json-ld-error #:code "invalid value object"))

             (let ((result-value (jsmap-ref result "@value")))
               (cond ((eq? result-value #nil)
                      (return #nil active-context))
                     ((and (not (string? result-value))
                           (jsmap-assoc "@language" result))
                      (throw 'json-ld-error #:code "invalid typed value"))
                     ;; TODO: resume here at 8.4
                     ((and (jsmap-assoc "@type" result)
                           (not (absolute-uri? (jsmap-ref result "@type"))))
                      (throw 'json-ld-error #:code "invalid typed value"))
                     (else result))))
            ;; sec 9
            ;; @@: unnecessarily pulling type out of result several times,
            ;;   we could do it just once... maybe with a (delay) at top
            ;;   of cond?
            ((and (jsmap-assoc "@type" result)
                  (json-array? (jsmap-ref result "@type")))
             (jsmap-cons "@type" (jsmap-ref result "@type" result) result))

            ;; sec 10
            ((or (jsmap-assoc "@set" result)
                 (jsmap-assoc "@list" result))
             ;; @@: Hacky
             (let* ((num-members (jsmap-length result)))
               ;; 10.1
               (if (not (or (eq? num-members 1)
                            (and (jsmap-assoc "@index" result)
                                 (eq? num-members 2))))
                   (throw 'json-ld-error #:code "invalid set or list object"))

               ;; 10.2
               (let ((set-mapping (jsmap-assoc "@set" result)))
                 (if set-mapping
                     (cdr set-mapping)
                     result))))))

         ;; sec 11
         (define (adjust-result-2 result)
           (if (and (jsmap-assoc "@language" result)
                    (eq? (jsmap-length result) 1))
               (return #nil active-context)
               result))

         ;; sec 12
         (define (adjust-result-3 result)
           ;; Graph adjustments...
           (if (member active-property '(#nil "@graph"))
               ;; drop free-floating values
               (cond ((or (eq? (jsmap-length result) 0)
                          (jsmap-assoc "@value" result)
                          (jsmap-assoc "@list" result))
                      (return #nil active-context))
                     ;; @@: Do we need to check jsmap? at this point?
                     ;;   I think the only other thing result becomes is #nil
                     ;;   and we return it explicitly in such a case
                     ((and (jsmap-assoc "@id" result)
                           (eq? 1 (jsmap-length result)))
                      (return #nil active-context))

                     (else result))
               ;; otherwise, do nothing
               result))

         ;; sec 13
         (values
          ((compose-forward adjust-result-1 adjust-result-2 adjust-result-3)
           result)
          active-context))))))

(define (expand-element active-context active-property element)
  (match element
    ((? null? _)
     (values #nil active-context))
    ((? scalar? _)
     (if (member active-property '(#nil "@graph"))
         (values #nil active-context)
         ;; Note that value-expansion should also do a multi-value return
         ;; with active-context... in theory...
         (value-expansion active-context active-property element)))
    ((? json-array? _)
     ;; Does a multi-value return with active context
     (expand-json-array active-context active-property element))
    ((? jsmap? _)
     (expand-json-object active-context active-property element))))

(define (expand vjson)
  ;; TODO: convert all jsmap to vjson so we don't have this (v?)
  "Expand (v?)json using json-ld processing algorithms"
  (receive (expanded-result active-context)
      (expand-element initial-active-context #nil vjson)
    ;; final other than arrayify that is!
    (define (final-adjustments expanded-result)
      (cond ((and (jsmap? expanded-result)
                 (eq? 1 (jsmap-length expanded-result))
                 (jsmap-assoc "@graph" expanded-result))
            (jsmap-ref "@graph" expanded-result))
           ((eq? expanded-result #nil)
            '())
           (else expanded-result)))
    (define (arrayify expanded-result)
      (if (json-array? expanded-result)
          expanded-result
          (list expanded-result)))
    (values
     ((compose-forward final-adjustments arrayify)
      expanded-result)
     active-context)))


;; Algorithm 7.2

(define (value-expansion active-context active-property value)
  #f
  )
