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
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:export (update-context))


;; I mix up IRI, URI, and URL all over this document just like everyone else.
;; <sunglasses>DEAL WITH IT</sunglasses>

(define (basic-deref-remote-context iri)
  (throw 'json-ld-error "remote resolver not implemented yet :)"))

;; This is effectively a check to see if something's an asbolute uri anyway...
(define absolute-uri? string->uri)

(define (blank-node? obj)
  "See if OBJ is a blank node (a string that starts with \"_:\")"
  (and (string? obj)
       (> (string-length obj) 2)
       (equal? (substring obj 0 2) "_:")))

(define (maybe-append-uri-to-base uri base)
  "A sorta-correct way to join a URI to BASE, assuming there is a BASE,
and assuming URI isn't a URI on its own.

If not, it just returns URI.

Does the dumbest possible thing: string-appends together the base and URI.
This might not be the best way to do it, further reading into
  http://tools.ietf.org/html/rfc3986#section-5.1
should be done.

NOTE: It loooks like the correct version of this is done in jsonld.py
"
  (if (and (string? base)
           (not (absolute-uri? uri)))
      (string-append base uri)
      uri))

;; Algorithm 6.1

(define* (update-context active-context local-context
                         #:optional (remote-contexts '())
                         #:key
                         (deref-context basic-deref-remote-context)
                         base-iri)
  "This function builds up a new active-context based on the
remaining context information to process from local-context"
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
    (define (append-to-base uri)
      "Append to the current base (if appropriate)"
      ;; Not useful if this is the first invocation of result,
      ;; but we don't use it there, so no biggie
      (maybe-append-uri-to-base uri (json-ref result "@base")))

    (define (equal-including-checking-base? uri1 uri2)
      "A check method to see if A matches B, or B with base apended"
      (or (equal? uri1 uri2)
          (equal? uri1 (append-to-base uri2))))

    (if (null? local-context) result
        (let ((context (car local-context))
              (next-contexts (cdr local-context)))
          (match context
            ;; If null, result is a newly-initialized active context 
            (#nil
             (loop `(@ ("@base" . ,(or base-iri #nil)))
                   next-contexts remote-contexts))

            ;; Okay it's a string, great, that means it's an iri
            ((? string? (= append-to-base context))
             (if (member context remote-contexts equal-including-checking-base?)
                 (throw 'json-ld-error
                        #:code "recursive context inclusion"
                        #:context context))
             (let ((derefed-context (deref-context context)))
               (if (not (and (json-alist? derefed-context)
                             (json-ref derefed-context "@context")))
                   (throw 'json-ld-error #:code "invalid remote context"
                          #:context context))
               ;; We made it this far, so recurse on the derefed context
               ;; then continue with that updated result
               (let* ((context derefed-context)
                      (result (update-context result context
                                              (cons context remote-contexts)
                                              #:deref-context deref-context)))
                 (loop result next-contexts remote-contexts))))

            ((? json-alist? context)
             ;; Time to process over a json object of data.  Yay!
             ;; We're really just folding over this object here,
             ;; but three keys are special:
             ;; "@base", "@vocab", and "@language".
             ;; Otherwise, we process using the "Create term definition"
             ;; algorithm.
             ;;
             ;; Because that's a lot of steps, for readability
             ;; we break these out into functions then do the fold.
             (define (modify-result-from-base result context-base)
               (if (and context-base
                        (null? remote-contexts))
                   ;; In this case we'll adjusting the result's "@base"
                   ;; depending on what this context's @base is
                   (match context-base
                     ;; If the @base in this context is null, remove
                     ;; whatever current @base is in the result
                     ((? null? _)
                      ;; Remove base iri from result
                      (remove (lambda (x) (match x (("@base" . _) #t) (_ #f)))
                              result))

                     ;; If it's an absolute URI, let's set that as the result's
                     ;; @base
                     ((? asbolute-uri? base-uri)
                      (json-acons "@base" base-uri result))

                     ;; Otherwise... if it's a string, we assume it's
                     ;; still a relative URI
                     ((? string? relative-base-uri)
                      ;; If the current *result's* base-uri is not null....
                      ;; resolve it against current base URI of result
                      (if (string? (json-ref result "@base"))
                          (json-acons "@base"
                                      (maybe-append-uri-to-base
                                       relative-base-uri (json-ref result "@base"))
                                      result)
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

             (define (modify-result-from-vocab result context-vocab)
               (cond ((null? context-vocab)
                      ;; remove vocabulary mapping from result
                      (remove (lambda (x) (match x (("@vocab" . _) #t) (_ #f)))
                              result))
                     ;; If either an absolute IRI or blank node,
                     ;; @vocab of result is set to context-vocab
                     ((or (absolute-uri? context-vocab)
                          (blank-node? context-vocab))
                      (json-acons "@type" context-vocab result))
                     (else
                      (throw 'json-ld-error #:code "invalid vocab mapping"))))

             (define (modify-result-from-language result context-language)
               (cond ((null? context-language)
                      ;; remove vocabulary mapping from result
                      (remove (lambda (x) (match x (("@language" . _) #t) (_ #f)))
                              result))
                     ((string? context-language)
                      (json-acons "@language" (string-downcase context-language)
                                  result))
                     (else
                      (throw 'json-ld-error #:code "invalid default language"))))

             (define (build-result)
               (car
                (fold
                 (lambda (x result)
                   (let ((ctx-entry (car x))
                         (defined (cdr x)))
                     (match ctx-entry
                       (("@base" . ctx-base)
                        (cons (modify-result-from-base result ctx-base)
                              defined))
                       (("@vocab" . ctx-vocab)
                        (cons (modify-result-from-vocab result ctx-vocab)
                              defined))
                       (("@language" . ctx-language)
                        (cons (modify-result-from-language result ctx-language)
                              defined))
                       ((ctx-key . ctx-val)
                        ;; Notably we aren't passing ctx-key here because
                        ;; (I suppose) create-term-definition has the whole context
                        ;; and so can look it up anyway...
                        (receive (result defined)
                            (create-term-definition
                             result context ctx-key defined)
                          (cons result defined))))))
                 (cons result '()) ;; second value here is "defined"
                 (cdr context))))

             (loop
              (build-result)
              next-contexts remote-contexts))

            ;; 3.3: Anything else at this point is an error...
            (_ (throw 'json-ld-error
                      #:code "invalid local context"
                      #:context context)))))))

;; Algorithm 6.2

(define (create-term-definition active-context local-context term defined)
  ;; Let's see, has this term been defined, or started to be
  ;; defined yet?...
  (match (assq term defined)
    ((;; If term definition already was created, we do nothing
      ;; so return what we got!
      (_ . #t)
      (values active-context defined))
     (;; If term definition is false, that means term definition
      ;; started but never completed... a cycle!  Abort, abort!
      (_ . #f)
      (throw 'json-ld-error #:code "cyclic IRI mapping"))
     (;; Not referenced yet in defined, continue
      #f
      (let (;; Set defined's value for this key to false, indicating
            ;; that we started processing 
            (defined (acons term #f defined)))
        ;; TODO: Resume here at step 3, how do we see if something is a keyword?


        )))))
