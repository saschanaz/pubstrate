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

(define* (update-context active-context local-context
                         #:optional (remote-contexts '())
                         #:key
                         (deref-context basic-deref-remote-context)
                         base-iri)
  (define (append-to-base uri)
    "Append to the current base (if appropriate)"
    (maybe-append-uri-to-base uri base-iri))

  (define (equal-including-checking-base? uri1 uri2)
    "A check method to see if A matches B, or B with base apended"
    (or (equal? uri1 uri2)
        (equal? uri1 (append-to-base uri2))))

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
                      (result (update-context result context (cons context remote-contexts)
                                              #:deref-context deref-context)))
                 (loop result next-contexts remote-contexts))))

            ((? json-alist? context)
             ;; TODO: this should NOT be a cond here... because
             ;; we might do multiple things here for @base @vocab and @language
             ;; It looks like 4, 5, and 6 are just adjusting the result.
             ;; So we could instead making them into mini-functions we
             ;; then chain together.
             ;;
             ;; So we should use these functions in combination with a fold...

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

             ;; Fold goes here, and fold feeds into loop

             )

            ;; 3.3: Anything else at this point is an error...
            (_ (throw 'json-ld-error
                      #:code "invalid local context"
                      #:context context)))))))
