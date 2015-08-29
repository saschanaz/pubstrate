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


(define (basic-deref-remote-context iri)
  (throw 'json-ld-error "remote resolver not implemented yet :)"))

(define* (maybe-append-uri-to-base uri #:optional base)
  "A sorta-correct way to join a URI to BASE, assuming there is a BASE,
and assuming URI isn't a URI on its own.

If not, it just returns URI.

Does the dumbest possible thing: string-appends together the base and URI.
This might not be the best way to do it, further reading into
  http://tools.ietf.org/html/rfc3986#section-5.1
should be done.
"
  (if (and base (not (string->uri uri)))
      (string-append base uri)
      uri))

(define* (update-context active-context local-context
                         #:optional (remote-contexts '())
                         ;; TODO: add #:base keyword argument option
                         #:key
                         (deref-context basic-deref-remote-context)
                         base-iri)
  (define (append-to-base uri)
    "Append to the current base (if appropriate)"
    (maybe-append-uri-to-base uri base-iri))

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
             (loop '(@) next-contexts remote-contexts))

            ;; Okay it's a string, great, that means it's an iri
            ((= append-to-base (? string? context))
             (if (member context remote-contexts)  ; TODO: append-to-base in the
                                                   ;   remote-contexts member check
                 (throw 'json-ld-error "recursive context inclusion"
                        #:context context))
             (let ((derefed-context (deref-context context)))
               (if (not (and (json-alist? derefed-context)
                             (json-ref derefed-context "@context")))
                   (throw 'json-ld-error "invalid remote context" context))
               ;; We made it this far, so recurse on the derefed context
               ;; then continue with that updated result
               (let* ((context derefed-context)
                      (result (update-context result context (cons context remote-contexts)
                                              #:deref-context deref-context)))
                 (loop result next-contexts remote-contexts))))

            ((? json-alist? context)
             ;; TODO: resume here at 3.4
             #f
             
             )

            ;; 3.3: Anything else at this point is an error...
            (_ (throw 'json-ld-error "invalid local context" #:context context)))))))
