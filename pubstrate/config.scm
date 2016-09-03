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

(define-module (pubstrate config)
  #:use-module (pubstrate paths)
  #:use-module (pubstrate webapp store)
  #:use-module (srfi srfi-1)  ; list lib
  #:use-module (srfi srfi-9)  ; records
  #:use-module (ice-9 format)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (;; Methods for creating configuration specifications
            make-config-spec make-config-spec*

            ;; Users will use this to actually configure their
            ;; application
            configure

            ;; Loading, referencing, printing configs
            load-config config-ref config-pprint

            ;; @@: Should we export <spec-var> and friends?
            ;;   Is it really useful?
            <spec-var> spec-var
            spec-var? spec-var-name spec-var-desc
            spec-var-validate spec-var-default

            ;; Not sure we need to export this either.
            <loaded-config> loaded-config?
            loaded-config-fields loaded-config-spec))


(define-record-type <spec-var>
  (make-spec-var name desc validate default)
  spec-var?
  (name spec-var-name)
  (desc spec-var-desc)
  (validate spec-var-validate)
  (default spec-var-default))

(define *no-default*
  (cons '*no* '*default*))

(define* (spec-var name desc
                  #:key (validate (const #t))
                  default)
  "Define the specification of a configuration variable.

NAME should be a symbol, and DESC is a string describing what this
variable means.  (It's a good idea to be specific if any specific type
is expected.)

VALIDATE is a predicate which takes one argument, a variable provided
by a user.  If a user does not provide a value, DEFAULT is instead
called, which is a procedure accepting one argument of the entire
user-supplied configuration as an alist.  (If you just want to always
provide a single default without any other context, you can use the
`const' procedure.)

Note that any procedure defined without a default will be considered
mandatory.  If you don't want a configuration option to be mandatory,
provide a DEFAULT!"
  (make-spec-var name desc validate default))

(define (make-config-spec spec-items)
  (fold
   (match-lambda*
     (((name rest ...) prev)
      (vhash-consq name (apply spec-var name rest)
                   prev)))
   vlist-null
   spec-items))

(define-syntax-rule (make-config-spec* (name rest ...) ...)
  (make-config-spec
   (list (list (quote name) rest ...) ...)))

(define-syntax-rule (configure (key val) ...)
  "Define configuration with a list like:

  (configure
   (key \"val\")
   (another-key 22))

This really just produces an alist, but it's a bit easier to both
read and pretty-print if generating a config file for a user."
  (list (cons (quote key) val) ...))

;;; Wrapped in a record type for abstraction's sake, since
;;; we might not keep things as a vhash forever...
(define-record-type <loaded-config>
  (make-loaded-config fields spec)
  loaded-config?
  (fields loaded-config-fields)
  (spec loaded-config-spec)) ; not sure this is useful but why not

;; @@: Probably not the absolutely cleanest implementation, but
;;   should be more than fast enough, and works.
(define (load-config user-config spec)
  "Load and validate USER-CONFIG (an alist, possibly produced through
the `configure' procedure) against SPEC.  Builds a vhash using vhash-consq.
This will also handle all defaults."
  ;; We do this *first* because if there are fields that absolutely
  ;; are mandatory and are not handled, we need to error out early
  ;; so that defaults with look at config won't try to access
  ;; a value they need which isn't there.
  (define keys-in-user-config
    ;; Build a list of all keys in user config..
    ;; also make sure we aren't missing any critical
    ;; defaults
    (let ((table (make-hash-table)))
      ;; Set up keys-in-user-config
      (for-each
       (match-lambda
         ((key . val)
          (hash-set! table key #t)))
       user-config)
      ;; Validate we aren't missing anything mandatory
      (vhash-fold
       (lambda (key spec-var prev)
         (if (and (not (spec-var-default spec-var))
                  (not (hash-ref table key)))
             ;; if there's no default, it's mandatory
             (throw 'config-error
                    'mandatory-field
                    "Field was mandatory but is not provided"
                    #:key key)
             ;; value we pass back doesn't matter
             #f))
       #f spec)
      ;; Return table
      table))
  (define initial-loaded-config
    (fold
     (match-lambda*
       (((key . val) prev)
        (let* ((spec-var-match (vhash-assq key spec))
               (spec-var (and=> spec-var-match cdr)))
          (cond
           ;; Unknown... ignore but warn
           ((not spec-var-match)
            (format (current-error-port)
                    "WARNING: Ignoring unknown config field: ~s\n"
                    key)
            prev)
           ;; Seems legit... add to keys-in-user-config
           ;; as well as the config we're building up
           (((spec-var-validate spec-var) val)
            (hashq-set! keys-in-user-config key #t)
            (vhash-consq key val prev))
           ;; Otherwise it didn't validate... throw an error
           (else
            (throw 'config-error
                   'invalid-var
                   "Invalid value for config field"
                   #:key key #:val val #:spec-var spec-var))))))
     vlist-null user-config))
  (define keys-unhandled
    (vhash-fold
     (lambda (key val prev)
       (if (not (hash-ref keys-in-user-config key))
           (cons key prev)
           prev))
     '() spec))
  ;; Produce final loaded config
  (define loaded-config
    (fold
     (lambda (key prev)
       (let* ((spec-var (cdr (vhash-assq key spec)))
              (default-proc (spec-var-default spec-var)))
         (if default-proc
             ;; There's a default provided, so give it
             ;; the config and the result will be the value
             (vhash-consq key (default-proc user-config) prev)
             ;; No default?  Uhoh, it was mandatory
             (throw 'config-error
                    'mandatory-field
                    "Field was mandatory but is not provided"
                    #:key key))))
     initial-loaded-config keys-unhandled))
  ;; And return it!
  (make-loaded-config loaded-config spec))

(define* (config-ref loaded-config key #:key default)
  "Extract a variable from a LOADED-CONFIG."
  (match (vhash-assq key (loaded-config-fields loaded-config))
    (#f default)
    ((key . val) val)))

(define* (config-pprint loaded-config
                        #:optional (port (current-output-port)))
  (pretty-print (vhash-fold
                 (lambda (key val prev)
                   (cons (list key val)
                         prev))
                 '() (loaded-config-fields loaded-config))
                port))
