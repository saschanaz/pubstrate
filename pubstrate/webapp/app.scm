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

(define-module (pubstrate webapp app)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (pubstrate config)
  #:use-module (pubstrate crypto)
  #:use-module (pubstrate webapp routes)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp sessions)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp config)
  #:use-module (pubstrate webapp ctx)
  #:use-module ((system repl server)
                #:renamer (symbol-prefix-proc 'repl:))
  #:export (run-webapp

            with-app-ctx-from-config with-app-ctx-from-config-file
            set-app-ctx-from-config! app-ctx-clean-up!))

(define (ctx-vars-from-request request)
  (define session
    (session-data (ctx-ref 'session-manager) request))
  ;; Get the user based on session data, if there
  (define user
    (and=> (assoc-ref session 'user)
           (lambda (username)
             (store-user-ref (ctx-ref 'store)
                             username))))
  `((user . ,user)))

(define (webapp-server-handler request request-body)
  (receive (view args)
      (route request)
    (with-extended-ctx
     (ctx-vars-from-request request)
     (lambda ()
       (apply view request request-body args)))))

(define (base-uri-from-other-params host port path)
  (build-uri 'http
             #:host (or host "localhost")
             #:port port
             #:path path))

(use-modules (ice-9 vlist))
(define %debug-ctx vlist-null)

(define (repl-set-ctx!)
  "For debugging/hacking purposes, set %ctx to %debug-ctx"
  (%ctx %debug-ctx))

(define (app-ctx-from-config config)
  (define store
    (match (config-ref config 'store)
      ((store-proc . store-args)
       (apply store-proc store-args))))
  `((store . ,store)
    (base-uri . ,(config-ref config 'base-uri))
    ;; TODO: signing-key-path
    (session-manager . ,(make-session-manager (gen-signing-key))))  )

(define (set-app-ctx-from-config! config)
  "Set the %ctx parameter based on the application context built from CONFIG

Only for debugging / REPL hacking!  Use with-app-ctx-from-config for real code."
  (ctx-extend! (app-ctx-from-config config)))

(define (set-app-ctx-from-config-file! config-file)
  "Set the %ctx parameter based on the application context built from CONFIG

Only for debugging / REPL hacking!  Use with-app-ctx-from-config for real code."
  (set-app-ctx-from-config! (load-config (load config-file)
                                         pubstrate-config-spec)))

(define (app-ctx-clean-up!)
  (store-close (ctx-ref 'store)))

(define (with-app-ctx-from-config config thunk)
  "Evaluate THUNK in application context built from CONFIG"
  (with-extended-ctx
   (app-ctx-from-config config)
   (lambda ()
     (dynamic-wind
       (const #t)  ; no-op in-guard
       thunk
       (lambda ()  ; clean up
         (app-ctx-clean-up!))))))

(define (with-app-ctx-from-config-file config-file thunk)
  "Like `with-app-ctx-from-config' but load config-file's config"
  (with-app-ctx-from-config (load-config (load config-file)
                                         pubstrate-config-spec)
                            thunk))

(define* (run-webapp config
                     #:key (host #f) (port 8080)
                     (announce #f))
  (define (maybe-kwarg kwarg val)
    (lambda (current-kwargs)
      (if val
          (append (list kwarg val)
                  current-kwargs)
          current-kwargs)))
  (let ((server-args
         ((compose
           (maybe-kwarg #:host host)
           (maybe-kwarg #:port (and=> port string->number)))
          '())))
    (with-app-ctx-from-config
     config
     (lambda ()
       (set! %debug-ctx (%ctx))
       (display (string-append
                 "Running on "
                 (uri->string (ctx-ref 'base-uri))
                 "\n"))
       (run-server (lambda args (apply webapp-server-handler args))
                   'http server-args)))))
