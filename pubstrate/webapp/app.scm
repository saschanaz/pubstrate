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
  #:use-module (web request)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (gcrypt hmac)
  #:use-module (webutils sessions)
  #:use-module (pubstrate config)
  #:use-module (pubstrate webapp routes)
  #:use-module (pubstrate webapp db)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp config)
  #:use-module (pubstrate webapp ctx)
  #:use-module ((system repl server)
                #:renamer (symbol-prefix-proc 'repl:))
  #:export (run-webapp

            repl-set-ctx!

            with-app-ctx-from-config with-app-ctx-from-config-file
            set-app-ctx-from-config! app-ctx-clean-up!))

(define (ctx-vars-from-request request)
  (define session
    (session-data (ctx-ref 'session-manager) request))
  (define db
    (ctx-ref 'db))
  ;; Get the user based on session data, if there
  (define (session-user)
    (and=> (assoc-ref session 'user)
           (lambda (username)
             (db-user-ref db username))))
  (define (api-user)
    (match (assoc-ref (request-headers request) 'authorization)
      (('bearer . (? string? token))
       (and=> (db-bearer-entry-ref db token)
              (lambda (entry)
                (db-asobj-ref db (bearer-entry-user-id entry)))))
      (_ #f)))
  (define user
    (or (session-user)
        (api-user)))
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
  (define (init-from-config key)
    (match (config-ref config key)
      ((db-proc . db-args)
       (apply db-proc db-args))))
  (define db (init-from-config 'db))
  (define filestore (init-from-config 'filestore))
  `((db . ,db)
    (filestore . ,filestore)
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
  (db-close (ctx-ref 'db)))

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
           (maybe-kwarg #:port port))
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
