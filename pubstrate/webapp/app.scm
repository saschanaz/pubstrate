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
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (gcrypt hmac)
  #:use-module (webutils sessions)
  #:use-module (srfi srfi-34)
  #:use-module (pubstrate config)
  #:use-module (pubstrate webapp conditions)
  #:use-module (pubstrate webapp http-status)
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

(define %server-error-text
  "\
****************
* SERVER ERROR *
****************")

(define %user-error-text
  "\
**************
* USER ERROR *
**************")

(define (webapp-server-handler request request-body)
  (define (respond-error header message code)
    (values (build-response
             #:code code
             #:headers '((content-type text/plain)))
            (format #f "~a\n\n~a ~a\nReason: ~a"
                    header code (or (status-ref code) "???")
                    (or message "Not given."))))

  (receive (view args)
      (route request)
    (catch
     #t
     (lambda ()
       (with-extended-ctx
        (ctx-vars-from-request request)
        (lambda ()
          (guard (condition
                  ((server-error? condition)
                   (respond-error %server-error-text
                                  ;; (error-message condition)
                                  #f ; maybe we shouldn't leak server error reasons?
                                  (error-code condition)))
                  ((user-error? condition)
                   (respond-error %user-error-text
                                  (error-message condition)
                                  (error-code condition))))
            (apply view request request-body args)))))
     ;; Default 500 error handler
     (lambda _
       (respond-error %server-error-text
                      "An unexpected error occured."
                      500))
     ;; Print error to port
     (let ((err (current-error-port)))
       (lambda (key . args)
         (false-if-exception
          (let ((stack (make-stack #t 4)))
            (display-backtrace stack err)
            (print-exception err (stack-ref stack 0)
                             key args))))))))

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
