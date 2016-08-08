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
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (pubstrate webapp routes)
  #:use-module (pubstrate webapp storage)
  #:use-module (pubstrate webapp params)
  #:use-module ((system repl server)
                #:renamer (symbol-prefix-proc 'repl:))
  #:export (run-webapp webapp-cli))

(define (webapp-server-handler request request-body)
  (receive (view args)
      (route request)
    (apply view request request-body args)))

(define (base-uri-from-other-params host port path)
  (build-uri 'http
             #:host (or host "localhost")
             #:port port
             #:path path))

(define %debug-store #f)
(define %debug-base-uri #f)

(define* (run-webapp #:key (storage (make <memory-store>))
                     (host #f)
                     (port 8080)
                     (base-uri (base-uri-from-other-params
                                host port "/"))
                     (announce #f))
  (define (maybe-kwarg kwarg val)
    (lambda (current-kwargs)
      (if val
          (append (list kwarg val)
                  current-kwargs)
          current-kwargs)))
  (display (string-append
            "Running on "
            (uri->string base-uri)
            "\n"))
  (let ((server-args
         ((compose
           (maybe-kwarg #:host host)
           (maybe-kwarg #:port port))
          '())))
    (set! %debug-store storage)
    (set! %debug-base-uri base-uri)
    (parameterize ((%store storage)
                   (%base-uri base-uri))
      (run-server (lambda args (apply webapp-server-handler args))
                  'http server-args))))

