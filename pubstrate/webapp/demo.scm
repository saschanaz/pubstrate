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

(define-module (pubstrate webapp demo)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-1)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate generics)
  #:use-module (pubstrate paths)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate contrib html)
  #:use-module (pubstrate contrib mime-types)
  #:use-module (pubstrate webapp templates)
  #:use-module (pubstrate webapp utils)
  #:use-module (sxml simple)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module ((system repl server)
                #:renamer (symbol-prefix-proc 'repl:))
  #:export (run-webapp))

(define-as-generic asobj-gallery-tmpl
  "Template for showing an asobj in a gallery")

(define-as-method (asobj-gallery-tmpl (asobj ^Object))
  'foo)


;;; Views
;;; =====

(define (index request body)
  (respond-html (index-tmpl)))

(define (mockup request body)
  (respond-html (mockup-tmpl)))

(define (user-page request body user)
  'TODO)

(define (user-inbox request body user)
  'TODO)

(define (user-outbox request body username)
  (define (post-to-outbox oauth-user)
    'TODO)
  (define (read-from-outbox oauth-user)
    'TODO)
  'TODO)

(define (login request body)
  'TODO)

(define (logout request body)
  'TODO)

(define (content request body user slug-or-id)
  'TODO)

(define (asobj request body asobj-id)
  'TODO)

(define (oauth-authorize request body)
  'TODO)


(define (standard-four-oh-four request body)
  ;; TODO: Add 404 status message
  (values '((content-type . (text/plain)))
          "Not found!"))

;;; Static site rendering... only available on devel instances (hopefully!)
(define (render-static request body static-path)
  (respond
   (call-with-input-file (web-static-filepath static-path) get-bytevector-all)
   #:content-type (mime-type static-path)))



;;; Routing
;;; =======

(define (route request)
  (match (pk 'path (split-and-decode-uri-path (uri-path (request-uri request))))
    (() (values index '()))
    (("static" static-path ...)
     ;; TODO: make this toggle'able
     (values render-static
             (list (string-append "/" (string-join
                                       static-path "/")))))
    (("mockup")
     (values mockup '()))
    ;; Not found!
    (_ (values standard-four-oh-four '()))))



;;; Application
;;; ===========

(define (webapp-server-handler request request-body)
  (receive (view args)
      (route request)
    (apply view request request-body args)))

(define (run-webapp . args)
  (repl:spawn-server)
  (display "Hoo boy!\n")
  (run-server (lambda args (apply webapp-server-handler args))))
