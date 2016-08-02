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
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-1)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate generics)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate contrib html)
  #:use-module (oop goops)
  #:use-module (sxml simple)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:export (run-webapp))

(define-as-generic asobj-gallery-tmpl
  "Template for showing an asobj in a gallery")

(define-as-method (asobj-gallery-tmpl (asobj ^Object))
  'foo)


;;; Templates
;;; =========

(define* (base-tmpl body #:key title)
  `((doctype html)
    (head
     (meta (@ (charset "utf-8")))
     (title ,(if title
                 (string-append title " -- Pubstrate")
                 "Pubstrate"))
     ;; css
     (link (@ (rel "stylesheet")
              (href ,(prefix-url "/css/main.css")))))
    (body
     (div (@ (id "main-wrapper"))
          (header (@ (id "site-header"))
                  ;; @@: Not semantic naming!
                  (span (@ (id "site-header-left-stuff"))
                        (a (@ (href ,(prefix-url "/")))
                           "Pubstrate"))
                  (span (@ (id "site-header-right-stuff"))
                        ,@(if (%user)
                              (list "Hello, " (user-username (%user)) "!")
                              (list
                               "[" `(a (@ (href ,(prefix-url "/logout/")))
                                       "Log out")
                               "]"))))
          (div (@ (id "site-main-content"))
               ,body))
     (div (@ (id "site-footer"))
          "Copyright notice goes here!"))))

(define (index-tmpl)
  (base-tmpl "Beep boop, hello there!"))



;;; Views
;;; =====

(define (index request body)
  (respond-html (index-tmpl)))

(define (mockup request body)
  (respond (mockup-tmpl)))

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


;;; Routing
;;; =======

(define (route request)
  (match (pk 'path (split-and-decode-uri-path (uri-path (request-uri request))))
    (() (values index '()))
    ((static static-path ...)
     ;; TODO: make this toggle'able
     (values render-static (list static-path)))
    ;; Not found!
    (_ (values standard-four-oh-four '()))))


;;; Storage stuff
;;; =============

(define-class <simple-storage> ()
  (asentry-store #:init-thunk make-hash-table))

(define-generic storage-save-asentry)
(define-generic storage-get-asentry)

(define (storage-get-asentry-fat store id)
  'TODO)
(define (storage-save-asentry-lean store asentry)
  'TODO)



;;; <asentry>
;;; =========

(define-immutable-record-type <asentry>
  (make-asentry asobj private)
  asentry?
  (asobj asentry-asobj set-asentry-asobj)
  ;; Private data is sjson
  (private asentry-private set-asentry-private))



;;; Utils and misc
;;; ==============

(define (prefix-url url)
  ;; TODO!
  url)


(define* (respond #:optional body #:key
                  (status 200)
                  (content-type-params '((charset . "utf-8")))
                  (content-type 'text/html)
                  (extra-headers '()))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          body))

(define (respond-html sxml . respond-args)
  (apply respond (lambda (port)
                   (sxml->html sxml port))
         respond-args))


;;; Application
;;; ===========

(define %user (make-parameter #f))
(define %db (make-parameter #f))

(define (webapp-server-handler request request-body)
  (receive (view args)
      (route request)
    (apply view request request-body args)))

(define (run-webapp . args)
  (repl:spawn-server)
  (display "Hoo boy!\n")
  (run-server (lambda args (apply webapp-server-handler args))))
