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

;; (define-module (pubstrate webapp demo)
;;   #:use-module (srfi srfi-9 gnu)
;;   #:use-module (srfi srfi-1)
;;   #:use-module (pubstrate asobj)
;;   #:use-module (pubstrate generics)
;;   #:use-module (pubstrate vocab)
;;   #:use-module (oop goops)
;;   #:use-module (web server)
;;   #:export (run-webapp))

(use-modules (srfi srfi-9 gnu))
(use-modules (srfi srfi-1))
(use-modules (pubstrate asobj))
(use-modules (pubstrate generics))
(use-modules (pubstrate vocab))
(use-modules (oop goops))
(use-modules (web server))
(use-modules ((system repl server)
              #:renamer (symbol-prefix-proc 'repl:)))

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

(define (index rqst)
  (respond (index-tmpl)))

(define (mockup rqst)
  (respond (mockup-tmpl)))

(define (user-page rqst user)
  'TODO)

(define (user-inbox rqst user)
  'TODO)

(define (user-outbox rqst username)
  (define (post-to-outbox oauth-user)
    'TODO)
  (define (read-from-outbox oauth-user)
    'TODO)
  'TODO)

(define (login rqst)
  'TODO)

(define (logout rqst)
  'TODO)

(define (content rqst user slug-or-id)
  'TODO)

(define (asobj rqst asobj-id)
  'TODO)

(define (oauth-authorize rqst)
  'TODO)



;;; Routing
;;; =======

(define (route request)
  'TODO)


;;; Storage stuff
;;; =============

(define-class <simple-storage> ()
  (asentry-store #:init-thunk make-hash-table))


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



;;; Application
;;; ===========

(define %user (make-parameter #f))
(define %db (make-parameter #f))

(define (webapp-server-handler request request-body)
  (values '((content-type . (text/plain)))
          "Hello World!"))

(define (run-webapp . args)
  (repl:spawn-server)
  (display "Hoo boy!\n")
  (run-server webapp-server-handler))
