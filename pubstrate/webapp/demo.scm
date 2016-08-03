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
              (href ,(prefix-url "/static/css/main.css")))))
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


(define (mockup-tmpl)
  (base-tmpl
   `(;; Input elements... TODO
     (div (@ (id "user-input")))
     ;; Main feed stuff
     (div
      (@ (id "main-feed-block")
         (class "main-feed-block"))
      (div (@ (class "feedish-tabs"))
           (div (@ (class "tab-active tab"))
                "Inbox")
           (div (@ (class "tab-look-at-me tab"))
                "Mentions")
           (div (@ (class "tab-idle tab"))
                "Direct")
           (div (@ (class "tab-look-at-me tab"))
                "Meanwhile"))
      (div (@ (class "feedish-content"))
           (div (@ (class "post-and-replies-wrapper"))
                (div (@ (class "feedish-top-post feedish-post"))
                     (div (@ (class "feedish-header"))
                          (div
                           (img (@ (class "feedish-user-avatar")
                                   (src "/static/images/red-ghostie.png")
                                   (alt "Red ghostie test"))))
                          (header (@ (class "feedish-header-right"))
                                  (h2 "New release of 8sync!")
                                  (div (@ (class "feedish-byline"))
                                       "By "
                                       (a (@ (href "http://dustycloud.org/"))
                                          "Christopher Allan Webber")
                                       " -- "
                                       (a (@ (href "/entry/293dc92a-1d5d-49dc-b3c4-b24ddc2feb9b/"))
                                          "April 22, 2016 @ 2:30pm"))))
                     (div (@ (class "feedish-entry-content"))
                          (p "Greetings, gentle citizens of the GNUiverse!  I'm happy to "
                             "announce 8sync 0.1, the very first release of "
                             (a (@ (href "http://gnu.org/s/8sync/"))
                                "8sync")
                             " an asynchronous programming library for "
                             (a (@ (href "http://gnu.org/s/guile/"))
                                "GNU Guile")
                             ". (I recently wrote a "
                             (a (@ (href "https://www.gnu.org/software/8sync/news/hello-world.html"))
                                "blogpost introducing 8sync")
                             " if that's of interest to you.) "
                             (a (@ (href "ftp://ftp.gnu.org/gnu/8sync/8sync-0.1.0.tar.gz"))
                                "Get 8sync here!")))
                     (div (@ (class "feedish-entry-buttons"))
                          (div (@ (class "like-plain"))
                               "♥ Like")
                          (div (@ (class "share-plain"))
                               "♻ Share")))))))))


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
