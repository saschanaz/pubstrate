;;; coding: utf-8

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

(define-module (pubstrate webapp templates)
  #:use-module (srfi srfi-2)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate generics)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate webapp utils)
  #:use-module (pubstrate webapp params)
  ;; TODO: Move html parsing stuff into utils.html and remove this import
  #:use-module (htmlprag)
  #:export (base-tmpl index-tmpl mockup-tmpl
            user-homepage-tmpl

            login-tmpl

            asobj-page-tmpl
            toplevel-activity-tmpl))

(define* (base-tmpl body #:key title)
  (define (header-link link-name link-url)
    (list
     "[" `(a (@ (href ,link-url))
             ,link-name)
     "]"))
  `((doctype html)
    (head
     (meta (@ (charset "utf-8")))
     (title ,(if title
                 (string-append title " -- Pubstrate")
                 "Pubstrate"))
     ;; css
     (link (@ (rel "stylesheet")
              (href ,(local-uri "static" "css" "main.css")))))
    (body
     (div (@ (id "main-wrapper"))
          (header (@ (id "site-header"))
                  ;; @@: Not semantic naming!
                  (span (@ (id "site-header-left-stuff"))
                        (b (a (@ (href ,(local-uri)))
                              "*pubstrate*")))
                  (span (@ (id "site-header-right-stuff"))
                        ,@(if (%user)
                              (list "Hello, " (user-username (%user)) "!"
                                    ;; TODO: Add [inbox] [mentions] [direct] [meanwhile]
                                    " :: "
                                    (header-link "Log out" (local-uri "logout")))
                              (list (header-link "Log in" (local-uri "login"))))))
          (div (@ (id "site-main-content"))
               ,body))
     (div (@ (id "site-footer"))
          "Copyright notice goes here!"))))

(define (index-tmpl)
  (base-tmpl "Beep boop, hello there!"))

(define (user-tmpl user)
  (base-tmpl
   `(p "Hi!  This is "
       ,(user-username user)
       "'s page.")))

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


(define (user-homepage-tmpl user activities prev-url next-url)
  (base-tmpl
   `(div (@ (class "generic-content-box"))
         (p "Hi!  This is "
            ,(or (asobj-ref user "name")
                 (asobj-ref user "preferredUsername"))
            "'s page.")
         (hr)
         ,@(map
            (lambda (activity)
              `(div (@ (class "post-and-replies-wrapper"))
                    ,(toplevel-activity-tmpl activity)))
            activities))))


(define (login-tmpl)
  (base-tmpl
   `(div (@ (class "generic-content-box"))
         (h1 "Log in:")
         (form (@ (action ,(local-uri "login"))
                  (method "POST")
                  (enctype "application/x-www-form-urlencoded"))
               (table
                (tr (th "Username")
                    (td (input (@ (type "text")
                                  (name "username")))))
                (tr (th "Password")
                    (td (input (@ (type "password")
                                  (name "password")))))
                (tr (td)  ; empty cell
                    (td (button (@ (type "submit"))
                                "Submit"))))))))



;;; Generic methods for displaying different types of content
;;; =========================================================

(define (asobj-page-tmpl asobj)
  (base-tmpl
   `(div
     ;; Input elements... TODO
     (div (@ (id "user-input")))
     ;; Main feed stuff
     (div (@ (id "main-feed-block")
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
                    ,(toplevel-activity-tmpl asobj)))))))

(define (render-if exp)
  (if exp (list exp) '()))

(define (render-inline-if exp)
  (if exp exp '()))


;; @@: Maybe rename to display-activity?
(define-as-generic toplevel-activity-tmpl
  "Render an activitystreams object in HTML.
Arguments: (asobj)")

(define-as-method (toplevel-activity-tmpl (asobj ^Create))
  `(div (@ (class "feedish-top-post feedish-post"))
        ;; Render header
        ,(asobj-header-tmpl asobj)
        ;; Render object
        ,(inline-asobj-tmpl (asobj-ref asobj "object"))
        ;; Render buttons
        (div (@ (class "feedish-entry-buttons"))
             (div (@ (class "like-plain"))
                  "♥ Like")
             (div (@ (class "share-plain"))
                  "♻ Share")
             (div (@ (class "reply-plain"))
                  "⌨ Reply"))))


(define-as-generic asobj-header-tmpl
  "Display a header in a toplevel asobj")

;; @@: maybe this should just be for ^Create?  Not clear yet!
(define-as-method (asobj-header-tmpl (asobj ^Object))
  (let* ((actor (asobj-ref asobj "actor"))
         (actor-name (or (asobj-ref actor "preferredUsername")
                         (asobj-ref actor "name")
                         (asobj-ref actor "id")))
         (title (and-let* ((object (asobj-ref asobj "object"))
                           (name (asobj-ref object "name")))
                  `(h2 ,name)))
         ;; TODO: should either use the internal date,
         ;;  or we should use our own provided date
         (when-posted "April 22, 2016 @ 2:30pm"))
    `(div (@ (class "feedish-header"))
          ;; TODO
          ;; Avatar (and maybe username?)
          (div
           (img (@ (class "feedish-user-avatar")
                   (src "/static/images/red-ghostie.png")
                   (alt "Red ghostie test"))))
          ;; Information about this post
          (header (@ (class "feedish-header-right"))
                  ,@(render-if title)
                  (div (@ (class "feedish-byline"))
                       (b "By: ")
                       (a (@ (href ,(asobj-ref actor "id")))
                          ,actor-name)
                       (b " At: ")
                       (a (@ (href ,(asobj-ref asobj "id")))
                          ,when-posted))))))


(define-as-generic inline-asobj-tmpl
  "Render an <asobj> inline, probably from another activity.")

;; @@: Should this be Note or Object?  Or do things render "like a note"
;;  by default?  What else might render this way?
(define-as-method (inline-asobj-tmpl (asobj ^Object))
  (let ((content-html
         ;; TODO: Need to sanitize this...
         (and-let* ((content (asobj-ref asobj "content")))
           (cdr (html->shtml content)))))
    `(div (@ (class "feedish-entry-content"))
          ,@(render-if content-html))))
