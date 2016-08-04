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
  #:use-module (pubstrate webapp utils)
  #:use-module (pubstrate webapp params)
  #:export (base-tmpl index-tmpl mockup-tmpl))

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