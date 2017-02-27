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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (sjson utils)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate generics)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate date)
  #:use-module (pubstrate webapp template-utils)
  #:use-module (pubstrate webapp form-widgets)
  #:use-module (pubstrate webapp utils)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp ctx)

  ;; TODO: Move html parsing stuff into utils.html and remove this import
  #:use-module (htmlprag)
  #:export (base-tmpl index-tmpl mockup-tmpl
            user-homepage-tmpl

            generic-content-tmpl
            centered-content-tmpl

            login-tmpl

            asobj-page-tmpl
            toplevel-activity-tmpl))


(define* (base-tmpl body #:key title)
  (define (header-link link-name link-url)
    (list
     "[" `(a (@ (href ,link-url))
             ,link-name)
     "]"))
  (define username
    (and=> (ctx-ref 'user)
           (lambda (u)
             (asobj-ref u "preferredUsername"))))
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
                        ,@(if (ctx-ref 'user)
                              (list "Hello, "
                                    `(a (@ (href ,(local-uri "u" username)))
                                        ,(asobj-ref (ctx-ref 'user) "name"))
                                    "!"
                                    " :: "
                                    ;; TODO: Add [inbox] [mentions] [direct] [meanwhile]
                                    (header-link "inbox"
                                                 (local-uri "u" username "inbox"))
                                    " :: "
                                    (header-link "Log out" (local-uri "logout")))
                              (list (header-link "Log in" (local-uri "login"))))))
          (div (@ (id "site-main-content"))
               ,body))
     (div (@ (id "site-footer"))
          (a (@ (href "https://gitlab.com/dustyweb/pubstrate"))
             "Pubstrate")
          " is released under the "
          (a (@ (href "https://www.gnu.org/copyleft/gpl.html"))
             "GNU General Public License")
          ", version 3 or later."))))


;;; Individual pages
;;; ================

(define (index-tmpl)
  (base-tmpl
   (generic-content-tmpl
    `(div (h1 "Welcome to Pubstrate!")
          (p (a (@ (href "https://gitlab.com/dustyweb/pubstrate"))
                "Pubstrate")
             " is a decentralized social networking application "
             "built on top of the "
             (a (@ (href "https://www.w3.org/TR/activitypub/"))
                "ActivityPub")
             " federation protocol.")))))

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


(define (generic-content-tmpl . content)
  `(div (@ (class "generic-content-box"))
        ,@content))

(define* (centered-content-tmpl . content)
  `(div (@ (class "simple-centered-wrap"))
        ,(apply generic-content-tmpl content)))

(define (user-homepage-tmpl user activities prev-url next-url)
  (base-tmpl
   `(div (@ (class "generic-content-box"))
         (div (@ (class "pre-content-header"))
              (p "Hi!  This is "
                 ,(or (asobj-ref user "name")
                      (asobj-ref user "preferredUsername"))
                 "'s page."))
         ,@(map
            (lambda (activity)
              `(div (@ (class "post-and-replies-wrapper"))
                    ,(toplevel-activity-tmpl activity)))
            activities))))



(define* (login-tmpl login-form #:key next try-again)
  (base-tmpl
   (centered-content-tmpl
    '(h1 "Log in:")
    (if try-again
        '(em "Sorry, try again.")
        '())
    `(form (@ (action ,(local-uri "login"))
              (method "POST")
              (enctype "application/x-www-form-urlencoded"))
           ,@(render-if next
                        `(input (@ (name "next")
                                   (type "hidden")
                                   (value ,next))))
           (table
            ,(form-render-table login-form)
            (tr (td)  ; empty cell
                (td (button (@ (type "submit"))
                            "Submit"))))))
   #:title "Login"))



;;; Generic methods for displaying different types of content
;;; =========================================================

(define* (asobj-page-tmpl asobj #:key title)
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
                    ,(if title
                         `(h2 ,title)
                         '())
                    ,(toplevel-activity-tmpl asobj)))))))


;; @@: Maybe rename to display-activity or render-activity?
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

(define-as-method (toplevel-activity-tmpl (asobj ^Collection))
  (define first
    (match (asobj-ref asobj "first")
      ;; if itself an asobj, then that's good enough
      ((? asobj? asobj) asobj)
      ;; if it's an id, we need to look it up maybe?
      ((? string? id)
       'TODO)))
  `(div (@ (class "collection"))
        ,(toplevel-activity-tmpl first)))

(define-as-method (toplevel-activity-tmpl (asobj ^CollectionPage))
  (define items
    (or (asobj-ref asobj "orderedItems")
        (asobj-ref asobj "items")))

  `(div (@ (class "collection-page"))
        ,(cond
          ((eq? items '())
           '(i "There doesn't seem to be anything here."))
          (else
           (map (lambda (item)
                  `(div (@ (class "post-and-replies-wrapper"))
                        ,(toplevel-activity-tmpl
                          item)))
                items)))
        ;; TODO: Put navigation here.
        ))


(define-as-method (toplevel-activity-tmpl (asobj ^Activity))
  `(div (@ (class "feedish-top-post feedish-post"))
        "Unknown activity type!"))

(define-as-method (toplevel-activity-tmpl (asobj ^Object))
  (toplevel-activity-tmpl (make-as ^Create (%default-env)
                                   #:object asobj)))

(define-as-method (toplevel-activity-tmpl (asobj ^Delete))
  (let* ((actor (or (asobj-ref asobj "actor")
                    (asobj-ref asobj '("object" "attributedTo"))))
         (actor-name (or (and actor (or (asobj-ref actor "preferredUsername")
                                        (asobj-ref actor "name")
                                        (asobj-ref actor "id")))
                         "???"))
         (when-posted
          (and=> (asobj-ref asobj "published")
                 (lambda (pub-str)
                   (date->string
                    (rfc3339-string->date pub-str)
                    "~b ~d, ~Y @ ~r")))))
    `(div (@ (class "feedish-top-post feedish-post"))
          (p (i "Post deleted by " ,actor-name
                ,@(if when-posted
                      `(" on " ,when-posted ".")
                      "."))))))

(define-as-method (toplevel-activity-tmpl (asobj ^Tombstone))
  (let* ((deleted-date (and=> (asobj-ref asobj "deleted")
                              rfc3339-string->date))
         (deleted-date-str (and deleted-date
                                (date->string deleted-date
                                              "~b ~d, ~Y @ ~r")))
         (yr (or (and deleted-date
                      (date->string deleted-date
                                    "~Y"))
                 "????")))
    `(div (@ (class "feedish-top-post feedish-post"))
          (pre (@ (class "tombstone-ascii"))
               ,(string-append
                 "              *                     *
 *                     *                  __     *
      *                     *            <  '.
             *                            )  )       *
                                   *     <__-'   *
    *      *       .-------------.
                 .'               '.                *
       *         |                 |   *
                 |   TOMB OF THE   |       *
            *    |     UNKNOWN     |            *
   *             | ACTIVITYSTREAMS |
                 |     OBJECT      |
             .^. |                 |
  _  .+.  _  |~| |    ????-"yr"    |  .+. .-.  _  .-.
 | | |~| |=| | | |                 |  |=| |~| | | | |
``'`'`''``'`'`'`'``'``'`'`''``'`'`'`'``'`''``''``'`'`'"))
          (p (i ,(if deleted-date-str
                     (string-append "This object was deleted on "
                                    deleted-date-str
                                    ".")
                     "This object has been deleted."))))))




(define-as-generic asobj-header-url
  "URL to show in the header of an activitystreams object")

(define-as-method (asobj-header-url (asobj ^Object))
  (or (asobj-ref asobj "url")
      (asobj-id asobj)))

(define-as-method (asobj-header-url (asobj ^Create))
  (or (asobj-ref asobj '("object" "url"))
      (asobj-ref asobj '("object" "id"))
      (asobj-ref asobj "url")
      (asobj-id asobj)))

(define-as-generic asobj-header-tmpl
  "Display a header in a toplevel asobj")

(define-as-method (asobj-header-tmpl (asobj ^Object))
  (let* ((actor (or (asobj-ref asobj "actor")
                    (asobj-ref asobj '("object" "attributedTo"))))
         (actor-name (or (and actor (or (asobj-ref actor "preferredUsername")
                                        (asobj-ref actor "name")
                                        (asobj-ref actor "id")))
                         "???"))
         (title (and-let* ((object (asobj-ref asobj "object"))
                           (name (asobj-ref object "name")))
                  `(h2 ,name)))
         ;; TODO: should either use the internal date,
         ;;  or we should use our own provided date
         (when-posted
          (and=> (asobj-ref asobj "published")
                 (lambda (pub-str)
                   (date->string
                    (rfc3339-string->date pub-str)
                    "~b ~d, ~Y @ ~r"))))

         (tags (filter
                asobj?
                (append (maybe-listify (asobj-ref asobj "tag" '()))
                        (maybe-listify (asobj-ref asobj '("object" "tag")
                                                  '())))))
         (header-entry
          (lambda (key content)
            `(div (@ (class "feedish-header-entry"))
                  (b (@ (class "header-key"))
                     ,(string-append key ": "))
                  (span (@ (class "header-content"))
                        ,content))))
         
         ;; TODO: We should probably make instrument display a generic
         ;;    because we might do it a bit differently depending on what
         ;;    kind used
         (instrument
          (or (asobj-ref asobj "instrument")
              (asobj-ref asobj '("object" "instrument"))))
         (instrument-name
          (and instrument
               (or (asobj-ref instrument "name")
                   (asobj-ref instrument "url")
                   (asobj-ref instrument "href")
                   (asobj-ref instrument "id"))))
         (instrument-href
          (and instrument-name
               (or (asobj-ref instrument "href")
                   (asobj-ref instrument "url")
                   (asobj-ref instrument "href")
                   (asobj-ref instrument "id"))))

         (write-tag
          (lambda (tag)
            (let* ((mention? (asobj-is-a? tag ^Mention))
                   (link (or (asobj-ref tag "href")
                             (asobj-ref tag "url")
                             (asobj-ref tag "id")))
                   (name (string-append
                          (if mention?
                              "@" "#")
                          (or (asobj-ref tag "name")
                              ;; whuh
                              "*"))))
              `(span (@ (class ,(if mention?
                                    "header-tag-mention"
                                    "header-tag-plain")))
                     ,(if link
                          `(a (@ (href ,link))
                              ,name)
                          name))))))
    `(div (@ (class "feedish-header"))
          ;; TODO
          ;; Avatar (and maybe username?)
          (div
           (img (@ (class "feedish-user-avatar")
                   (src "/static/images/red-ghostie.png")
                   (alt "Red ghostie test"))))
          ;; Information about this post
          (header (@ (class "feedish-header-right"))
                  ,@(maybe-render title)
                  ,(header-entry
                    "By" (if actor
                             `(a (@ (href ,(asobj-ref actor "id")))
                                 ,actor-name)
                             "???"))
                  ,(header-entry
                    "At" `(a (@ (href ,(asobj-header-url asobj)))
                             ,when-posted))
                  ,@(render-if
                     instrument-name
                     (header-entry
                      "Posted via" (if instrument-href
                                       `(a (@ (href ,instrument-href))
                                           ,instrument-name)
                                       instrument-name)))
                  ,@(render-if
                     (not (eq? tags '()))
                     (header-entry
                      "Tags" (list-intersperse (map write-tag tags)
                                               `(span (@ (class "tag-sep"))
                                                      ", "))))))))



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
          ,@(maybe-render content-html))))

(define-as-method (inline-asobj-tmpl (asobj ^Video))
  (let* ((content-html
          ;; TODO: Need to sanitize this...
          (and-let* ((content (asobj-ref asobj "content")))
            (cdr (html->shtml content))))
         (video-links (maybe-listify (asobj-ref asobj "url" '())))
         (source-links
          (delete
           #f (map (match-lambda
                     ((? string-uri? href)
                      `(source (@ (src ,href))))
                     ((and (? asobj? _) (? (cut asobj-is-a? <> ^Link) link))
                      (match (asobj-ref link "href")
                        ((? string-uri? href)
                         (let ((media-type (asobj-ref link "mediaType"))
                               (maybe-add-prop (lambda (prop-name var)
                                                 (if var
                                                     `((,prop-name ,var))
                                                     '()))))
                           `(source (@ (src ,href)
                                       ,@(if media-type
                                             `((type ,media-type))
                                             '())))))
                        (_ #f))))
                   video-links))))
    `(div (@ (class "feedish-entry-content"))
          (div (@ (class "video-box"))
               (video (@ (controls "controls")
                         (preload "metadata"))
                      ,@source-links))
          ,@(maybe-render content-html))))
