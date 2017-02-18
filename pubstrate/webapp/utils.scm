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

;;;
;;; Utils and misc
;;;

(define-module (pubstrate webapp utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate contrib html)
  #:use-module (pubstrate webapp ctx)
  #:use-module ((pubstrate webapp http-status)
                #:renamer (symbol-prefix-proc 'status:))
  #:use-module (rnrs bytevectors)
  #:use-module (web response)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (web http)
  #:export (local-uri abs-local-uri
            respond respond-html
            respond-redirect respond-not-found
            require-login
            requesting-asobj?
            uri-set
            urlencode urldecode request-query-form
            asobj-local? uri-local?))

;; TODO: add local-uri* and abs-local-uri* which should allow
;;   optional GET parameters & fragments

(define (local-uri . path)
  "Construct a local URI for this site based off the %base-uri parameter"
  (if (not (ctx-ref 'base-uri))
      (throw 'base-uri-not-set
             "%base-uri parameter not set"))
  (string-append
     "/"
     (encode-and-join-uri-path
      (append (split-and-decode-uri-path
               (uri-path (ctx-ref 'base-uri)))
              path))))

(define (abs-local-uri . path)
  "Build an absolute local URI, as a string."
  (let ((base-uri (ctx-ref 'base-uri)))
    (uri->string
     (build-uri (uri-scheme base-uri)
                #:host (uri-host base-uri)
                #:port (uri-port base-uri)
                #:path (apply local-uri path)))))

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
                   (set-port-encoding! port "utf-8")
                   (sxml->html sxml port))
         respond-args))

(define robot-404-message "\
           _________________
        .-'                 '-.
      .'                       '.
     .\"  _  _    ___  _  _   _  \".
     '  | || |  / _ \\| || | | |  '
     |  | || |_| | | | || |_| |  |
     |  '._   _| '-' ;._   _|_|  |
     '     |_|  \\___/   |_| (_)  '
     '.                         .'
       '.                     .'    _--------_
         '---------   .------'    .'          '.
                  / ./           |   Page Not   |
                 (  (      .o    '.   Found!   .'
                  '.\\     (        '--.   .---'
                        .---.          ) )
                     o-|O   O|-o    --=-'
                       |[vvv]|
                      .'-----'.
                   .##|.-=.=-.|##.
                   #  ||'~-~'||  #
                  (') ||_____|| (')
                      '-------'
                      ((     ))
                     [__]   [__]")

(define (respond-not-found)
  (respond robot-404-message
           #:content-type 'text/plain
           #:status status:not-found))

(define (requesting-asobj? request)
  (any (match-lambda
         (((or 'application/activity+json
               'application/ld+json) _ ...)
          #t)
         (_ #f))
       (request-accept request)))


(define* (uri-set orig-uri #:key
                  (scheme #nil) (userinfo #nil)
                  (host #nil) (port #nil)
                  (path #nil) (query #nil)
                  (fragment #nil))
  "Functional setter for URIs"
  (define (maybe-val val orig-getter)
    (if (not (eq? val #nil))
        val
        (orig-getter orig-uri)))

  (define parsed-query
    (match query
      (#f #f)
      ((? (lambda (x) (eq? x #nil)) _) #nil)
      ((? string? _) query)
      ((? pair? _) (urlencode query))))

  (build-uri-reference
   #:scheme (maybe-val scheme uri-scheme)
   #:userinfo (maybe-val userinfo uri-userinfo)
   #:host (maybe-val host uri-host)
   #:port (maybe-val port uri-port)
   #:path (maybe-val path uri-path)
   #:query (maybe-val parsed-query uri-query)
   #:fragment (maybe-val fragment uri-fragment)
   #:validate? #f))

(define* (respond-redirect to #:key permanent
                           query
                           (extra-headers '()))
  "Respond with a redirect.

TO is either a URI or a string to be converted to a URI.
PERMANENT will specify status \"301 Moved Permanently\", otherwise
the default status \"302 Found\" will be used.

QUERY may be supplied, and should be an alist of query parameters.

EXTRA-HEADERS may supply additional HTTP headers."
  (define to-uri
    (let ((uri (if (string? to)
                   (string->uri-reference to)
                   to)))
      ;; Maybe append query parameters
      (if query
          (uri-set uri #:query query)
          uri)))
  (respond ""
           #:extra-headers `((location . ,to-uri)
                             ,@extra-headers)
           #:status (if permanent
                        status:moved-permanently
                        status:found)))

(define (urldecode form)
  "Decode application/x-www-form-urlencoded FORM

FORM may be a utf8-encoded bytevector or a string."
  (if form
      (let ((str (match form
                   ((? bytevector? _) (utf8->string form))
                   ((? string? _) form))))
        (map
         (lambda (item)
           (match (string-split item #\=)
             ((key val)
              (cons (uri-decode key)
                    (uri-decode val)))
             (key
              (cons (uri-decode key)
                    #f))
             (_ (throw 'bad-urlencoded-string
                       #:string str
                       #:item item))))
         (string-split str #\&)))
      '()))


(define (urlencode query-alist)
  "Urlencode QUERY-ALIST into a string"
  (string-join
   (map (match-lambda
          ((key . val)
           (string-append (uri-encode (match key
                                        ((? string? _) key)
                                        ((? symbol? _) (symbol->string key))))
                          "="
                          (uri-encode val))))
        query-alist)
   "&"))

(define (request-query-form request)
  "Return parsed alist from query parameter of REQUEST's uri"
  (and=> (uri-query (request-uri request))
         urldecode))

(define (require-login request thunk)
  (if (not (ctx-ref 'user))
      (respond-redirect
       (local-uri "login")
       #:query `(("next" . ,(uri-path (request-uri request)))))
      (thunk)))

(define (asobj-local? asobj)
  "Return whether ASOBJ has an id relative to the base-uri in %ctx"
  (let ((asobj-uri (asobj-id asobj)))
    (uri-local? asobj-uri)))

(define (uri-local? uri)
  "Return whether URI is relative to the base-uri in %ctx"
  (let ((uri (match uri
               (#f #f)
               ((? string? _)
                (string->uri uri))
               ((? uri? _)
                uri)))
        (base-uri (ctx-ref 'base-uri)))
    ;; @@: We're not looking at fragment URIs as being a base, but
    ;;  that doesn't likely make sense anyway for our purposes
    (and uri
         (eq? (uri-scheme base-uri)   ; TODO: make http/https equiv?
              (uri-scheme uri))
         (equal? (uri-host base-uri)
                 (uri-host uri))
         (equal? (uri-port base-uri)
                 (uri-port uri))
         (string-prefix? (or (uri-path base-uri) "")
                         (uri-path uri)))))
