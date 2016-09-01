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
  #:use-module (ice-9 match)
  #:use-module (pubstrate contrib html)
  #:use-module (pubstrate webapp ctx)
  #:use-module ((pubstrate webapp http-status)
                #:renamer (symbol-prefix-proc 'status:))
  #:use-module (rnrs bytevectors)
  #:use-module (web response)
  #:use-module (web request)
  #:use-module (web uri)
  #:export (local-uri abs-local-uri
            respond respond-html
            respond-redirect respond-not-found
            requesting-asobj?
            decode-urlencoded-form))

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
  (match (request-content-type request)
    (((or 'application/activity+json
          'application/ld+json) _ ...)
     #t)
    (_ #f)))


(define string->uri* (@@ (web uri) string->uri*))

(define* (respond-redirect to #:key permanent
                           (extra-headers '()))
  (respond ""
           #:extra-headers
           `((location . ,(if (string? to)
                              (string->uri* to)
                              to))
             ,@extra-headers)
           #:status (if permanent
                        status:moved-permanently
                        status:found)))

(define (decode-urlencoded-form body)
  "Decode application/x-www-form-urlencoded BODY"
  (let ((str (match body
               ((? bytevector? _) (utf8->string body))
               ((? string? _) body))))
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
     (string-split str #\&))))
