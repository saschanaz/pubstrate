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


(define-module (pubstrate webapp cookie)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (web http)
  #:use-module (pubstrate date)
  #:export (set-cookie))

;;; HTTP Cookie support


;;; Cookie, including utils used by Set-Cookie
;;; ==========================================

;; Valid characters for cookie values
;; (all printable ascii characters, excepting "," and ";")
(define cookie-val-char-set
  (char-set-difference (char-set-delete char-set:ascii #\, #\;)
                       char-set:iso-control))
;; Valid characters for cookie names
;; (same as cookie-val-char-set, minus "=")
(define cookie-name-char-set
  (char-set-delete cookie-val-char-set #\=))


;; TODO: Not the best implementation.  On known cookie-av pairs
;;   (see rfc6265) we should do proper parsing.
(define (parse-cookie cookie-text)
  (let ((parts (string-split cookie-text #\;)))
    (define (split-cookie-pair cookie-pair)
      (let* ((trimmed (string-trim cookie-pair))
             (delim (string-index trimmed #\=))
             (attrib (if delim
                         (substring trimmed 0 delim)
                         trimmed))
             (val (if delim
                      (substring trimmed (+ delim 1))
                      #t)))
        (cons attrib val)))
    (map split-cookie-pair parts)))

(define (write-cookie cookie-alist port)
  (let ((cookie-str
         (string-join 
          (map (match-lambda
                 ;; If the value is a string, we join it with =
                 ((name . (? string? val))
                  (string-append name "=" val))
                 ;; If the value is a date, we convert it to
                 ;; an HTTP-style date string, then join with =
                 ((name . (? date? val))
                  (string-append name "=" (date->http-date-string val)))
                 ;; If the value is #t, we just use the name from the pair
                 ((name . #t)
                  name))
               cookie-alist)
          "; ")))
    (display cookie-str port)))

(define (valid-cookie-name? str)
  "Check if STR is a valid cookie name"
  (and (string? str)
       (string-every cookie-name-char-set str)))

(define (valid-cookie-val? str)
  "Check if STR is a valid cookie value"
  (or (eq? str #t)
      (and (string? str)
           (string-every cookie-val-char-set str))
      (date? str)))

(define (validate-cookie cookie-alist)
  (match cookie-alist
    ((((? valid-cookie-name? name) . (or #t (? valid-cookie-val? _))) ...)
     #t)
    (_ #f)))

;; ;; @@: We might never need to do this unless we're a client...
;; (define* (cookie-valid? cookie request #:optional (date (current-date)))
;;   "See whether or not a cookie is valid (applies to domain/path/date)

;; Not the same as cookie-validator, this isn't about syntax."
;;   'TODO)

(declare-header! "Cookie"
                 cookie-parser cookie-validator cookie-writer)


;;; Set-Cookie
;;; ==========

(define (parse-set-cookie str)
  ;; We can utilize the parse-cookie code here.
  ;; The difference between Set-Cookie representation and Cookie
  ;; representation is that the Set-Cookie isn't *just* an alist,
  ;; because the first pair is special (the actual cookie pair name
  ;; and value) whereas the rest are just attributes (properties
  ;; about the cookie that the browser looks at)
  (match (parse-cookie str)
    (((name . val) attrs ...)
     (list name val attrs))))

(define (validate-set-cookie obj)
  (match obj
    ;; See comment in parse-set-cookie.
    ((name val attrs ...)
     (validate-cookie
      (cons (cons name val)
            attrs)))
    (_ #f)))

(define (write-set-cookie obj port)
  (match obj
    ;; See comment in parse-set-cookie.
    ((name val attrs ...)
     (write-cookie
      (cons (cons name val)
            attrs)
      port))))

(declare-header! "Set-Cookie"
                 parse-set-cookie validate-set-cookie write-set-cookie
                 #:multiple? #t)

;;; Utility for users to construct Set-Cookie headers easily.
(define* (set-cookie name #:optional val
                     #:key expires max-age domain
                     path secure http-only
                     (extensions '()))  ; extensions is its own alist
  "Produce a Set-Cookie header"
  (define (maybe-append name val)
    (lambda (prev)
      (if val
          (cons (cons name val)
                prev)
          prev)))
  (define basic-prop-alist
    ((compose
      (maybe-append "Expires" expires)
      (maybe-append "Max-Age" max-age)
      (maybe-append "Domain" domain)
      (maybe-append "Path" path)
      (maybe-append "Secure" secure)
      (maybe-append "HttpOnly" http-only))
     '()))
  (define prop-alist
    (append basic-prop-alist
            extensions))

  (list name val prop-alist))
