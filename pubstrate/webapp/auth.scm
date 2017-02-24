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

(define-module (pubstrate webapp auth)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (web http)
  #:use-module (sjson utils)
  #:use-module (pubstrate contrib gcrypt-hash)
  #:use-module (pubstrate contrib base32)
  #:export (gen-bearer-token

            salt-and-hash-password salted-hash-matches?
            salted-hash->string string->salted-hash
            salted-hash->sjson sjson->salted-hash))



;;; OAuth (currently OAuth 2.0) support
;;; ===================================

(define *random-state* (random-state-from-platform))

(define %token-chars
  #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define %token-chars-length (vector-length %token-chars))

(define (random-token-char)
  (vector-ref %token-chars (random %token-chars-length)))

;; TODO: Rename to plain ol' gen-token?  We seem to be using this for
;;   a lot of things...
(define* (gen-bearer-token #:optional (length 50))
  "Bearer tokens can be... well, nearly anything that's a string.
They're fairly opaque, by design.  In our case, a random string
of a specified length is fine."
  (list->string
   (map (lambda _ (random-token-char))
        (iota length))))



;;; (Salted) hash support
;;; =====================

(define-record-type <salted-hash>
  (make-salted-hash salt hash)
  salted-hash?
  (salt salted-hash-salt)
  (hash salted-hash-hash))  ; @@: could later add hash-type if appropriate..

(define %salted-hash-delimiter #\:)

(define* (salt-and-hash-password password
                                 ;; We can use a bearer token as a salt
                                 ;; because it's also just a random string of
                                 ;; characters...
                                 #:optional (salt (gen-bearer-token)))
  "Salt and hash a new password"
  (let* ((salted-password (string-append
                           salt (list->string (list %salted-hash-delimiter))
                           password))
         (hashed-password
          (bytevector->base32-string (sha256 (string->utf8 salted-password)))))
    (make-salted-hash salt hashed-password)))

(define (salted-hash-matches? salted-hash password)
  "See if a <salted-hash> matches this password"
  (let ((pw-salted-hash (salt-and-hash-password
                         password
                         (salted-hash-salt salted-hash))))
    (equal? (salted-hash-hash salted-hash)
            (salted-hash-hash pw-salted-hash))))

(define (salted-hash->string salted-hash)
  "Serialize a <salted-hash> to a string."
  (string-append (salted-hash-salt salted-hash)
                 (list->string (list %salted-hash-delimiter))
                 (salted-hash-hash salted-hash)))

(define (string->salted-hash string)
  (let* ((where (string-index string %salted-hash-delimiter))
         (salt (substring string 0 where))
         (hash (substring string (+ where 1))))
    (make-salted-hash salt hash)))

(define (salted-hash->sjson salted-hash)
  "Serialize a <salted-hash> to sjson."
  `(@ ("salt" ,(salted-hash-salt salted-hash))
      ("hash" ,(salted-hash-hash salted-hash))))

(define (sjson->salted-hash sjson)
  "Convert sjson serialization back to <salted-hash>"
  (make-salted-hash (json-object-ref sjson "salt")
                    (json-object-ref sjson "hash")))


;;; Bearer token http header support
;;; ================================

;;; By default Guile doesn't know anything about bearer tokens.
;;; So we need to supply support for that ourselves...
;;; Code borrowed and adapted from GNU Guile.

;; Copyright (C)  2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; @@: We should get this contributed upstream to Guile!

(define default-val-parser
  (@@ (web http) default-val-parser))
(define skip-whitespace
  (@@ (web http) skip-whitespace))
(define bad-header-component
  (@@ (web http) bad-header-component))
(define parse-key-value-list
  (@@ (web http) parse-key-value-list))
(define write-key-value-list
  (@@ (web http) write-key-value-list))
(define key-value-list?
  (@@ (web http) key-value-list?))
(define declare-header!
  (@@ (web http) declare-header!))

(define* (parse-credentials str #:optional (val-parser default-val-parser)
                            (start 0) (end (string-length str)))
  (let* ((start (skip-whitespace str start end))
         (delim (or (string-index str char-set:whitespace start end) end)))
    (if (= start end)
        (bad-header-component 'authorization str))
    (let ((scheme (string->symbol
                   (string-downcase (substring str start (or delim end))))))
      (case scheme
        ((basic bearer)
         (let* ((start (skip-whitespace str delim end)))
           (if (< start end)
               (cons scheme (substring str start end))
               (bad-header-component 'credentials str))))
        (else
         (cons scheme (parse-key-value-list str default-val-parser delim end)))))))

(define (validate-credentials val)
  (and (pair? val) (symbol? (car val))
       (case (car val)
         ((basic bearer) (string? (cdr val)))
         (else (key-value-list? (cdr val))))))

(define (write-credentials val port)
  (display (header->string (car val)) port)
  (display #\space port)
  (case (car val)
    ((basic bearer) (display (cdr val) port))
    (else (write-key-value-list (cdr val) port))))

(define (declare-credentials-header! name)
  (declare-header! name
    parse-credentials validate-credentials write-credentials))

(declare-credentials-header! "Authorization")
