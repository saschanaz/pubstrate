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
  #:use-module (pubstrate json-utils)
  #:use-module (pubstrate contrib gcrypt-hash)
  #:use-module (pubstrate contrib base32)
  #:use-module (rnrs bytevectors)
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
  `(@ ("salt" . ,(salted-hash-salt salted-hash))
      ("hash" . ,(salted-hash-hash salted-hash))))

(define (sjson->salted-hash sjson)
  "Convert sjson serialization back to <salted-hash>"
  (make-salted-hash (json-alist-ref sjson "salt")
                    (json-alist-ref sjson "hash")))
