;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; Also pulls some code from Guix, which has the following copyrights:
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (pubstrate webapp crypto)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table)
  #:use-module (system foreign)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  ;; TODO: Obvs using guix modules is a problem :)
  #:use-module (guix gcrypt)
  #:use-module (guix base64)
  #:export (sign-data sign-data-base64
            verify-data verify-data-base64))


;;; Error handling stuff.
;;; Snarfed from Guix!

(define error-source
  (let* ((ptr  (libgcrypt-func "gcry_strsource"))
         (proc (pointer->procedure '* ptr (list int))))
    (lambda (err)
      "Return the error source (a string) for ERR, an error code as thrown
along with 'gcry-error'."
      (pointer->string (proc err)))))

(define error-string
  (let* ((ptr  (libgcrypt-func "gcry_strerror"))
         (proc (pointer->procedure '* ptr (list int))))
    (lambda (err)
      "Return the error description (a string) for ERR, an error code as
thrown along with 'gcry-error'."
      (pointer->string (proc err)))))

(define (gcrypt-error-printer port key args default-printer)
  "Print the gcrypt error specified by ARGS."
  (match args
    ((proc err)
     (format port "In procedure ~a: ~a: ~a"
             proc (error-source err) (error-string err)))))

(set-exception-printer! 'gcry-error gcrypt-error-printer)


;;; Random numbers

(define %gcry-weak-random 0)  ; not used
(define %gcry-strong-random 1)
(define %gcry-very-strong-random 2)

(define %gcry-randomize
  (pointer->procedure void (libgcrypt-func "gcry_randomize")
                      `(* ,size_t ,int)))  ; buffer, length, level

(define* (gen-random-bv #:optional (bv-length 50)
                        (level %gcry-strong-random))
  (let* ((bv (make-bytevector bv-length))
         (bv-ptr (bytevector->pointer bv)))
    (%gcry-randomize bv-ptr bv-length %gcry-strong-random)
    bv))

(define %gcry-create-nonce
  (pointer->procedure void (libgcrypt-func "gcry_create_nonce")
                      `(* ,size_t)))  ; buffer, length


(define* (gen-random-nonce #:optional (bv-length 50))
  (let* ((bv (make-bytevector bv-length))
         (bv-ptr (bytevector->pointer bv)))
    (%gcry-create-nonce bv-ptr bv-length)
    bv))

(define* (random-token #:optional (bv-length 30)
                       (type 'strong))
  "Generate a random token.

Generates a token of bytevector BV-LENGTH, default 30.

The default TYPE is 'strong.  Possible values are:
 - strong: Uses libgcrypt's gcry_randomize procedure with level
   GCRY_STRONG_RANDOM (\"use this level for session keys and similar
   purposes\").
 - very-strong: Also uses libgcrypt's gcry_randomize procedure with level
   GCRY_VERY_STRONG_RANDOM (\"Use this level for long term key material\")
 - nonce: Uses libgcrypt's gcry_xcreate_nonce, whose documentation I'll
   just quote inline:

     Fill BUFFER with LENGTH unpredictable bytes.  This is commonly
     called a nonce and may also be used for initialization vectors and
     padding.  This is an extra function nearly independent of the other
     random function for 3 reasons: It better protects the regular
     random generator's internal state, provides better performance and
     does not drain the precious entropy pool."
  (let ((bv (match type
              ('strong
               (gen-random-bv bv-length %gcry-strong-random))
              ('very-strong
               (gen-random-bv bv-length %gcry-very-strong-random))
              ('nonce
               (gen-random-nonce bv-length)))))
    (base64-encode bv 0 bv-length #f #t base64url-alphabet)))


;;; HMAC
;;; ====

(define %no-error 0)  ; GPG_ERR_NO_ERROR

(define-wrapped-pointer-type <mac>
  mac?
  pointer->mac mac->pointer
  (lambda (mac port)
    (format port "#<mac ~x>"
            (pointer-address (mac->pointer mac)))))


(define %gcry-mac-open
  (pointer->procedure int (libgcrypt-func "gcry_mac_open")
                      `(* ,int ,unsigned-int *)))  ; gcry_mac_hd_t *HD, int ALGO,
                                                   ; unsigned int FLAGS, gcry_ctx_t CTX

(define mac-algorithms-mapping
  (alist->hashq-table
   `((sha256 . 101)
     (sha512 . 103)
     (sha3-256 . 116)
     (sha3-512 . 118))))

(define (mac-algo-ref sym)
  (hashq-ref mac-algorithms-mapping sym))

(define mac-algo-maclen
  (let ((proc (pointer->procedure
               int (libgcrypt-func "gcry_mac_get_algo_maclen")
               `(,int))))
    (lambda (sym)
      "Get expected length in bytes of mac yielded by algorithm SYM"
      (proc (mac-algo-ref sym)))))

(define (mac-open algorithm)
  "Create a <mac> object set to use ALGORITHM"
  (let* ((mac (bytevector->pointer (make-bytevector (sizeof '*))))
         (algo (mac-algo-ref algorithm))
         (err (%gcry-mac-open mac algo 0 %null-pointer)))
    (if (= err 0)
        (pointer->mac (dereference-pointer mac))
        (throw 'gcry-error 'mac-open err))))

(define %gcry-mac-setkey
  (pointer->procedure int (libgcrypt-func "gcry_mac_setkey")
                      `(* * ,size_t)))

(define (mac-setkey mac key)
  "Set the KEY on <mac> object MAC

In our case, KEY is either a string or a bytevector."
  (let* ((key (match key
                ((? bytevector? key)
                 key)
                ((? string? key)
                 (string->utf8 key))))
         (err (%gcry-mac-setkey (mac->pointer mac)
                                (bytevector->pointer key)
                                (bytevector-length key))))
    (if (= err 0)
        #t
        (throw 'gcry-error 'mac-setkey err))))

(define mac-close
  (let ((proc (pointer->procedure
               void (libgcrypt-func "gcry_mac_close")
               '(*))))  ; gcry_mac_hd_t H
    (lambda (mac)
      "Release all resources of MAC.

Running this on an already closed <mac> might segfault :)"
      (proc (mac->pointer mac)))))

(define mac-write
  (let ((proc (pointer->procedure
               int (libgcrypt-func "gcry_mac_write")
               `(* * ,size_t))))
    (lambda (mac obj)
      "Writes string or bytevector OBJ to MAC"
      (let* ((bv (match obj
                   ((? bytevector? obj)
                    obj)
                   ((? string? obj)
                    (string->utf8 obj))))
             (err (proc (mac->pointer mac)
                        (bytevector->pointer bv)
                        (bytevector-length bv))))
        (if (= err 0)
            #t
            (throw 'gcry-error 'mac-write err))))))

(define mac-read
  (let ((proc (pointer->procedure
               int (libgcrypt-func "gcry_mac_read")
               `(* * *))))
    (lambda (mac algorithm)
      "Get bytevector representing result of MAC's written, signed data"
      (define (int-bv* n)
        ;; Get the pointer to a bytevector holding an integer with this number
        (let ((bv (make-bytevector (sizeof int))))
          (bytevector-uint-set! bv 0 n (native-endianness) (sizeof int))
          (bytevector->pointer bv)))
      (let* ((bv-len (mac-algo-maclen algorithm))
             (bv (make-bytevector bv-len))
             (err (proc (mac->pointer mac)
                        (bytevector->pointer bv)
                        (int-bv* bv-len))))
        (if (= err 0)
            bv
            (throw 'gcry-error 'mac-read err))))))

;; GPG_ERR_CHECKSUM *should* be 10, but it seems to return here as
;; 16777226... unfortunately this is because we're pulling back an integer
;; rather than the gcry_error_t type.

(define mac-verify
  (let ((proc (pointer->procedure
               int (libgcrypt-func "gcry_mac_verify")
               `(* * ,size_t))))
    (lambda (mac bv)
      "Verify that BV matches result calculated in MAC

BV should be a bytevector with previously calculated data."
      (let ((err (proc (mac->pointer mac)
                       (bytevector->pointer bv)
                       (bytevector-length bv))))
        (if (= err 0)
            (values #t err)
            ;; TODO: This is WRONG!  See the comment above
            ;;   this procedure's definition for why.  If we could
            ;;   parse it as the appropriate GPG error, GPG_ERR_CHECKSUM
            ;;   should be 10.
            (values #f err))))))

(define* (sign-data key data #:key (algorithm 'sha512))
  "Signs DATA with KEY for ALGORITHM.  Returns a bytevector."
  (let ((mac (mac-open algorithm)))
    (mac-setkey mac key)
    (mac-write mac data)
    (let ((result (mac-read mac algorithm)))
      (mac-close mac)
      result)))

(define* (sign-data-base64 key data #:key (algorithm 'sha512))
  "Like sign-data, but conveniently encodes to base64."
  (base64-encode (sign-data key data #:algorithm algorithm)))


(define* (verify-data key data sig #:key (algorithm 'sha512))
  "Verify that DATA with KEY matches previous signature SIG for ALGORITHM."
  (let ((mac (mac-open algorithm)))
    (mac-setkey mac key)
    (mac-write mac data)
    (let ((result (mac-verify mac sig)))
      (mac-close mac)
      result)))

(define* (verify-data-base64 key data b64-sig #:key (algorithm 'sha512))
  (verify-data key data
               (base64-decode b64-sig)
               #:algorithm algorithm))
