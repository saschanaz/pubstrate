;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (pubstrate contrib hash)
  #:use-module (system foreign)
  #:use-module (pubstrate paths)
  #:use-module (rnrs bytevectors)
  #:export (shas256))

;;; Commentary:
;;;
;;; Common code for the GNU Libgcrypt bindings.  Loading this module
;;; initializes Libgcrypt as a side effect.
;;;
;;; NOTE: This is mostly borrowed from Guix... two separate modules!
;;;   guix/gcrypt.scm and guix/hash.scm ...!  (A bit cut down, though!)
;;; Code:

(define libgcrypt-func
  (let ((lib (dynamic-link %libgcrypt)))
    (lambda (func)
      "Return a pointer to symbol FUNC in libgcrypt."
      (dynamic-func func lib))))

(define gcrypt-version
  ;; According to the manual, this function must be called before any other,
  ;; and it's not clear whether it can be called more than once.  So call it
  ;; right here from the top level.
  (let* ((ptr     (libgcrypt-func "gcry_check_version"))
         (proc    (pointer->procedure '* ptr '(*)))
         (version (pointer->string (proc %null-pointer))))
    (lambda ()
      "Return the version number of libgcrypt as a string."
      version)))

(define-syntax GCRY_MD_SHA256
  ;; Value as of Libgcrypt 1.5.2.
  (identifier-syntax 8))

(define sha256
  (let ((hash (pointer->procedure void
                                  (libgcrypt-func "gcry_md_hash_buffer")
                                  `(,int * * ,size_t))))
    (lambda (bv)
      "Return the SHA256 of BV as a bytevector."
      (let ((digest (make-bytevector (/ 256 8))))
        (hash GCRY_MD_SHA256 (bytevector->pointer digest)
              (bytevector->pointer bv) (bytevector-length bv))
        digest))))
