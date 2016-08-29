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

(define-module (pubstrate contrib gcrypt-hash)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (pubstrate contrib gcrypt)
  #:export (sha256))

;;; Commentary:
;;;
;;; NOTE: This is borrowed from Guix, but considerably cut down.
;;;   Maybe it should be consolidated in crypto.scm.
;;;
;;; Code:

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
