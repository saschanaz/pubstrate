;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (tests test-filestore)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (pubstrate webapp filestore)
  #:use-module (pubstrate paths))

(test-begin "test-filestore")

(call-with-temporary-directory
 (lambda (tmpdir)
   (define filestore (make-simple-filestore tmpdir))
   (test-assert (not (filestore-file-exists? filestore '("is" "this" "dog.txt"))))
   (test-assert (not (file-exists? (path-join tmpdir "/is/this/dog.txt"))))
   (let ((dog-file (filestore-open-write filestore '("is" "this" "dog.txt"))))
     (display "YES, THIS IS DOG" dog-file)
     (close dog-file))
   (test-assert (filestore-file-exists? filestore '("is" "this" "dog.txt")))
   (test-assert (file-exists? (path-join tmpdir "/is/this/dog.txt")))
   (let ((dog-file (filestore-open-read filestore '("is" "this" "dog.txt"))))
     (test-equal (get-string-all dog-file)
       "YES, THIS IS DOG")
     (close dog-file))
   (filestore-delete filestore '("is" "this" "dog.txt"))
   (test-assert (not (filestore-file-exists? filestore '("is" "this" "dog.txt"))))
   (test-assert (not (file-exists? (path-join tmpdir "/is/this/dog.txt"))))))

(test-end "test-filestore")
(test-exit)
