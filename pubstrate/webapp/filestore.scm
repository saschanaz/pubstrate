;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2016, 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (pubstrate webapp filestore)
  #:use-module (oop goops)
  #:use-module (pubstrate contrib define-method-star)
  #:use-module (pubstrate paths)
  #:export (<filestore>
            filestore-local?

            <simple-filestore>
            filestore-base-path
            make-simple-filestore

            filestore-local-path
            filestore-file-exists?
            filestore-open
            filestore-open-read
            filestore-open-write
            filestore-delete))

(define-class <filestore> ()
  (local? #:init-value #t
          #:allocation #:class
          #:getter filestore-local?))

(define-class <simple-filestore> (<filestore>)
  (local? #:init-value #t
          #:allocation #:class
          #:getter filestore-local?)
  (base-path #:init-keyword #:base-path
             #:getter filestore-base-path))

(define (make-simple-filestore base-path)
  (make <simple-filestore>
    #:base-path base-path))

(define-method* (filestore-local-path (filestore <simple-filestore>) path)
  (clean-path
   (apply path-join (filestore-base-path filestore)
          path)))

(define-method (filestore-file-exists? (filestore <simple-filestore>) path)
  (file-exists? (filestore-local-path filestore path)))

(define-method* (filestore-open (filestore <simple-filestore>) path
                                #:optional (flags O_RDONLY)
                                #:key (make-directory? #f))
  (let* ((local-path (filestore-local-path filestore path))
         (local-dir (dirname local-path)))
    (when (and make-directory?
               (not (file-exists? local-dir)))
      (mkdir-recursive local-dir))
    (open local-path flags)))

(define-method (filestore-open-read (filestore <simple-filestore>) path)
  (filestore-open filestore path O_RDONLY))

(define-method (filestore-open-write (filestore <simple-filestore>) path)
  (filestore-open filestore path (logior O_WRONLY O_CREAT)
                  #:make-directory? #t))

(define-method (filestore-delete (filestore <simple-filestore>) path)
  (delete-file (filestore-local-path filestore path)))
