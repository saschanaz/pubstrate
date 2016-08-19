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

;;; GDBM based store

(define-module (pubstrate webapp storage-gdbm)
  #:use-module (gdbm)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate webapp auth)
  #:use-module (pubstrate webapp list-pagination)
  #:use-module (pubstrate webapp storage)
  #:export (<gdbm-store>
            make-gdbm-store
            gdbm-store-close))

(define (write-to-string obj)
  (with-output-to-string
    (lambda ()
      (write obj (current-output-port)))))

(define (read-from-string str)
  (with-input-from-string str
    (lambda ()
      (read (current-input-port)))))

(define-class <gdbm-store> (<docustore>)
  (asobjs #:init-keyword #:asobjs)
  (containers #:init-keyword #:containers)
  (bearer-entries #:init-keyword #:bearer-entries)
  (serializers
   #:allocation #:class
   #:init-value
   (lambda (sym)
     (case sym
       ((asobjs) asobj->combined-string)
       ((containers) write-to-string)
       ((bearer-entries)
        (lambda (bearer-entry)
          (write-to-string
           (bearer-entry->alist bearer-entry)))))))
  (deserializers
   #:allocation #:class
   #:init-value
   (lambda (sym)
     (case sym
       ((asobjs)
        (lambda (serialized)
          (if serialized
              (combined-string->asobj serialized (%default-env))
              #f)))
       ((containers) read-from-string)
       ((bearer-entries)
        (lambda (serialized)
          (alist->bearer-entry
           (read-from-string serialized))))))))

(define-method (docustore-set! (store <gdbm-store>)
                               slot key val)
  "Store and serialize VAL for KEY in STORE's SLOT"
  (let ((serialize ((slot-ref store 'serializers) slot)))
    (gdbm-set! (slot-ref store slot) key
               (serialize val))))

(define-method (docustore-ref (store <gdbm-store>)
                              slot key)
  "Retrieve and deserialize value for KEY in STORE's SLOT"
  (let ((deserialize ((slot-ref store 'deserializers) slot)))
    (and=> (gdbm-ref (slot-ref store slot) key)
           deserialize)))

(define-method (docustore-remove! (store <gdbm-store>)
                                   slot key)
  "Retrieve a (serialized) value for KEY in STORE's SLOT"
  (gdbm-delete! (slot-ref store slot) key))

(define (directory-exists? dir)
  "Check to see if DIR exists."
  (and (file-exists? dir)
       (eq? (stat:type (stat dir))
            'directory)))

(define (path-join base-path . paths)
  "Join together BASE-PATH and all remaining PATHS with filename separator.

Note that if there's a forward slash at the end of the final path, this
procedure will not preserve it."
  (let* ((separator-char (string-ref file-name-separator-string 0))
         (stripped-paths
          (map (lambda (path)
                 (string-trim-both path separator-char))
               paths))
         (stripped-base-path (string-trim-right base-path separator-char)))
    (string-join (append (list stripped-base-path)
                         stripped-paths)
     file-name-separator-string)))

(define (make-gdbm-store db-dir)
  (define (db-file filename)
    (path-join db-dir filename))
  (if (not (directory-exists? db-dir))
      (throw 'not-a-directory
             "Provided db-dir is not a directory."
             #:db-dir db-dir))
  (make <gdbm-store>
    #:asobjs
    (gdbm-open (db-file "asobj.db") GDBM_WRCREAT)
    #:containers
    (gdbm-open (db-file "containers.db") GDBM_WRCREAT)
    #:bearer-entries
    (gdbm-open (db-file "bearer-entries.db") GDBM_WRCREAT)))

(define (gdbm-store-close store)
  (gdbm-close (slot-ref store 'asobjs))
  (gdbm-close (slot-ref store 'containers))
  (gdbm-close (slot-ref store 'bearer-entries)))
