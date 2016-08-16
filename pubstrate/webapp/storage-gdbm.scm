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

(define-class <gdbm-store> ()
  (asobj-db #:init-keyword #:asobj-db)
  (container-db #:init-keyword #:container-db))

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
    #:asobj-db
    (gdbm-open (db-file "asobj.db") GDBM_WRCREAT)
    #:container-db
    (gdbm-open (db-file "containers.db") GDBM_WRCREAT)))

(define-method (storage-asobj-set! (store <gdbm-store>) asobj)
  (let ((id (asobj-id asobj)))
    (if (not id)
        (throw 'asobj-storage-failure
               "Can't save an asobj if no id set on its asobj"))
    (gdbm-set! (slot-ref store 'asobj-db)
               id (asobj->combined-string asobj))))

(define-method (storage-asobj-ref (store <gdbm-store>) id)
  (let ((db-result (gdbm-ref (slot-ref store 'asobj-db)
                             id)))
    (if db-result
        (combined-string->asobj db-result (%default-env))
        #f)))

(define (gdbm-store-close store)
  (gdbm-close (slot-ref store 'asobj-db)))


(define (write-to-string obj)
  (with-output-to-string
    (lambda ()
      (write obj (current-output-port)))))

(define (read-from-string str)
  (with-input-from-string str
    (lambda ()
      (read (current-input-port)))))

;; Again, probabalistic, but safe... if even necessary!
(define-method (storage-container-new! (store <gdbm-store>))
  (define container-db (slot-ref store 'container-db))
  (define (keep-trying)
    (let ((token (gen-bearer-token)))
      (if (gdbm-ref container-db token)
          (keep-trying)
          (begin
            (gdbm-set! container-db token
                       (write-to-string '()))
            token))))
  (keep-trying))

(define (get-container-or-error container-db key)
  (cond
   ((gdbm-ref container-db key) => read-from-string)
   (else (throw 'no-container-for-key
                #:key key))))

(define-method (storage-container-append! (store <gdbm-store>)
                                          container-key val)
  (define container-db (slot-ref store 'container-db))
  (define current-members
    (get-container-or-error container-db container-key))

  (gdbm-set! container-db container-key
             (write-to-string
              (cons val current-members))))

(define-method (storage-container-fetch-all (store <gdbm-store>)
                                            container-key)
  (define container-db (slot-ref store 'container-db))
  (get-container-or-error container-db container-key))

(define-method (storage-container-page (store <gdbm-store>) container-key
                                       member how-many)
  (define container-db (slot-ref store 'container-db))
  (list-paginate
   (get-container-or-error container-db container-key)
   member how-many))

(define-method (storage-container-first-page (store <gdbm-store>)
                                             container-key how-many)
  (define container-db (slot-ref store 'container-db))
  (receive (page prev next)
      (list-paginate-first
       (get-container-or-error container-db container-key)
       how-many)
    (values (or page '()) prev next)))

(define-method (storage-container-member? (store <gdbm-store>)
                                          container-key item)
  (if (member (get-container-or-error
               (slot-ref store 'container-db)
               container-key)
              item)
      #t #f))
