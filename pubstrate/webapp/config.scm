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

;;; Configuration of the web application
;;;

(define-module (pubstrate webapp config)
  #:use-module (pubstrate paths)
  #:use-module (pubstrate config)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (pubstrate webapp db)
  #:use-module (pubstrate webapp db-gdbm)
  #:use-module (pubstrate webapp filestore)
  #:export (pubstrate-config-spec))

;; TODO: Not used (and neither is the plain db import, then)
;;   Remove if we decide to keep it that way.
(define (make-memory-db-but-warn)
  (display
   "WARNING: db-path not provided, so using memory db only\n"
   (current-error-port))
  (make-memory-db))

(define procedure-or-procedure-with-args?
  (match-lambda
    ((? procedure? _) #t)
    (((? procedure? _) args ...) #t)
    (_ #f)))

(define pubstrate-config-spec
  (make-config-spec*
   ;; The (only) two mandatory fields
   (base-uri "Base URI this application is running at.
This should be a uri type."
             #:validate uri?)
   (state-dir
    "This is a base directory for storing state of the application (ie, data).

This config variable is mandatory, but is not generally used directly.
Instead, several other variables (such as signing-key-path) use it as
a basis to infer a default location to put their own data."
    #:validate string?)

   (post-to-localhost?
    "Whether or not the application will permit posting to localhost / 127.0.0.1.
Doing so can be dangerous, since some services expect localhost to be 'private'
for a machine."
    #:validate boolean?
    #:default (const #f))

   (incoming-filter
    "Predicate that takes an incoming AS2 object and determines
whether or not it will be accepted to the actor's inbox.  Takes
two arguments, the ASOBJ in question, and the ACTOR asobj."
    #:validate procedure?
    #:default (const #t))

   (db "Database.
This should be a procedure to produce a database, or a list
of (db-constructor args ...) where db-constructor is a procedure
to initialize a database and args are arguments to that procedure."
       #:validate procedure-or-procedure-with-args?
          ;;;; If nothing is provided, the application will
          ;;;; initialize a memory db but will throw a warning.
       ;; #:default (const make-memory-db-but-warn)
       #:default (lambda (cfg)
                   (list make-gdbm-db
                         (path-join (assoc-ref cfg 'state-dir)
                                    "gdbm-db"))))
   (signing-key-path
    "Filename to keep the private key at.
If not provided, will be constructed from state-dir.
If the file does not exist, it will be created at initialization
time."
    #:validate string?
    #:default (lambda (cfg)
                (path-join (assoc-ref cfg 'state-dir)
                           "crypto" "signing-key.txt")))


   (filestore "Filestore.
This should be a procedure to produce a filestore, or a list
of (filestore args ...) where filestore is a procedure
to initialize a filestore system and args are arguments to that
procedure."
              #:validate procedure-or-procedure-with-args?
              #:default
              (lambda (cfg)
                (list make-simple-filestore
                      (path-join (assoc-ref cfg 'state-dir)
                                 "files"))))
   
   ;; (https-only
   ;;  "Whether Pubstrate should only communicate with other servers over HTTPS."
   ;;  #:default #f)
   ))

