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

(define-module (pubstrate webapp cli configure)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (pubstrate paths)
  #:export (configure-cli))


;;; "komment" utility
;;; =================
;;; pretty-printable block comments

;; @@: Not used currently.  Remove if it stays that way...
(define-record-type <komment>
  (komment text)
  komment?
  (text komment-text))

(set-record-type-printer!
 <komment>
 (lambda (komment port)
   (display (string-append "#| " (komment-text komment) " |#")
            port)))


;;; Main CLI stuff
;;; ==============

(define* (build-config filename
                       #:key (base-uri-str "http://localhost:8080/")
                       (state-dir (dirname filename))
                       (store-setup #f)
                       (extra-imports '()))
  (mkdir-recursive (dirname filename))
  (when (file-exists? filename)
    (display "Clobbering existing file!\n")
    (delete-file filename))
  (with-output-to-file filename
    (lambda ()
      (pretty-print `(use-modules (pubstrate config)
                                  (web uri)
                                  ,@extra-imports))
      (newline)
      (display ";; A default directory used as a base to construct other data/state\n")
      (display ";; paths of the running application (user data, keys, etc).\n")
      (pretty-print `(define %state-dir ,state-dir))
      (newline)
      (display ";; Here's where your config goes.  See documentation for fields.\n")
      (display ";; (Actually, for now see pubstrate/webapp/config.scm, heh)\n")
      (pretty-print
       `(configure
         (base-uri (string->uri ,base-uri-str))
         (state-dir %state-dir)
         ,@(if store-setup
               `((store ,store-setup))
               '()))))))

;; TODO: copied from runserver.scm, maybe we should put this in a utils
;;   file somewhere
(define (maybe-kwarg kwarg val)
  (lambda (current-kwargs)
    (if val
        (append (list kwarg val)
                current-kwargs)
        current-kwargs)))

(define (build-config-interactively config-file)
  (display "Wouldn't it be great if this feature existed?\n"))

(define help-text
  "\
pubstrate-web configure [options] configfile

Write out an initial configuration file for use with Pubstrate's web
application.

 -h, --help            Print this help.
 -d, --state-dir=DIR   Where the application state/data will be kept.
                       If not provided, will be a \"data\" subdirectory
                       of the same directory the config file is written to.
 -u, --base-uri=URL    Base URI the application will run as.
                       This is important, because Pubstrate relies on
                       URIs to construct identifiers.
                       If not given, will default to http://localhost:8080/.
                       (Unless you're just testing things locally, you'll
                       want to change this!)
 --store-type=DB-TYPE  What database/store type you'd like to use.
                       Available options are: memory, gdbm.
                       (Defaults to implicitly gdbm.)\n")

(define option-spec
  '((help (single-char #\h) (value #f))
    (interactive (single-char #\i) (value #f))
    (state-dir (single-char #\d) (value #t))
    (base-uri (single-char #\u) (value #t))
    (store-type (value #t))))

(define (configure-cli args)
  ;; Maybe append a keyword argument to a list of existing
  ;; keyword arguments
  (let* ((options (getopt-long args option-spec
                               #:stop-at-first-non-option #t))
         (config-file
          (match (option-ref options '() '())
            ((config-file _ ...)
             config-file)
            (_ #f))))
    (cond
     ((option-ref options 'help #f)
      (display help-text))
     ((option-ref options 'interactive #f)
      (build-config-interactively config-file))
     (config-file
      (let* ((maybe-append-store-stuff
              (lambda (current-kwargs)
                (match (option-ref options 'store-type #f)
                  ("memory"
                   (append (list #:extra-imports '((pubstrate webapp store))
                                 #:store-setup '(list make-memory-store))
                           current-kwargs))
                  ("gdbm"
                   (append (list #:extra-imports '((pubstrate webapp store-gdbm)
                                                   (pubstrate paths))
                                 #:store-setup
                                 '(list make-gdbm-store
                                        #:path
                                        (path-join %state-dir "gdbm-store")))
                           current-kwargs))
                  (#f current-kwargs)
                  (anything-else
                   (format #t "Unknown store type ~s!\n"
                           anything-else)
                   (exit 1)))))
             (kwargs
              ((compose
                (maybe-kwarg #:base-uri-str
                             (option-ref options 'base-uri #f))
                (maybe-kwarg #:state-dir
                             (option-ref options 'state-dir #f))
                maybe-append-store-stuff)
               '())))
        (apply build-config config-file kwargs)
        (format #t "Wrote file to: ~a\n"
                config-file)))
     (else
      (display "Sorry, you must supply the configfile argument.\n")
      (display help-text)
      (exit 1)))))
