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

(define-module (pubstrate webapp cli)
  #:use-module (pubstrate package-config)
  #:use-module (pubstrate webapp cli runserver)
  #:use-module (pubstrate webapp cli configure)
  #:use-module (pubstrate webapp cli adduser)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:export (dispatch-subcommand))


;;; Main / subcommand dispatch CLI
;;; ==============================

(define subcommands
  `(("run" . (,run-cli "Run the web server"))
    ;; ("demo" . ,demo-cli)
    ("configure" . (,configure-cli "Write out initial configuration file"))
    ("adduser" . (,adduser-cli "Add a new user to the store"))))

(define (show-main-help)
  (display "Usage: pubstrate-web [--help | --version] [command [command-options]]\n")
  (newline)
  (display "Available subcommands:\n")
  (for-each
   (match-lambda
     ((name . (cmd desc))
      (format #t " - ~a: ~a\n"
              name desc)))
   subcommands))

(define main-option-spec
  '((help (single-char #\h) (value #f))
    (version (value #f))))

(define additional-version-string
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.")

(define (dispatch-subcommand args)
  (define options (getopt-long args main-option-spec
                               #:stop-at-first-non-option #t))
  (define main-args
    (assoc-ref options '()))
  (cond
   ;; Main help goes here!
   ((assoc-ref options 'help)
    (show-main-help))
   ;; Show verison number
   ((assoc-ref options 'version)
    (format #t "~a version ~a\n"
            %pubstrate-package-name %pubstrate-version)
    (display additional-version-string) (newline))
   ((eq? main-args '())
    (show-main-help))
   ;; Dispatch to subcommand
   ((assoc-ref subcommands (car main-args)) =>
    (match-lambda
      ((subcmd help)
       (subcmd main-args))))
   ;; Invalid subcommand, list available subcommands
   (else
    (display "Invalid subcommand.\n\n")
    (show-main-help))))
