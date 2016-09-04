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
  #:use-module (pubstrate webapp app)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp store-gdbm)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module ((system repl server)
                #:renamer (symbol-prefix-proc 'repl:))
  #:export (dispatch-subcommand))

;;; "run" subcommand

;; I guess for now we just want to be able to set a prefix
;; and a store location.

(define run-option-spec
  '((help (single-char #\h) (value #f))
    (db-dir (single-char #\d) (value #t))
    (host (single-char #\h) (value #t))
    (port (single-char #\p) (value #t))
    (base-uri (value #t))
    (repl-server (single-char #\r) (value #f))))

(define* (get-store-from-db-path db-path #:key (warn-if-memory-store #t))
  (if db-path
      (make-gdbm-store db-path)
      (begin
        (if warn-if-memory-store
            (display
             "WARNING: db-path not provided, so using memory store only\n"
             (current-error-port)))
        (make-memory-store))))

(define %webapp-help-text
  "\
pubstrate-web run [options] configfile
  -h, --help           Display this help
  -d, --db-dir=PATH    Path to a directory where GDBM databases will be stored.
  -h, --host=HOST      Server host address.  Defaults to localhost.
  -p, --port=PORT      Server port.  Defaults to 8080.

      --base-uri=BASE-URI
                       Base URI to generate URLs from.  Normally generated from
                       host and port.

  -r, --repl-server[=REPL-PORT]
                       Run a REPL server (on REPL-PORT if provided, otherwise
                       on port 37146)")

(define (run-cli args)
  ;; Maybe append a keyword argument to a list of existing
  ;; keyword arguments
  (define (maybe-kwarg kwarg val)
    (lambda (current-kwargs)
      (if val
          (append (list kwarg val)
                  current-kwargs)
          current-kwargs)))
  (let ((options (getopt-long args run-option-spec)))
    (cond
     ((option-ref options 'help #f)
      (display %webapp-help-text) (newline))
     (else
      (let* ((store (get-store-from-db-path
                       (option-ref options 'db-dir #f)))
             (get-option
              (lambda (option)
                (option-ref options option #f)))
             ;; Build up all the keyword arguments sent to run-server
             ;; depending on the options passed in
             (kwargs
              ((compose
                (maybe-kwarg #:host (get-option 'host))
                (maybe-kwarg #:port (get-option 'port))
                (maybe-kwarg #:base-uri (get-option 'base-uri)))
               (list #:store store))))
        ;; Enable the REPL server if requested
        (match (option-ref options 'repl-server #f)
          ;; If false, do nothing
          (#f #f)
          ;; If it's a string and a number, serve the repl on this
          ;; port number on localhost
          ((and (? string? _) (= string->number repl-port-number))
           (repl:spawn-server (repl:make-tcp-server-socket #:port repl-port-number)))
          ;; Anything else, start with the default port
          (_ (repl:spawn-server)))
        ;; Now run the server! :)
        (apply run-webapp
               ;; Announce that we're running on start-up
               #:announce (current-output-port)  ; @@: maybe STDERR instead?
               kwargs))))))


;;; Main / subcommand dispatch CLI
;;; ==============================

;; TODO
(define (configure-cli args)
  'TODO)

(define subcommands
  `(("run" . (,run-cli "Run the web server"))
    ;; ("demo" . ,demo-cli)
    ("configure" . (,configure-cli "Write out initial configuration file"))))

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
