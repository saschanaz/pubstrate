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

(define-module (pubstrate webapp cli runserver)
  #:use-module (pubstrate webapp app)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp store-gdbm)
  #:use-module (pubstrate config)
  #:use-module (pubstrate webapp config)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module ((system repl server)
                #:renamer (symbol-prefix-proc 'repl:))
  #:export (run-cli))

;;; "run" subcommand

;; I guess for now we just want to be able to set a prefix
;; and a store location.

(define run-option-spec
  '((help (single-char #\h) (value #f))
    (host (single-char #\h) (value #t))
    (port (single-char #\p) (value #t))
    (config (single-char #\c) (value #t) (required? #t))
    (repl-server (single-char #\r) (value optional))))

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
  -h, --host=HOST      Server host address.  Defaults to localhost.
  -p, --port=PORT      Server port.  Defaults to 8080.

  -r, --repl-server[=REPL-PORT|REPL-SOCKET-FILE]
                       Run a REPL server (on REPL-PORT if provided and a number,
                       or filename REPL-SOCKET-FILE if a string, otherwise on
                       port 37146)")

(define (run-cli args)
  ;; Maybe append a keyword argument to a list of existing
  ;; keyword arguments
  (define (maybe-kwarg kwarg val)
    (lambda (current-kwargs)
      (if val
          (append (list kwarg val)
                  current-kwargs)
          current-kwargs)))
  (let ((options (getopt-long args run-option-spec
                              #:stop-at-first-non-option #t)))
    (cond
     ((option-ref options 'help #f)
      (display %webapp-help-text) (newline))
     (else
      (let* ((config-filename
              (option-ref options 'config #f))
             (config (load-config (load-from-path config-filename)
                                  pubstrate-config-spec))
             (get-option
              (lambda (option)
                (option-ref options option #f)))
             ;; Build up all the keyword arguments sent to run-server
             ;; depending on the options passed in
             (kwargs
              ((compose
                (maybe-kwarg #:host (get-option 'host))
                (maybe-kwarg #:port (get-option 'port)))
               '()))
             ;; @@: Maybe (ice-9 q) is better...
             (cleanup-steps '()))
        (define (spawn-repl-socket-file socket-path)
          (repl:spawn-server (repl:make-unix-domain-server-socket
                              #:path socket-path))
          (set! cleanup-steps
                (cons (lambda ()
                        (delete-file socket-path))
                      cleanup-steps)))
        ;; Enable the REPL server if requested
        (match (option-ref options 'repl-server #f)
          ;; If false, do nothing
          (#f #f)
          ;; If it's a string and a number, serve the repl on this
          ;; port number on localhost
          ((and (? string? _) (? string->number _)
                (= string->number repl-port-number))
           (format #t "Starting REPL on port ~a\n" repl-port-number)
           (repl:spawn-server (repl:make-tcp-server-socket
                               #:port repl-port-number)))
          ((and (? string? socket-path))
           (format #t "Starting REPL at socket ~a\n" socket-path)
           (spawn-repl-socket-file socket-path))
          ;; Anything else, start with the default port
          (_
           (display "Starting REPL at socket /tmp/guile-socket\n")
           (spawn-repl-socket-file "/tmp/guile-socket")))
        ;; Now run the server! :)
        (dynamic-wind
          (const #f) ; no-op
          (lambda ()
            (apply run-webapp config
                   ;; Announce that we're running on start-up
                   #:announce (current-output-port)  ; @@: maybe STDERR instead?
                   kwargs))
          ;; Do cleanup (if any)
          (lambda ()
            (for-each (lambda (step)
                        (step))
                      cleanup-steps))))))))

