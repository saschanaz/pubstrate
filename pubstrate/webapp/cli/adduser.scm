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

(define-module (pubstrate webapp cli adduser)
  #:use-module (pubstrate webapp app)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp ctx)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:export (adduser-cli))

(define (adduser-with-config config-filename username password)
  (with-app-ctx-from-config-file
   "/home/cwebber/sandbox/data/pubstrate/pubstrate-cfg.scm"
   (lambda ()
     (db-add-new-user! (ctx-ref 'db)
                       username password))))

(define option-spec
  '((help (single-char #\h) (value #f))
    (username (single-char #\u) (value #t))
    (password (single-char #\p) (value #t))))

(define help-text
  "\
pubstrate-web adduser [options] configfile

 -u, --username       Username for user
 -p, --password       Password for user")

(define (adduser-cli args)
  (define (prompt-user prompt-text)
    (display prompt-text)
    (read-line (current-input-port)))

  (let* ((options (getopt-long args option-spec
                               #:stop-at-first-non-option #t))
         (config-filename
          (match (assoc-ref options '())
            ((config-filename _ ...)
             config-filename)
            (_ #f))))
    (cond
     ((or (option-ref options 'help #f)
          (not config-filename))
      (display help-text) (newline))
     (else
      (let ((username (or (option-ref options 'username #f)
                          (let lp ()
                            (or (prompt-user "Username: ")
                                (begin
                                  (display "No username supplied.\n")
                                  (lp))))))
            (password (or (option-ref options 'password #f)
                          (prompt-user "Password: "))))
        (newline)
        (adduser-with-config config-filename username password))))))
