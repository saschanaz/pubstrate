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

(define-module (pubstrate webapp conditions)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module ((pubstrate webapp http-status)
                #:renamer (symbol-prefix-proc 'status:))
  #:export (&webapp-error
            webapp-error?
            error-message
            error-code

            &server-error
            server-error?

            &user-error
            user-error?

            raise-server-error
            raise-user-error))

;; An error caused in the web application that we should probably
;; explain to the user.
(define-condition-type &webapp-error &error
  webapp-error?
  ;; A message to present to the user
  (message error-message)
  ;; The type of error code, ie
  ;; 400 for Bad Request or
  ;; 500 for Internal Server Error
  (code error-code))

;; An error caused on the server's end.
(define-condition-type &server-error &webapp-error
  server-error?)

;; An error caused by user input.
(define-condition-type &user-error &webapp-error
  user-error?)

(define* (raise-server-error message #:optional (code status:internal-server-error))
  (raise (condition (&server-error
                     (message message)
                     (code code)))))

(define* (raise-user-error message #:optional (code status:bad-request))
  (raise (condition (&user-error
                     (message message)
                     (code code)))))
