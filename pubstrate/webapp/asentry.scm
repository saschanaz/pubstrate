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

(define-module (pubstrate webapp asentry)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (pubstrate asobj)
  #:export (make-asentry <asentry>
            asentry?
            asentry-asobj set-asentry-asobj
            asentry-private set-asentry-private

            asentry-id))

(define-immutable-record-type <asentry>
  (make-asentry asobj private)
  asentry?
  (asobj asentry-asobj set-asentry-asobj)
  ;; Private data is sjson
  (private asentry-private set-asentry-private))

(define (asentry-id asentry)
  "Shortcut to get the id from an asentry's asobj."
  (asobj-id (asentry-asobj asentry)))
