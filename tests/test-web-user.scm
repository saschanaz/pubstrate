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

(define-module (tests test-web-user)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64)
  #:use-module (web uri)
  #:use-module (pubstrate webapp params)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp user))

(test-begin "test-web-user")

(parameterize ((%base-uri (string->uri "https://coolsite.example/"))
               (%store (make-memory-store)))
  (test-equal (user-id-from-username "discocat")
    "https://coolsite.example/u/discocat")
  (store-add-new-user! (%store) "seadub" "monkeybarf")
  (let ((user (store-user-ref (%store) "seadub")))
    (test-assert (user-password-matches? user "monkeybarf"))
    (test-assert (not (user-password-matches? user "bananapudding")))))

(test-end "test-web-user")

(test-exit)

