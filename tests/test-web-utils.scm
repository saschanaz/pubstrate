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

(define-module (tests test-web-utils)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64)
  #:use-module (web uri)
  #:use-module (pubstrate webapp params)
  #:use-module (pubstrate webapp utils))

(test-begin "test-web-utils")

(parameterize ((%base-uri (string->uri "https://coolsite.example/")))
  (test-equal (local-uri) "/")
  (test-equal (local-uri "foo" "bar") "/foo/bar")
  (test-equal (abs-local-uri "foo" "bar") "https://coolsite.example/foo/bar"))

(parameterize ((%base-uri (string->uri "https://coolsite.example/stuff")))
  (test-equal (local-uri) "/stuff")
  (test-equal (local-uri "foo" "bar") "/stuff/foo/bar")
  (test-equal (abs-local-uri "foo" "bar") "https://coolsite.example/stuff/foo/bar"))

(test-end "test-web-utils")

(test-exit)
