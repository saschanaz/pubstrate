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
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp utils))

(test-begin "test-web-utils")

(with-extended-ctx
 `((base-uri . ,(string->uri "https://coolsite.example/")))
 (lambda ()
   (test-equal (local-uri) "/")
   (test-equal (local-uri "foo" "bar") "/foo/bar")
   (test-equal (abs-local-uri "foo" "bar") "https://coolsite.example/foo/bar")))

(with-extended-ctx
 `((base-uri . ,(string->uri "https://coolsite.example/stuff")))
 (lambda ()
   (test-equal (local-uri) "/stuff")
   (test-equal (local-uri "foo" "bar") "/stuff/foo/bar")
   (test-equal (abs-local-uri "foo" "bar") "https://coolsite.example/stuff/foo/bar")))

(define stupid-l33t-form
  (urldecode "newbl33t=wow%20that%20%3D%3D%20a%20cool%20thing%21%20%23%20lol&ya=buddy"))

;; This is the worst test variable I have ever made
(test-equal (assoc-ref stupid-l33t-form "newbl33t")
  "wow that == a cool thing! # lol")
(test-equal (assoc-ref stupid-l33t-form "ya")
  "buddy")
;; Should also work if a bytevector
(test-equal (assoc-ref (urldecode
                        #vu8(121 97 61 98 117 100 100 121)) ; "ya=buddy"
                       "ya")
  "buddy")

(test-equal (urlencode '((banana . "peanut butter")
                         ("cookie" . "party")))
  "banana=peanut%20butter&cookie=party")

(test-equal (uri->string
             (uri-set (string->uri "http://foo.example")
                      #:scheme 'https
                      #:port '8080
                      #:path "/yeah"
                      #:query '((cookie . "party"))))
  "https://foo.example:8080/yeah?cookie=party")

(test-end "test-web-utils")
(test-exit)
