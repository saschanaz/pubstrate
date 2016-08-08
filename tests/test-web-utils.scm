(define-module (tests test-asobj)
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
