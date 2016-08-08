(define-module (tests test-asobj)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64)
  #:use-module (web uri)
  #:use-module (pubstrate webapp params)
  #:use-module (pubstrate webapp storage)
  #:use-module (pubstrate webapp user))

(test-begin "test-web-user")

(parameterize ((%base-uri (string->uri "https://coolsite.example/"))
               (%store (make-memory-store)))
  (add-new-user-to-store! "seadub" "monkeybarf")
  (let ((user (store-user-ref "seadub")))
    (test-assert (user-password-matches? user "monkeybarf"))
    (test-assert (not (user-password-matches? user "bananapudding")))))

(test-end "test-web-user")

(test-exit)

