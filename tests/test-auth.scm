(define-module (tests test-asobj)
  #:use-module (srfi srfi-64)
  #:use-module (pubstrate webapp auth))

(test-begin "test-auth")

;; Basic hashing checks
(test-assert
    (salted-hash-matches?
     (salt-and-hash-password "monkey")
     "monkey"))
(test-assert
    (not (salted-hash-matches?
          (salt-and-hash-password "monkey")
          "banana")))

(define salted-hash-salt (@@ (pubstrate webapp auth)
                             salted-hash-salt))
(define salted-hash-hash (@@ (pubstrate webapp auth)
                             salted-hash-hash))

;; Shouldn't be the same thing twice because it should be
;; a different hash
(let ((salted-hash1 (salt-and-hash-password "monkey"))
      (salted-hash2 (salt-and-hash-password "monkey")))
  
  ;; not the same when serialized
  (test-assert
      (not (equal? (salted-hash->string salted-hash1)
                   (salted-hash->string salted-hash2))))
  ;; not the same salt
  (test-assert
      (not (equal? (salted-hash-salt salted-hash1)
                   (salted-hash-salt salted-hash2))))
  ;; and certainly not the same hash!
  (test-assert
      (not (equal? (salted-hash-hash salted-hash1)
                   (salted-hash-hash salted-hash2)))))

(test-end "test-auth")
