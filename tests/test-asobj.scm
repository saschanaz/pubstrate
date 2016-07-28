(define-module (tests test-asobj)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (activitystuff vocab)
  #:use-module (activitystuff asobj))

(test-begin "test-asobj")

;;; Test that vocab URIs are working
(for-each
 (match-lambda
   ((astype expected-uri)
    (test-equal (astype-uri astype) expected-uri)))
 `((,$Object "http://www.w3.org/ns/activitystreams#Object")
   (,$Activity "http://www.w3.org/ns/activitystreams#Activity")
   (,$Collection "http://www.w3.org/ns/activitystreams#Collection")
   (,$Note "http://www.w3.org/ns/activitystreams#Note")))

;; Make sure that building a uri-map works
(let ((uri-map ((@@ (activitystuff asobj) build-astype-map)
                (list *core-vocab*) astype-uri)))
  (test-eq (hash-ref uri-map
                     "http://www.w3.org/ns/activitystreams#Join")
    $Join)
  ;; And in reverse
  (test-equal (hash-ref ((@@ (activitystuff asobj) reversed-hash-table)
                      uri-map)
                     $Join)
    "http://www.w3.org/ns/activitystreams#Join"))

(test-end "test-actors")
