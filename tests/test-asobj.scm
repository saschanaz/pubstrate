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

(define root-beer-note-sjson
  '(@ ("@type" . "Create")
      ("@id" . "http://tsyesika.co.uk/act/foo-id-here/")
      ("actor" . (@ ("@type" . "Person")
                    ("@id" . "http://tsyesika.co.uk")
                    ("displayName" . "Jessica Tallon")))
      ("to" "acct:cwebber@identi.ca")
      ("object" . (@ ("@type" . "Note")
                     ("@id" . "http://tsyesika.co.uk/chat/sup-yo/")
                     ("content" . "Up for some root beer floats?")))))

(define root-beer-note-sjson-no-@
  '(@ ("type" . "Create")
      ("id" . "http://tsyesika.co.uk/act/foo-id-here/")
      ("actor" . (@ ("type" . "Person")
                    ("id" . "http://tsyesika.co.uk")
                    ("displayName" . "Jessica Tallon")))
      ("to" "acct:cwebber@identi.ca")
      ("object" . (@ ("type" . "Note")
                     ("id" . "http://tsyesika.co.uk/chat/sup-yo/")
                     ("content" . "Up for some root beer floats?")))))

(define root-beer-note-asobj
  (make-asobj root-beer-note-sjson *basic-env*))
(define root-beer-note-asobj-no-@
  (make-asobj root-beer-note-sjson-no-@ *basic-env*))

(test-equal (asobj-id root-beer-note-asobj)
  "http://tsyesika.co.uk/act/foo-id-here/")
(test-equal (asobj-id root-beer-note-asobj-no-@)
  "http://tsyesika.co.uk/act/foo-id-here/")


(test-end "test-actors")
