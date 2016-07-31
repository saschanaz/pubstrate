(define-module (tests test-asobj)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate json-utils)
  #:use-module ((pubstrate shorthand)
                #:renamer (symbol-prefix-proc 'as:)))

(test-begin "test-asobj")

;;; Test that vocab URIs are working
(for-each
 (match-lambda
   ((astype expected-uri)
    (test-equal (astype-uri astype) expected-uri)))
 `((,^Object "http://www.w3.org/ns/activitystreams#Object")
   (,^Activity "http://www.w3.org/ns/activitystreams#Activity")
   (,^Collection "http://www.w3.org/ns/activitystreams#Collection")
   (,^Note "http://www.w3.org/ns/activitystreams#Note")))

;; Make sure that building a uri-map works
(let ((uri-map ((@@ (pubstrate asobj) build-astype-map)
                (list *core-vocab*) astype-uri)))
  (test-eq (hash-ref uri-map
                     "http://www.w3.org/ns/activitystreams#Join")
    ^Join)
  ;; And in reverse
  (test-equal (hash-ref ((@@ (pubstrate asobj) reversed-hash-table)
                      uri-map)
                     ^Join)
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

;; Here's a root beer note defined as an <asobj>
(define root-beer-note
  (as:create #:id "http://tsyesika.co.uk/act/foo-id-here/"
             #:actor (as:person #:id "http://tsyesika.co.uk"
                                #:displayName "Jessica Tallon")
             #:to "acct:cwebber@identi.ca"
             #:object (as:note #:id "http://tsyesika.co.uk/chat/sup-yo/"
                               #:content "Up for some root beer floats?")))

;; There's only one type here, Create, but that comes back as a list
(test-equal (asobj-types root-beer-note)
  (list ^Create))

(test-equal (asobj-assoc "to" root-beer-note)
  '("to" . "acct:cwebber@identi.ca"))
(test-assert (asobj? (cdr (asobj-assoc "actor" root-beer-note))))
(test-assert (asobj? (asobj-ref root-beer-note "actor")))
(test-equal (asobj-types root-beer-note)
  (list ^Create))

;; is the sjson-assoc-recursive helper working?
(test-equal
    ((@@ (pubstrate asobj) jsmap-assoc-recursive)
     '("actor" "displayName")
     root-beer-note-sjson)
  '(("actor" "displayName") . "Jessica Tallon"))
;; and via the asobj
(test-equal
    (asobj-sjson-assoc
     '("actor" "displayName")
     root-beer-note)
  '(("actor" "displayName") . "Jessica Tallon"))

;; If we can't find such a key, it shouldn't panic
(test-equal
    ((@@ (pubstrate asobj) jsmap-assoc-recursive)
     '("actor" "not-a-field")
     root-beer-note-sjson)
  #f)
;; and via the asobj
(test-equal
    (asobj-sjson-assoc
     '("actor" "not-a-field")
     root-beer-note)
  #f)

;; Test that inheritance works right
(test-equal
    ((@@ (pubstrate asobj) astype-list-inheritance)
     (list ^Question ^Invite))
  (list ^Question ^Content ^IntransitiveActivity ^Invite ^Offer
        ^Activity ^Object))

;; Make sure asobj-set works and can set asobj things
(let ((invitation
       (asobj-set root-beer-note "object"
                  (as:invite #:content
                             "Libreplanet root beer floats party"))))
  (test-assert (asobj? (asobj-ref invitation "object")))
  (test-equal (asobj-types (asobj-ref invitation "object"))
    (list ^Invite))
  ;; but of course, it should still just be inserted as a jsmap...
  (test-assert (jsmap? (cdr (asobj-sjson-assoc "object" invitation)))))

(test-end "test-asobj")
