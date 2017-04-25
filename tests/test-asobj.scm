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

(define-module (tests test-asobj)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (tests utils)
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
  '(@ ("@type" "Create")
      ("@id" "http://tsyesika.co.uk/act/foo-id-here/")
      ("actor" (@ ("@type" "Person")
                  ("@id" "http://tsyesika.co.uk")
                  ("displayName" "Jessica Tallon")))
      ("to" ("acct:cwebber@identi.ca"))
      ("object" (@ ("@type" "Note")
                   ("@id" "http://tsyesika.co.uk/chat/sup-yo/")
                   ("content" "Up for some root beer floats?")))))

(define root-beer-note-sjson-no-@
  '(@ ("type" "Create")
      ("id" "http://tsyesika.co.uk/act/foo-id-here/")
      ("actor" (@ ("type" "Person")
                  ("id" "http://tsyesika.co.uk")
                  ("displayName" "Jessica Tallon")))
      ("to" ("acct:cwebber@identi.ca"))
      ("object" (@ ("type" "Note")
                   ("id" "http://tsyesika.co.uk/chat/sup-yo/")
                   ("content" "Up for some root beer floats?")))))

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

(test-assert (asobj-is-a? root-beer-note ^Object))
(test-assert (asobj-is-a? root-beer-note ^Activity))
(test-assert (asobj-is-a? root-beer-note ^Create))
(test-assert (not (asobj-is-a? root-beer-note ^Link)))

;; There's only one type here, Create, but that comes back as a list
(test-equal (asobj-types root-beer-note)
  (list ^Create))

(test-equal (asobj-assoc root-beer-note "to")
  '("to" . "acct:cwebber@identi.ca"))
(test-assert (asobj? (cdr (asobj-assoc root-beer-note "actor"))))
(test-assert (asobj? (asobj-ref root-beer-note "actor")))
(test-equal (asobj-types root-beer-note)
  (list ^Create))

;; is the sjson-assoc-recursive helper working?
(test-equal
    ((@@ (pubstrate asobj) jsobj-assoc-recursive)
     root-beer-note-sjson
     '("actor" "displayName"))
  '(("actor" "displayName") . "Jessica Tallon"))
;; and via the asobj
(test-equal
    (asobj-sjson-assoc root-beer-note
                       '("actor" "displayName"))
  '(("actor" "displayName") . "Jessica Tallon"))

;; If we can't find such a key, it shouldn't panic
(test-equal
    ((@@ (pubstrate asobj) jsobj-assoc-recursive)
     '("actor" "not-a-field")
     root-beer-note-sjson)
  #f)
;; and via the asobj
(test-equal (asobj-sjson-assoc root-beer-note
                               '("actor" "not-a-field"))
  #f)

;; Test that inheritance works right
(test-equal
    ((@@ (pubstrate asobj) astype-list-inheritance)
     (list ^Question ^Invite))
  (list ^Question ^Content ^IntransitiveActivity ^Invite ^Offer
        ^Activity ^Object))

;; Make sure asobj-cons works and can set asobj things
(let ((invitation
       (asobj-cons root-beer-note "object"
                   (as:invite #:content
                              "Libreplanet root beer floats party"))))
  (test-assert (asobj? (asobj-ref invitation "object")))
  (test-equal (asobj-types (asobj-ref invitation "object"))
    (list ^Invite))
  ;; but of course, it should still just be inserted as a jsmap...
  (test-assert (jsmap? (cdr (asobj-sjson-assoc invitation "object")))))

;; Create a version of the root-beer-note with private data attached
;; to it

(define privatized-note
  (asobj-set-private
   root-beer-note
   '(@ ("sekret-kode" "bananaphone"))))

(test-equal
    (asobj-private-ref privatized-note "sekret-kode")
  "bananaphone")

;; Same thing if we were to use the sugar constructor
(let ((privatized-note
       (asobj-set-private* root-beer-note
                           #:sekret-kode "bananaphone")))
  (test-equal
      (asobj-private-ref privatized-note "sekret-kode")
    "bananaphone"))

;; Original note should just have the json-alist-nil though
(test-equal (asobj-private root-beer-note)
    json-alist-nil)

;; Test serializing to and restoring from sjson
(let ((restored-asobj
       (combined-sjson->asobj (asobj->combined-sjson privatized-note)
                              (%default-env))))
  (test-equal (asobj-ref restored-asobj "to")
    "acct:cwebber@identi.ca")
  (test-equal (asobj-private-ref restored-asobj "sekret-kode")
    "bananaphone"))

;; Test serializing to and restoring from string
(let ((restored-asobj
       (combined-string->asobj (asobj->combined-string privatized-note)
                               (%default-env))))
  (test-equal (asobj-ref restored-asobj "to")
    "acct:cwebber@identi.ca")
  (test-equal (asobj-private-ref restored-asobj "sekret-kode")
    "bananaphone"))


;; Test that if we have a list of items, that any items which would be an
;; asobj are treated as such
(let* ((asobj-with-multi-to
        (make-asobj
         '(@ ("@type" "Create")
             ("@id" "http://tsyesika.co.uk/act/foo-id-here/")
             ("actor" (@ ("@type" "Person")
                         ("@id" "http://tsyesika.co.uk")
                         ("displayName" "Jessica Tallon")))
             ("to" ("acct:cwebber@identi.ca"
                    (@ ("@type" "Person")
                       ("@id" "http://foo.example/mcfoo")
                       ("displayName" "Foo McFoo"))))
             ("object" (@ ("@type" "Note")
                          ("@id" "http://tsyesika.co.uk/chat/sup-yo/")
                          ("content" "Up for some root beer floats?"))))
         (%default-env)))
       (to-val (asobj-ref asobj-with-multi-to "to")))
  (test-assert (json-array? to-val))
  (test-assert (string? (first to-val)))
  (test-assert (asobj? (second to-val))))

(test-end "test-asobj")

(test-exit)
