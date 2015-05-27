;; Experimenting with api stuff in here

(define root-beer-note
  (make-as <Post>
    ;; Putting the string first is the same thing as #:@id
    "http://tsyesika.co.uk/act/foo-id-here/"
    #:actor (make-as <Person>
              #:@id "http://tsyesika.co.uk"
              #:displayName "Jessica Tallon")
    #:to (list "acct:cwebber@identi.ca")
    #:object (make-as <Note>
               "http://tsyesika.co.uk/chat/sup-yo/"
               #:content "Up for some root beer floats?")))

(as-to-json root-beer-note)

(define json-root-beer-note
  "{\"@type\": \"Post\",
 \"@id\": \"http://tsyesika.co.uk/act/foo-id-here/\",
 \"actor\": {
     \"@type\": \"Person\",
     \"@id\": \"http://tsyesika.co.uk\",
     \"displayName\": \"Jessica Tallon\"},
 \"to\": [\"acct:cwebber@identi.ca\"],
 \"object\": {
     \"@type\": \"Note\",
     \"@id\": \"http://tsyesika.co.uk/chat/sup-yo/\",
     \"content\": \"Up for some root beer floats?\"}}")
(json-to-as json-root-beer-note)
