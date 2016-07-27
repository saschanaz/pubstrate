;; Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;; Experimenting with api stuff in here

(define root-beer-note
  (make-as <Create>
    ;; Putting the string first is the same thing as #:@id
    "http://tsyesika.co.uk/act/foo-id-here/"
    #:actor (make-as <Person>
              #:@id "http://tsyesika.co.uk"
              #:displayName "Jessica Tallon")
    #:to (list "acct:cwebber@identi.ca")
    #:object (make-as <Note>
               "http://tsyesika.co.uk/chat/sup-yo/"
               #:content "Up for some root beer floats?")))

(as->json root-beer-note)

(define json-root-beer-note
  "{\"@type\": \"Create\",
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

;; Convert it back: (json->as-obj json-root-beer-note)


(pprint-json (expand (read-json-from-string "{
  \"@context\": {
    \"ical\": \"http://www.w3.org/2002/12/cal/ical#\",
    \"xsd\": \"http://www.w3.org/2001/XMLSchema#\",
    \"ical:dtstart\": {
      \"@type\": \"xsd:dateTime\"
    }
  },
  \"ical:summary\": \"Lady Gaga Concert\",
  \"ical:location\": \"New Orleans Arena, New Orleans, Louisiana, USA\",
  \"ical:dtstart\": \"2011-04-09T20:00Z\"
}")) (current-output-port))


(define json-test-2 (read-json-from-string "{
  \"@context\": {
    \"name\": \"http://rdf.data-vocabulary.org/#name\",
    \"ingredient\": \"http://rdf.data-vocabulary.org/#ingredients\",
    \"yield\": \"http://rdf.data-vocabulary.org/#yield\",
    \"instructions\": \"http://rdf.data-vocabulary.org/#instructions\",
    \"step\": {
      \"@id\": \"http://rdf.data-vocabulary.org/#step\",
      \"@type\": \"xsd:integer\"
    },
    \"description\": \"http://rdf.data-vocabulary.org/#description\",
    \"xsd\": \"http://www.w3.org/2001/XMLSchema#\"
  },
  \"name\": \"Mojito\",
  \"ingredient\": [
    \"12 fresh mint leaves\",
    \"1/2 lime, juiced with pulp\",
    \"1 tablespoons white sugar\",
    \"1 cup ice cubes\",
    \"2 fluid ounces white rum\",
    \"1/2 cup club soda\"
  ],
  \"yield\": \"1 cocktail\",
  \"instructions\": [
    {
      \"step\": 1,
      \"description\": \"Crush lime juice, mint and sugar together in glass.\"
    },
    {
      \"step\": 2,
      \"description\": \"Fill glass to top with ice cubes.\"
    },
    {
      \"step\": 3,
      \"description\": \"Pour white rum over ice.\"
    },
    {
      \"step\": 4,
      \"description\": \"Fill the rest of glass with club soda, stir.\"
    },
    {
      \"step\": 5,
      \"description\": \"Garnish with a lime wedge.\"
    }
  ]
}"))
