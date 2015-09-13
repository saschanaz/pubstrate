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
