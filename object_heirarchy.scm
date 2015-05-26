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

;;; Just an experiment to define the full set of relationships in AS2.0
;;; as classes.. not actually intended to be used

(use-modules (oop goops))

(define-class <json-ldable> ()
  (properties #:allocation #:each-subclass)
  ;; (mandatory #:allocation #:each-subclass)
  (uri #:allocation #:each-subclass)
  )
(class-slot-set! <json-ldable> 'properties
                 '(@context @type @id))

;; emacs: (put 'define-asclass 'scheme-indent-function 2)

;;; This is a shortcut for defining activitystreams-style objects
;;; since there is often a lot of boilerplate

(define-syntax-rule (define-asclass class (parent ...)
                      as-uri as-properties . rest)
  (begin
    (define-class class (parent ...)
      . rest)
    (class-slot-set! class 'uri as-uri)
    (class-slot-set! class 'properties as-properties)))

(define-generic render-as-json)

;; Utility to gather the list of all 

(define* (gather-properties class #:optional (prop 'properties))
  "Gather available properties recursively from AS definitions

Use like:
  (gather-properties <Activity>)"
  (let ((listy-results
         (catch 'goops-error
           (lambda () (class-slot-ref class prop))
           (lambda (key . args) #:unbound))))
    (if (eq? #:unbound listy-results)
        '()
        (apply append
               listy-results
               (map (lambda (class) (gather-properties class prop))
                    (class-direct-supers class))))))


;; ========================================
;; Core classes
;; ========================================

(define-asclass <ASObject> (<json-ldable>)
  "http://www.w3.org/ns/activitystreams#Object"
  '(alias attachment attributedTo content
          context displayName endTime generator icon
          image inReplyTo location preview published
          replies scope startTime summary tag title
          updated url))

(define-asclass <ASLink> (<json-ldable>)
  "http://www.w3.org/ns/activitystreams#Link"
  '(href rel mediaType displayName title
         hreflang height width duration))


(define-asclass <ASLink> (<json-ldable>)
  "http://www.w3.org/ns/activitystreams#Link"
  '(href rel mediaType displayName title
         hreflang height width duration))

(define-asclass <Activity> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Activity"
  '(actor object target result origin priority
          to bto cc bcc))

;;; In this one, the actor is a direct object of action as opposed to
;;; the object property
;;; 
;;; ... technically this does NOT inherit the "object" field, but our
;;; algorithm for determining available properties assumes it does...

(define-asclass <IntransitiveActivity> (<Activity>)
  "http://www.w3.org/ns/activitystreams#IntransitiveActivity"
  '())

(define-asclass <Actor> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Actor"
  '())

(define-asclass <Collection> (<ASObject>)
  "http://www.w3.org/ns/activitystreams#Collection"
  '(totalItems itemsPerPage current next prev
               first last self items))

(define-asclass <OrderedCollection> (<Collection>)
  "http://www.w3.org/ns/activitystreams#OrderedCollection"
  '(startIndex))


;; ========================================
;; Extended classes: Activity types
;; ========================================

(define-asclass <Accept> (<Respond>)
  "http://www.w3.org/ns/activitystreams#Accept"
  '())

(define-asclass <TentativeAccept> (<Accept>)
  "http://www.w3.org/ns/activitystreams#TentativeAccept"
  '())

(define-asclass <Add> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Add"
  '())

(define-asclass <Arrive> (<IntransitiveActivity>)
  "http://www.w3.org/ns/activitystreams#Arrive"
  '())

(define-asclass <Create> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Create"
  '())

(define-asclass <Delete> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Delete"
  '())

(define-asclass <Favorite> (<Respond>)
  "http://www.w3.org/ns/activitystreams#Favorite"
  '())

(define-asclass <Follow> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Follow"
  '())

(define-asclass <Ignore> (<Respond>)
  "http://www.w3.org/ns/activitystreams#Ignore"
  '())

(define-asclass <Join> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Join"
  '())

(define-asclass <Leave> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Leave"
  '())

(define-asclass <Connect> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Connect"
  '())

(define-asclass <FriendRequest> (<Connect>)
  "http://www.w3.org/ns/activitystreams#FriendRequest"
  '())

(define-asclass <Give> (<Offer>)
  "http://www.w3.org/ns/activitystreams#Give"
  '())

(define-asclass <Invite> (<Offer>)
  "http://www.w3.org/ns/activitystreams#Invite"
  '())

(define-asclass <Post> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Post"
  '())

(define-asclass <Reject> (<Respond>)
  "http://www.w3.org/ns/activitystreams#Reject"
  '())

(define-asclass <TentativeReject> (<Reject>)
  "http://www.w3.org/ns/activitystreams#TentativeReject"
  '())

(define-asclass <Remove> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Remove"
  '())

(define-asclass <Review> (<Respond>)
  "http://www.w3.org/ns/activitystreams#Review"
  '())

(define-asclass <Save> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Save"
  '())

(define-asclass <Share> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Share"
  '())

(define-asclass <Undo> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Undo"
  '())

(define-asclass <Update> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Update"
  '())

(define-asclass <View> (<Experience>)
  "http://www.w3.org/ns/activitystreams#View"
  '())

;;; I'd love to see this one dropped :P
;;; see: https://github.com/jasnell/w3c-socialwg-activitystreams/issues/113
(define-asclass <Watch> (<View>)
  "http://www.w3.org/ns/activitystreams#Watch"
  '())

;;; I'd love to see this one become a subclass of Read
;;; see: https://github.com/jasnell/w3c-socialwg-activitystreams/issues/114
(define-asclass <Read> (<View>)
  "http://www.w3.org/ns/activitystreams#Read"
  '())

(define-asclass <Respond> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Respond"
  '())

(define-asclass <Move> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Move"
  '())

(define-asclass <Travel> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Travel"
  '())

(define-asclass <Announce> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Announce"
  '())

(define-asclass <Block> (<Ignore>)
  "http://www.w3.org/ns/activitystreams#Block"
  '())

(define-asclass <Flag> (<Respond>)
  "http://www.w3.org/ns/activitystreams#Flag"
  '())

(define-asclass <Dislike> (<Respond>)
  "http://www.w3.org/ns/activitystreams#Dislike"
  '())

(define-asclass <Confirm> (<Respond>)
  "http://www.w3.org/ns/activitystreams#Confirm"
  '())

(define-asclass <Assign> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Assign"
  '())

(define-asclass <Complete> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Complete"
  '())

(define-asclass <Achieve> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Achieve"
  '())

(define-asclass <Claim> (<Activity>)
  "http://www.w3.org/ns/activitystreams#Claim"
  '(proof))
