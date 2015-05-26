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
  @context @type @id)

;; ========================================
;; Core classes
;; ========================================

(define-class <ASObject> (<json-ldable>)
  ;; http://www.w3.org/ns/activitystreams#Object
  alias attachment attributedTo content
  context displayName endTime generator icon
  image inReplyTo location preview published
  replies scope startTime summary tag title
  updated url)

(define-class <ASLink> (<json-ldable>)
  ;; http://www.w3.org/ns/activitystreams#Link
  href rel mediaType displayName title
  hreflang height width duration)

(define-class <Activity> (<ASObject>)
  ;; http://www.w3.org/ns/activitystreams#Activity
  actor object target result origin priority
  to bto cc bcc)

;; In this one, the actor is a direct object of action as opposed to
;; the object property
(define-class <IntransitiveActivity> (<Activity>)
  ;; http://www.w3.org/ns/activitystreams#IntransitiveActivity
  )

(define-class <Actor> (<ASObject>)
  ;; http://www.w3.org/ns/activitystreams#Actor
  )

(define-class <Collection> (<ASObject>)
  ;; http://www.w3.org/ns/activitystreams#Collection
  totalItems itemsPerPage current next prev
  first last self items)

(define-class <OrderedCollection> (<Collection>)
  ;; http://www.w3.org/ns/activitystreams#OrderedCollection
  startIndex)


;; ========================================
;; Extended classes: Activity types
;; ========================================

(define-class <Accept> (<Respond>)
  ;; http://www.w3.org/ns/activitystreams#Accept
  )

(define-class <TentativeAccept> (<Accept>)
  ;; http://www.w3.org/ns/activitystreams#TentativeAccept
  )

(define-class <Add> (<Activity>)
  ;; http://www.w3.org/ns/activitystreams#Add
  )
