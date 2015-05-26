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
  (fields #:allocation #:each-subclass)
  ;; (mandatory #:allocation #:each-subclass)
  (uri #:allocation #:each-subclass)
  )
(class-slot-set! <json-ldable> 'fields
                 '(@context @type @id))

;; emacs: (put 'define-asclass 'scheme-indent-function 2)

;;; This is a shortcut for defining activitystreams-style objects
;;; since there is often a lot of boilerplate

(define-syntax-rule (define-asclass class (parent ...)
                      as-uri as-fields . rest)
  (begin
    (define-class class (parent ...)
      . rest)
    (class-slot-set! class 'uri as-uri)
    (class-slot-set! class 'fields as-fields)))

(define-generic render-as-json)

;; Utility to gather the list of all 

(define* (gather-fields class #:optional (prop 'fields))
  "Gather available fields recursively from AS definitions

Use like:
  (gather-fields <Activity>)"
  (let ((listy-results
         (catch 'goops-error
           (lambda () (class-slot-ref class prop))
           (lambda (key . args) #:unbound))))
    (if (eq? #:unbound listy-results)
        '()
        (apply append
               listy-results
               (map (lambda (class) (gather-fields class prop))
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

