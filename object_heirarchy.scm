;; Just an experiment to define the full set of relationships in AS2.0
;; as classes.. not actually intended to be used

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
