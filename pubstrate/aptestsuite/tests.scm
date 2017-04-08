(use-modules (srfi srfi-9)
             (oop goops)
             (ice-9 vlist)
             (ice-9 format)
             (ice-9 and-let-star)
             (pubstrate apclient)
             (pubstrate vocab))

;; report item
(define-record-type <ritem>
  (%make-ritem sym req-level desc subitems)
  ritem?
  (sym ritem-sym)
  (req-level ritem-req-level)
  (desc ritem-desc)
  (subitems ritem-subitems))

(define req-levels
  '(MAY MUST SHOULD NON-NORMATIVE))
(define (req-level? obj)
  (member obj req-levels))

(define* (ritem sym req-level desc
                #:key (subitems '()))
  (%make-ritem sym req-level desc (build-ritems subitems)))
(define (build-ritems lst)
  (map (lambda (args) (apply ritem args)) lst))

(define (reverse-compose . args)
  "compose, in reverse"
  (apply compose (reverse args)))

(define-class <response> ()
  (sym #:init-keyword #:sym)
  (comment #:init-keyword #:comment))

(define-class <success> (<response>))
(define-class <fail> (<response>))
(define-class <inconclusive> (<response>))

(define (report-on! report sym response-type . args)
  (hashq-set! report sym
              (apply make response-type
                     #:sym sym
                     args)))

(define server-outbox-items
  (build-ritems
   `(;;; MUST
     (outbox:accepts-activities
      MUST
      "Accepts Activity Objects")
     (outbox:accepts-non-activity-objects
      MUST
      "Accepts non-Activity Objects, and converts to Create Activities per 7.1.1")
     (outbox:removes-bto-and-bcc
      MUST
      "Removes the `bto` and `bcc` properties from Objects before storage and delivery")
     (outbox:ignores-id
      MUST
      "Ignores 'id' on submitted objects, and generates a new id instead")
     (outbox:responds-201-created
      MUST
      "Responds with status code 201 Created")
     (outbox:location-header
      MUST
      "Response includes Location header whose value is id of new object, unless the Activity is transient")
     (outbox:upload-media
      MUST
      "Accepts Uploaded Media in submissions"
      #:subitems ((outbox:upload-media:file-parameter
                   MUST
                   "accepts uploadedMedia file parameter")
                  (outbox:upload-media:object-parameter
                   MUST
                   "accepts uploadedMedia object parameter")
                  (outbox:upload-media:201-or-202-status
                   MUST
                   "Responds with status code of 201 Created or 202 Accepted as described in 6.")
                  (outbox:upload-media:location-header
                   MUST
                   "Response contains a Location header pointing to the to-be-created object's id.")
                  (outbox:upload-media:appends-id
                   MUST
                   "Appends an id property to the new object")))
     (outbox:update
      MUST
      "Update"
      #:subitems ((outbox:update:check-authorized
                   MUST
                   "Server takes care to be sure that the Update is authorized to modify its object before modifying the server's stored copy")))
    ;;; SHOULD
     (outbox:not-trust-submitted
      SHOULD
      "Server does not trust client submitted content")
     (outbox:validate-content
      SHOULD
      "Validate the content they receive to avoid content spoofing attacks.")
     ;; @@: Maybe should be under outbox:upload-media ?
     (outbox:upload-media-url
      SHOULD
      "After receiving submission with uploaded media, the server should include the upload's new URL in the submitted object's url property")
     (outbox:do-not-overload
      SHOULD
      "Take care not to overload other servers with delivery submissions")
     (outbox:create
      SHOULD
      "Create"
      #:subitems ((outbox:create:merges-audience-properties
                   SHOULD
                   "merges audience properties (to, bto, cc, bcc, audience) with the Create's 'object's audience properties")
                  (outbox:create:actor-to-attributed-to
                   SHOULD
                   "Create's actor property is copied to be the value of .object.attributedTo")))
     (outbox:follow
      SHOULD
      "Follow"
      #:subitems ((outbox:follow:adds-followed-object
                   SHOULD
                   "Adds followed object to the actor's Following Collection")))
     (outbox:add
      SHOULD
      "Add"
      #:subitems ((outbox:add:adds-object-to-target
                   SHOULD
                   "Adds object to the target Collection, unless not allowed due to requirements in 7.5")))
     (outbox:remove
      SHOULD
      "Remove"
      #:subitems ((outbox:remove:removes-from-target
                   SHOULD
                   "Remove object from the target Collection, unless not allowed due to requirements in 7.5")))
     (outbox:like
      SHOULD
      "Like"
      #:subitems ((outbox:like:adds-object-to-likes
                   SHOULD
                   "Adds the object to the actor's Likes Collection.")))
     (outbox:block
      SHOULD
      "Block"
      #:subitems ((outbox:block:prevent-interaction-with-actor
                   SHOULD
                   "Prevent the blocked object from interacting with any object posted by the actor."))))))

(define no-location-present-message
  "Couldn't verify since no Location header present in response")

(define* (test-outbox-activity-posted apclient report)
  ;; TODO: [outbox:removes-bto-and-bcc]
  ;;   Maybe?  This is a bit federation'y, requires that we have
  ;;   a server it can talk to
  (define activity-to-submit
    (as:create #:id "http://tsyesika.co.uk/act/foo-id-here/"  ; id should be removed
               #:object (as:note #:id "http://tsyesika.co.uk/chat/sup-yo/"  ; same with object id
                                 #:content "Up for some root beer floats?")))
  
  (define activity-submitted #f)

  (receive (response body)
      (apclient-submit apclient activity-to-submit)
    ;; [outbox:responds-201-created]
    (match (response-code response)
      (201
       (set! activity-submitted #t)
       (report-on! report 'outbox:responds-201-created
                   <success>))
      (other-status-code
       (report-on! report 'outbox:responds-201-created
                   <fail>
                   #:comment (format #f "Responded with status code ~a"
                                     other-status-code))))
    ;; [outbox:location-header]
    (match (response-location response)
      ((? uri? location-uri)
       (set! activity-submitted #t)
       (report-on! report 'outbox:location-header
                   <success>)

       ;; [outbox:ignores-id]
       ;; Now we fetch the object at the location...
       (receive (loc-response loc-asobj)
           (http-get-asobj location-uri)
         (or (and-let* ((is-200 (= (response-code loc-response) 200))
                        (is-asobj (asobj? loc-asobj))
                        (object 
                         (match (asobj-ref loc-asobj "object")
                           ;; nothing there
                           (#f #f)
                           ;; if it's itself an asobj, great
                           ((? asobj? obj) obj)
                           ;; If it looks like it's an identifier, retreive that
                           ;; recursively
                           ((? string? obj)
                            (and=> (string->uri obj)
                                   (lambda (obj-uri)
                                     (receive (obj-response obj-asobj)
                                         (http-get-asobj obj-uri)
                                       (and (= (response-code obj-response) 200) ; ok!
                                            obj-asobj)))))))
                        ;; make sure the id was changed for the outer activity
                        (changed-activity-id
                         (not (equal? (asobj-id loc-asobj)
                                      "http://tsyesika.co.uk/act/foo-id-here/")))
                        ;; ... as well as for the created object
                        (changed-object-id
                         (not (equal? (asobj-id loc-asobj)
                                      "http://tsyesika.co.uk/chat/sup-yo/"))))
               (report-on! report 'outbox:ignores-id
                           <success>))
             (report-on! report 'outbox:ignores-id
                         <fail>))))
      (#f
       (report-on! report 'outbox:location-header
                   <fail>)
       (report-on! report 'outbox:ignores-id
                   <inconclusive>
                   #:comment no-location-present-message)))

    (if activity-submitted
        (report-on! report 'accepts-activities
                    <success>)
        (report-on! report 'accepts-activities
                    <inconclusive>
                    #:comment "Response code neither 200 nor 201, and no Location header present"))))

(define (test-outbox-non-activity apclient report)
  (define activity-to-submit
    (as:note #:content "Up for some root beer floats?"))

  ;; [outbox:accepts-non-activity-objects]
  (receive (response body)
      (apclient-submit apclient activity-to-submit)
    (match (response-location response)
      ((? uri? location-uri)
       (receive (loc-response loc-asobj)
           (http-get-asobj location-uri)
         (if (and (asobj? loc-asobj)
                  (asobj-is-a? loc-asobj ^Create))
             (report-on! report 'accepts-non-activity-objects
                         <success>)
             (report-on! report 'accepts-non-activity-objects
                         <fail>))))
      (#f
       (report-on! report 'accepts-non-activity-objects
                   <inconclusive>
                   #:comment no-location-present-message)))))

(define (test-outbox-verification)
  ;; [outbox:not-trust-submitted]
  ;; [outbox:validate-content]
  )

(define (test-outbox-upload-media)
  ;; [outbox:upload-media]
  ;; [outbox:upload-media:file-parameter]
  ;; [outbox:upload-media:object-parameter]
  ;; [outbox:upload-media:201-or-202-status]
  ;; [outbox:upload-media:location-header]
  ;; [outbox:upload-media:appends-id]
  ;; [outbox:upload-media-url]
  )

(define (test-outbox-update)
  ;; [outbox:update]
  ;; [outbox:update:check-authorized]
  ;; [outbox:not-trust-submitted]
  )

(define (test-outbox-subjective)
  ;; [outbox:do-not-overload]
  )

(define (test-outbox-activity-create)
  ;; [outbox-create]
  ;; [outbox:create:merges-audience-properties]
  ;; [outbox:create:actor-to-attributed-to]
  )

(define (test-outbox-activity-follow)
  ;; [outbox:follow]
  ;; [outbox:follow:adds-followed-object]
  )

(define (test-outbox-activity-add)
  ;; [outbox:add]
  ;; [outbox:add:adds-object-to-target]
  )

(define (test-outbox-activity-remove)
  ;; [outbox:remove]
  ;; [outbox:remove:removes-from-target]
  )

(define (test-outbox-activity-like)
  ;; [outbox:like]
  ;; [outbox:like:adds-object-to-likes]
  )

(define (test-outbox-activity-block)
  ;; [outbox:block]
  ;; [outbox:block:prevent-interaction-with-actor]
  )

;;; "Accept activity submissions and produce correct side effects"
(define* (outbox-tests conn #:optional (report (make-hash-table)))

  )

