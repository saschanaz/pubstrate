(use-modules (ice-9 match)
             (ice-9 format)
             (ice-9 and-let-star)
             (ice-9 receive)
             (oop goops)
             (sjson utils)
             (8sync)
             (8sync repl)
             (8sync systems websocket server)
             (web uri)
             (web request)
             (pubstrate asobj)
             (pubstrate apclient)
             (pubstrate contrib html)
             (pubstrate webapp utils)
             (pubstrate webapp ctx)
             (pubstrate webapp form-widgets)
             ((pubstrate webapp views)
              #:renamer (symbol-prefix-proc 'ps-view:)))


(define (view:main-display request body)
  (define (one-entry msg)
    `(div (@ (class "stream-entry"))
          (p ,msg)))
  (define body-tmpl
    `((div (@ (id "stream-metabox"))
           (div (@ (id "stream"))))
      (div (@ (id "footer-metabox"))
           (div (@ (class "status-wrapper"))
                (b "ActivityPub Test Suite")
                " :: "
                (span (@ (id "connection-status")
                         (class "disconnected"))
                      "[disconnected]"))
           (p "ActivityPub test suite powered by "
              (a (@ (href "https://gitlab.com/dustyweb/pubstrate"))
                 "Pubstrate")
              "; "
              (a (@ (href "https://www.gnu.org/copyleft/gpl.html"))
                 "GNU General Public License")
              ", version 3 or later."))))
  (define (main-tmpl)
    `(html ;; (@ (xmlns "http://www.w3.org/1999/xhtml"))
           (head (title "ActivityPub test suite")
                 (meta (@ (charset "UTF-8")))
                 (link (@ (rel "stylesheet")
                        (href ,(local-uri "static" "aptestsuite" "aptestsuite.css"))))
                 (script (@ (type "text/javascript")
                            (src ,(local-uri "static" "aptestsuite" "testsuite.js")))
                         ""))
           (body ,@body-tmpl)))
  (respond-html
   (main-tmpl)))

(define (route request)
  (match (split-and-decode-uri-path (uri-path (request-uri request)))
    (() (values view:main-display '()))

    (("static" static-path ...)
     ;; TODO: make this toggle'able
     (values ps-view:render-static
             (list (string-append "/" (string-join
                                       static-path "/")))))

    ;; Not found!
    (_ (values ps-view:standard-four-oh-four '()))))



;;; Web application launching stuff

(define-class <checkpoint> ()
  (kont #:getter .kont
        #:init-keyword #:kont)
  (report-state #:getter .report-state
                #:init-keyword #:report-state))

;; This handles the logic for working with a single client.
;; The <case-manager> handles the actual IO with that user, but delegates
;; to this <case-worker> class to do all the actual state management
;; and run of the tests, etc.
;;
;; When the user's websocket closes, the case-worker for that user
;; shuts down.
(define-actor <case-worker> (<actor>)
  ((receive-input case-worker-receive-input)
   (rewind case-worker-rewind)
   (shutdown case-worker-shutdown)
   (*init* case-worker-init-and-run))
  (client-id #:init-keyword #:client-id
             #:accessor .client-id)
  (manager #:init-keyword #:manager
           #:getter .manager)
  ;; report is an alist
  (report #:init-value '()
          #:accessor .report)
  ;; When we need to get input from a user, we suspend to a continuation.
  ;; If we aren't waiting on user input, this is #f.
  (input-kont #:accessor .input-kont
              #:init-value #f)
  ;; Queued to be added to the checkpoints after next time input is
  ;; received
  (next-checkpoint #:accessor .next-checkpoint
                   #:init-value #f)
  ;; List of checkpoints that can be resumed
  (checkpoints #:init-value '()
               #:accessor .checkpoints)

  ;; ActivityPub client used to connect to the server
  (apclient #:init-value #f
            #:accessor .apclient))

(define (case-worker-receive-input case-worker m input)
  (with-user-io-prompt
   case-worker
   (lambda ()
     ;; Update checkpoints if appropriate
     (and=> (.next-checkpoint case-worker)
            (lambda (checkpoint)
              (set! (.next-checkpoint case-worker) #f)
              (set! (.checkpoints case-worker)
                    (cons checkpoint (.checkpoints case-worker)))))

     (cond ((.input-kont case-worker) =>
            (lambda (kont)
              ;; unset the input-kont
              (set! (.input-kont case-worker) #f)
              ;; and resume!
              (kont input)))
           (else
            (throw 'case-worker-invalid-input #:input input))))))

(define (case-worker-rewind case-worker m)
  "User decided to rewind to a previous prompt."
  (with-user-io-prompt
   case-worker
   (lambda ()
     (show-user "*** Rewinding... ***")))

  (with-user-io-prompt
   case-worker
   (lambda ()
     ;; This requires that we have a prompt up also at the moment,
     ;; and that we have something to resume in our checkpoints.
     (when (not (.input-kont case-worker))
       (throw 'case-worker-resuming-without-prompt
              #:from 'rewind))

     (match (.checkpoints case-worker)
       (()
        (throw 'case-worker-nothing-to-rewind
               "Can't rewind because no checkpoints available"))
       ;; Pop checkpoint off the stack
       ((checkpoint rest-checkpoints ...)
        ;; Pop!
        (set! (.checkpoints case-worker)
              rest-checkpoints)
        ;; Set the "next checkpoint" to be the one we just restored
        (set! (.next-checkpoint case-worker)
              checkpoint)
        ;; reset the report back to the state at this checkpoint
        (set! (.report case-worker) (.report-state checkpoint))
        ;; annnnd... we're back!
        ((.kont checkpoint)))))))

(define (case-worker-shutdown case-worker m)
  (self-destruct case-worker))

(define %user-io-prompt (make-prompt-tag))

(define (with-user-io-prompt case-worker thunk)
  "Call script PROC with IO procedures that are case-worker driven."
  (let lp ((thunk thunk))
    (match (call-with-prompt %user-io-prompt
             thunk
             (match-lambda*
               ((kont '*user-io* payload)
                (set! (.input-kont case-worker)
                      kont)
                (<- (.manager case-worker) 'ws-send
                    (.client-id case-worker)
                    payload))
               ((kont '*save-checkpoint*)
                (set! (.next-checkpoint case-worker)
                      (make <checkpoint>
                        #:kont kont
                        #:report-state (.report case-worker)))
                ;; TODO save checkpoint here
                (list '*call-again* kont))))
      ;; Maybe we didn't need this loop and we cluld have just
      ;; called (with-user-io-prompt) inside the call-with-prompt.
      ;; I'm not positive about whether it would be a proper tail
      ;; call tho...
      (('*call-again* kont)
       (lp kont))
      (_ 'no-op))))

(define (gen-payload case-worker type sxml)
  (write-json-to-string
   `(@ ("type" ,type)
       ("can-go-back" ,(not (eq? (.checkpoints case-worker)
                                 '())))
       ("content" 
        ,(with-output-to-string
           (lambda ()
             (sxml->html sxml (current-output-port))))))))

(define (show-user sxml)
  "Show the user the following SXML.

This must be done within the dynamic execution of both a case worker's
message handling, and within `with-user-io-prompt'."
  (define case-worker (%current-actor))
  (<- (.manager case-worker) 'ws-send
      (.client-id case-worker)
      (gen-payload case-worker "notice" sxml)))

(define* (get-user-input sxml #:key (checkpoint #t))
  "Show user a prompt with SXML, possibly saving a CHECKPOINT

This must be done within the dynamic execution of both a case worker's
message handling, and within `with-user-io-prompt'."
  (define case-worker (%current-actor))
  (if checkpoint
      (abort-to-prompt %user-io-prompt '*save-checkpoint*)
      (set! (.next-checkpoint case-worker) #f))
  (abort-to-prompt %user-io-prompt '*user-io*
                   (gen-payload case-worker "input-prompt" sxml)))

(define (case-worker-drop-top-checkpoint! case-worker)
  "Drop the top checkpoint, in case we're doing something over"
  (match (.checkpoints case-worker)
    ((top-checkpoint rest-checkpoints ...)
     (set! (.checkpoints case-worker)
           rest-checkpoints))
    ;; no checkpoints?  no-op!
    (() #f)))

(define (drop-top-checkpoint!)
  "Shortcut for case-worker-drop-top-checkpoint! using (%current-actor) parameter"
  (case-worker-drop-top-checkpoint! (%current-actor)))


(define (case-worker-init-and-run case-worker m . args)
  (with-user-io-prompt case-worker (lambda () (run-main-script case-worker))))

(define (report-it! case-worker key val)
  (set! (.report case-worker) (acons key val (.report case-worker))))

(define* (report-ref case-worker key #:key dflt)
  (match (assoc key (.report case-worker))
    ((key . val) val)))


;;; Demo, for testing things

(define (demo-script case-worker show-user get-user-input)
  (show-user "Welcome to the deli counter.  What would you like?")

  ;; .-----------------------.
  ;; | Menu:                 |
  ;; |  [X] sandwich         |
  ;; |  [X] drink            |
  ;; |  [ ] dessert          |
  ;; |               [submit]|
  ;; '-----------------------'
  (let ((user-input
         (get-user-input
          `((h2 "What would you like to eat?")
            (p "I hope you like cafeteria food")
            (ul (li (input (@ (name "sandwich")
                              (type "checkbox")))
                    " Sandwich")
                (li (input (@ (name "drink")
                              (type "checkbox")))
                    " Drink")
                (li (input (@ (name "dessert")
                              (type "checkbox")))
                    " Dessert"))))))
    (report-it! case-worker 'sandwich (json-object-ref user-input "sandwich"))
    (report-it! case-worker 'drink (json-object-ref user-input "drink"))
    (report-it! case-worker 'dessert (json-object-ref user-input "dessert"))
    (let ((wanted-str
           (match (delete #f (list
                              (and (json-object-ref user-input "sandwich")
                                   "a sandwich")
                              (and (json-object-ref user-input "drink")
                                   "a drink")
                              (and (json-object-ref user-input "dessert")
                                   "a dessert")))
             ((item)
              (string-append "just " item))
             ((items ... last-item)
              (string-append (string-join
                              (append items
                                      (list (string-append "and " last-item)))
                              ", ")))
             (()
              "nothing apparently!"))))

      (show-user (format #f  "Okay!  So you want... ~a... coming right up!"
                         wanted-str)))
    (get-user-input
     `((h2 "do you get this one though")
       (p "y/n actually just press submit")))
    (show-user "Yeah you got it")
    (get-user-input
     `((h2 "One more")
       (p "we promise")))
    (show-user "and we're done")))


;;; test-items and responses

;; This isn't 
(define-class <test-item> ()
  (sym #:init-keyword #:sym
       #:getter test-item-sym)
  (req-level #:init-keyword #:req-level
             #:getter test-item-req-level)
  (desc #:init-keyword #:desc
        #:getter test-item-desc)
  (subitems #:init-keyword #:subitems
            #:getter test-item-subitems))

(define req-levels
  '(MAY MUST SHOULD NON-NORMATIVE))
(define (req-level? obj)
  (member obj req-levels))

(define* (test-item sym req-level desc
                #:key (subitems '()))
  (make <test-item>
    #:sym sym #:req-levl req-level #:desc desc
    #:subitems (build-test-items subitems)))
(define (build-test-items lst)
  (map (lambda (args) (apply test-item args)) lst))

(define-class <response> ()
  (sym #:init-keyword #:sym)
  (comment #:init-keyword #:comment))

(define-syntax-rule (response-maker name type)
  (define (constructor-name . args)
    (apply make type args)))

(define-class <success> (<response>))
(define-class <fail> (<response>))
(define-class <inconclusive> (<response>))
(response-maker success <success>)
(response-maker fail <fail>)
(response-maker inconclusive <inconclusive>)


;;; Descriptions of the activitystreams requirements

;; This is used to build the implementation report, and is also decent
;; documentation on "what needs to be done."

(define server-outbox-items
  (build-test-items
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





;;; The main script

(define (link url body)
  "Construct a link that opens in a new tab, so the user won't accidentally
leave the tests in progress."
  `(a (@ (href ,url)
         (target "_blank"))
      ,body))

(define (warn . body)
  "Surround text in warning class."
  `(span (@ (class "warning"))
         ,@body))

(define (center-text body)
  `(div (@ (style "text-align: center;"))
        ,body))

(define (run-main-script case-worker)
  ;;; Find out which tests we're running
  (show-user
   `((p "Hello!  Welcome to the "
        ,(link "https://www.w3.org/TR/activitypub/"
               "ActivityPub")
        " test suite, part of "
        ,(link "https://activitypub.rocks/"
               "activitypub.rocks")
        "!")
     (p (i "Please don't close this tab until you've finished submitting "
           "your tests; your session is running as long as the tab stays open."))))

  (let get-input-loop ((checkpoint #t))
    (let* ((user-input
            (get-user-input
             `((h2 "What implementations are we testing today?")
               (table (@ (class "input-table"))
                      (tr (td (input (@ (name "testing-client")
                                        (type "checkbox"))))
                          (td (b "ActivityPub client")
                              (br)
                              "I'm testing an ActivityPub client which speaks the "
                              ,(link "https://www.w3.org/TR/activitypub/#client-to-server-interactions"
                                     "ActivityPub client-to-server protocol")
                              "."))
                      (tr (td (input (@ (name "testing-c2s-server")
                                        (type "checkbox"))))
                          (td (b "ActivityPub client-to-server server")
                              (br)
                              "I'm testing an ActivityPub client which follows the commands "
                              "of clients through the "
                              ,(link "https://www.w3.org/TR/activitypub/#client-to-server-interactions"
                                     "ActivityPub client-to-server protocol")
                              "."))
                      (tr (td (input (@ (name "testing-s2s-server")
                                        (type "checkbox"))))
                          (td (b "ActivityPub federated server")
                              (br)
                              "I'm testing an ActivityPub server which speaks to other servers "
                              "over the "
                              ,(link "https://www.w3.org/TR/activitypub/#server-to-server-interactions"
                                     "ActivityPub server-to-server protocol")
                              "."))))
             #:checkpoint checkpoint))
           (testing-client (json-object-ref user-input "testing-client"))
           (testing-c2s-server (json-object-ref user-input "testing-c2s-server"))
           (testing-s2s-server (json-object-ref user-input "testing-s2s-server")))
      (if (or testing-client testing-c2s-server testing-s2s-server)
          ;; We need at least one to continue
          (begin
            (when testing-client
              (report-it! case-worker 'testing-client #t)
              (test-client case-worker))
            (when testing-c2s-server
              (report-it! case-worker 'testing-c2s-server #t)
              (test-c2s-server case-worker))
            (when testing-s2s-server
              (report-it! case-worker 'teseting-s2s-server #t)
              (test-s2s-server case-worker)))
          ;; We didn't get anything, so let's loop until we do
          (begin (show-user (warn
                             '("It looks like you didn't select anything. "
                               "Please select at least one implementation type to test.")))
                 (drop-top-checkpoint!)
                 (get-input-loop #t)))))

  ;;; TODO: And here's the final report
  )



;;; Client tests

(define (test-client case-worker)
  (show-user "Here's where we'd test the client!")
  (show-user '("We should issue an apology here that the "
               "client testing code asks the most questions. "
               "Server testing code won't have to be as interactive.")))


;;; client-to-server server tests

(define (test-c2s-server case-worker)
  (show-user "Here's where we'd test the server's client-to-server support!")
  (set-up-c2s-server-client-auth case-worker)
  ;;(test-outbox-activity-posted case-worker)
  )

(define (set-up-c2s-server-client-auth case-worker)
  (define (get-user-obj)
    (let* ((user-input
            (get-user-input
             `((h2 "Authorize test client")
               (p "In order to test properly, we need to connect "
                  "as a client to your server.  Enter the address of an actor/user "
                  "(we're going to cause a bit of a mess to their timeline, "
                  "so it should probably be a one-off actor you create just for "
                  "this purpose) along with the authentication information below:")

               (p (i "(Note that this assumes that you're using the "
                     ,(link "https://www.w3.org/TR/activitypub/#authorization-oauth"
                            "OAuth workflow")
                     ".  If you are using another auth workflow, please "
                     ,(link "https://gitlab.com/dustyweb/pubstrate"
                            "file a bug")
                     " so we can get it incorporated into the test suite ASAP!"))
               (dl (dt (b "Actor id (a uri)"))
                   (dd (input (@ (type "text")
                                 (name "actor-id"))))))))
           (user-id (json-object-ref user-input "actor-id"))
           (retry (lambda ()
                    (drop-top-checkpoint!)
                    (get-user-obj))))
      (catch 'invalid-as2
        (lambda ()
          (cond
           ((or (not user-id)
                (not (string->uri user-id)))
            (show-user (warn "Not a valid uri!"))
            (retry))
           (else
            (make-apclient user-id))))
        (lambda _
          (show-user (warn "Sorry, there doesn't seem to be a valid AS2 object "
                           "at that endpoint?"))
          (retry)))))
  ;; Now we need to get their token...
  (define (setup-auth-info apclient)
    (define auth-token-endpoint
      (asobj-ref (apclient-user apclient) '("endpoints" "getAuthToken")))
    (let lp ()
      (let ((user-input
             (get-user-input
              ;; so badly phrased
              `(,(if auth-token-endpoint
                     `(p "Visit "
                         (a (@ (href ,auth-token-endpoint))
                            "this link")
                         " and fill in the auth token")
                     `(p "Unable to find OAuth endpoints... please manually insert "
                         "the auth token:"))
                (dl (dt (b "Auth token"))
                    (dd (input (@ (type "text")
                                  (name "auth-token")))))))))
        (cond ((json-object-ref user-input "auth-token") =>
               (lambda (auth-token)
                 (set! (apclient-auth-token apclient) auth-token)))
              (else
               (drop-top-checkpoint!)
               (lp)))))

    ;; Add the fully setup apclient to the case-worker
    (set! (.apclient case-worker) apclient))

  (show-user (center-text `(h2 "**** Testing server's client-to-server support! ****")))
  (setup-auth-info (get-user-obj))
  (pk '(.apclient case-worker) (.apclient case-worker)))

(define no-location-present-message
  "Couldn't verify since no Location header present in response")

;; (define* (test-outbox-activity-posted case-worker)
;;   ;; TODO: [outbox:removes-bto-and-bcc]
;;   ;;   Maybe?  This is a bit federation'y, requires that we have
;;   ;;   a server it can talk to
;;   (define activity-to-submit
;;     (as:create #:id "http://tsyesika.co.uk/act/foo-id-here/"  ; id should be removed
;;                #:object (as:note #:id "http://tsyesika.co.uk/chat/sup-yo/"  ; same with object id
;;                                  #:content "Up for some root beer floats?")))
  
;;   (define activity-submitted #f)

;;   (receive (response body)
;;       (apclient-submit apclient activity-to-submit)
;;     ;; [outbox:responds-201-created]
;;     (match (response-code response)
;;       (201
;;        (set! activity-submitted #t)
;;        (report-on! report 'outbox:responds-201-created
;;                    <success>))
;;       (other-status-code
;;        (report-on! report 'outbox:responds-201-created
;;                    <fail>
;;                    #:comment (format #f "Responded with status code ~a"
;;                                      other-status-code))))
;;     ;; [outbox:location-header]
;;     (match (response-location response)
;;       ((? uri? location-uri)
;;        (set! activity-submitted #t)
;;        (report-on! report 'outbox:location-header
;;                    <success>)

;;        ;; [outbox:ignores-id]
;;        ;; Now we fetch the object at the location...
;;        (receive (loc-response loc-asobj)
;;            (http-get-asobj location-uri)
;;          (or (and-let* ((is-200 (= (response-code loc-response) 200))
;;                         (is-asobj (asobj? loc-asobj))
;;                         (object 
;;                          (match (asobj-ref loc-asobj "object")
;;                            ;; nothing there
;;                            (#f #f)
;;                            ;; if it's itself an asobj, great
;;                            ((? asobj? obj) obj)
;;                            ;; If it looks like it's an identifier, retreive that
;;                            ;; recursively
;;                            ((? string? obj)
;;                             (and=> (string->uri obj)
;;                                    (lambda (obj-uri)
;;                                      (receive (obj-response obj-asobj)
;;                                          (http-get-asobj obj-uri)
;;                                        (and (= (response-code obj-response) 200) ; ok!
;;                                             obj-asobj)))))))
;;                         ;; make sure the id was changed for the outer activity
;;                         (changed-activity-id
;;                          (not (equal? (asobj-id loc-asobj)
;;                                       "http://tsyesika.co.uk/act/foo-id-here/")))
;;                         ;; ... as well as for the created object
;;                         (changed-object-id
;;                          (not (equal? (asobj-id loc-asobj)
;;                                       "http://tsyesika.co.uk/chat/sup-yo/"))))
;;                (report-on! report 'outbox:ignores-id
;;                            <success>))
;;              (report-on! report 'outbox:ignores-id
;;                          <fail>))))
;;       (#f
;;        (report-on! report 'outbox:location-header
;;                    <fail>)
;;        (report-on! report 'outbox:ignores-id
;;                    <inconclusive>
;;                    #:comment no-location-present-message)))

;;     (if activity-submitted
;;         (report-on! report 'accepts-activities
;;                     <success>)
;;         (report-on! report 'accepts-activities
;;                     <inconclusive>
;;                     #:comment "Response code neither 200 nor 201, and no Location header present"))))


;;; server to server server tests

(define (test-s2s-server case-worker)
  (show-user "Here's where we'd test the server's federation support!"))


;;; case manager / case worker stuff

(define (receive-input case-worker m input)
  (match (.input-kont case-worker)
    ;; Ignore if we don't have input
    (#f #f)
    ;; Otherwise, resume with this input provided
    (kont (kont input))))

;; This is really the combo web server / websocket server.
;; It's called case-manager because of bad puns; case-worker is delegated
;; to for all the communication with the client over the websocket.
(define-actor <case-manager> (<websocket-server>)
  ()
  (workers #:init-thunk make-hash-table
           #:accessor .workers)
  ;; This is a kludge... we really shouldn't have to double
  ;; record these, should we?
  (client-worker-map #:init-thunk make-hash-table
                     #:accessor .client-worker-map))

(define (case-manager-ws-client-connect case-manager client-id)
  (pk 'connected!)
  (let ((worker (create-actor case-manager <case-worker>
                              #:client-id client-id
                              #:manager (actor-id case-manager))))
    (hash-set! (.workers case-manager)
               client-id worker)))

(define* (case-manager-worker-ref case-manager client-id
                                  #:key (error-on-nothing #t))
  ")Fetch worker with CLIENT-ID.

If ERROR-ON-NOTHING, error out if worker is not found."
  (let ((worker (hash-ref (.workers case-manager) client-id)))
    (when (and (not worker) error-on-nothing)
      (throw 'worker-not-found "No worker with that id!"
             #:client-id client-id))
    worker))


(define (case-manager-ws-client-disconnect case-manager client-id)
  (pk 'disconnected!)
  (let ((worker (case-manager-worker-ref case-manager client-id)))
    (<- worker 'shutdown)
    (hash-remove! (.workers case-manager) client-id)))

(define (case-manager-ws-new-message case-manager client-id raw-data)
  (let ((worker (case-manager-worker-ref case-manager client-id))
        ;; shadow data with version parsed from json
        (json-data (read-json-from-string raw-data)))
    (match (json-object-ref json-data "action")
      ("send-input"
       (<- worker 'receive-input (json-object-ref json-data "data")))
      ("rewind"
       (<- worker 'rewind)))))

(define (http-handler request body)
  (receive (view args)
      (route request)
    (apply view request body args)))

(define (main . args)
  (define hive (make-hive))
  (with-extended-ctx
   ;; TODO: fixme...
   `((base-uri . ,(string->uri "http://localhost:8989/")))
   (lambda ()
     (bootstrap-actor hive <case-manager>
                      #:http-handler (wrap-apply http-handler)
                      #:port 8989
                      #:on-ws-client-connect (wrap-apply case-manager-ws-client-connect)
                      #:on-ws-client-disconnect (wrap-apply case-manager-ws-client-disconnect)
                      #:on-ws-message (wrap-apply case-manager-ws-new-message))
     (bootstrap-actor hive <repl-manager>)
     (run-hive hive '()))))
