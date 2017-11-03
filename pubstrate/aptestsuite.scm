;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

(use-modules (gcrypt random)
             (gcrypt hash)
             (gdbm)
             (ice-9 control)
             (ice-9 match)
             (ice-9 format)
             (ice-9 and-let-star)
             (ice-9 receive)
             (oop goops)
             (sjson utils)
             (8sync)
             (8sync repl)
             (8sync systems websocket server)
             (fibers conditions)
             (rnrs bytevectors)
             (srfi srfi-1)   ; list utils
             (srfi srfi-19)  ; dates
             (srfi srfi-11)
             (srfi srfi-26)  ; cut
             (srfi srfi-41)  ; streams
             (web http)
             (web uri)
             (web request)
             (web response)
             (webutils date)
             (pubstrate contrib base32)
             (pubstrate asobj)
             (pubstrate apclient)
             (pubstrate contrib html)
             (pubstrate contrib define-method-star)
             (pubstrate vocab)
             (pubstrate package-config)
             (pubstrate webapp utils)
             (pubstrate webapp ctx)
             (pubstrate webapp form-widgets)
             ((pubstrate webapp views)
              #:renamer (symbol-prefix-proc 'ps-view:))
             ((pubstrate shorthand)
              #:renamer (symbol-prefix-proc 'as:)))

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

(define (send-to-pseudoactor request body client-id pseudoactor-name rest-paths)
  (define pseudoactor
    (and=>
     (hash-ref (.client-pseudoactors (*current-actor*))
               (string->number client-id))
     (cut hash-ref <> pseudoactor-name)))

  (if pseudoactor
      (<-wait pseudoactor 'pseudoactor-view
              rest-paths request body)
      (respond-not-found)))

(define (download-report request body db-key)
  (let ((doc (<-wait (.db-manager (*current-actor*)) 'get-document
                     db-key)))
    (if doc
        (respond doc #:content-type 'text/json)
        (respond-not-found))))

(define (route request)
  (match (split-and-decode-uri-path (uri-path (request-uri request)))
    (() (values view:main-display '()))

    ;; emulating activitypub actors and endpoints
    (("ap" "u" client-id pseudoactor rest-paths ...)
     (values send-to-pseudoactor (list client-id pseudoactor rest-paths)))

    (("download-report" db-key)
     (values download-report (list db-key)))

    ;; static files 
    (("static" static-path ...)
     ;; TODO: make this toggle'able
     (values ps-view:render-static (list static-path)))

    ;; Not found!
    (_ (values ps-view:standard-four-oh-four '()))))


;;; Database manager
(define-actor <db-manager> (<actor>)
  ((store-document! db-manager-store-document!)
   (get-document db-manager-get-document))
  (db-path #:init-keyword #:db-path
           #:getter .db-path)
  (db #:init-keyword #:db
      #:accessor .db))

(define-method (actor-init! (db-manager <db-manager>))
  (set! (.db db-manager)
        (gdbm-open (.db-path db-manager) GDBM_WRCREAT)))

(define-method (actor-cleanup! (db-manager <db-manager>))
  (gdbm-close (.db db-manager)))

(define (db-manager-store-document! db-manager m doc)
  "Store DOC, a string, in db-manager's database"
  (let ((key (bytevector->base32-string (sha256 (string->utf8 doc)))))
    (gdbm-set! (.db db-manager) key doc)
    key))

(define (db-manager-get-document db-manager m key)
  "Fetch document matching KEY from db-manager's gdbm database"
  (gdbm-ref (.db db-manager) key))


;;; Web application launching stuff

(define-class <checkpoint> ()
  (kont #:getter .kont
        #:init-keyword #:kont)
  (report-state #:getter .report-state
                #:init-keyword #:report-state))

;; Random username generator
(define username-bits
  #("apple" "banana" "orange" "grape" "pineapple" "mango" "coconut"
    "blackberry" "strawberry" "cherry" "blueberry"
    "robot" "robo" "cyber" "tron"
    "kitten" "puppy"
    "boom" "bonk" "boink"
    "red" "orange" "yellow" "green" "blue" "purple"
    "evan" "jessica" "chris" "karen" "mike" "nathan" "tim" "bonnie"
    "eva" "alyssa" "ben" "lem" "cy"
    "lisp" "scheme" "hy" "racket" "planner" "schemer" "conniver"
    "python" "js" "csharp" "mono" "jvm" "perl" "elisp"
    "vim" "emacs"
    "xkobo" "wesnoth" "supertux" "neverball" "crawl"))

(define (random-username-bit)
  (vector-ref username-bits
              (random (vector-length username-bits))))

(define random-username
  (let ((counter 0))
    (lambda ()
      (set! counter (1+ counter))
      (string-append (string-capitalize (random-username-bit))
                     (string-capitalize (random-username-bit))
                     (number->string counter)))))

;;; These are "actors" (and only pseudo-) in the ActivityPub sense.
;;; Basically we need to collect some information 
(define-actor <pseudoactor> (<actor>)
  ((get-username
    (lambda (actor m)
      (.username actor)))
   (get-client-id
    (lambda (actor m)
      (.client-id actor)))
   (get-inbox
    (lambda (actor m)
      (.inbox actor)))
   (get-outbox
    (lambda (actor m)
      (.outbox actor)))
   (get-pseudoactor-id
    (lambda (actor m)
      (%pseudoactor-id actor)))
   (asobj->outbox! %pseudoactor-asobj->outbox!)
   (pseudoactor-view pseudoactor-view)
   (pseudoactor-asobj %pseudoactor-asobj)
   (shutdown
    (lambda (actor m)
      (self-destruct actor))))
  (username #:init-thunk random-username
            #:accessor .username)
  ;; used for constructing the url
  (client-id #:init-keyword #:client-id
             #:accessor .client-id)
  (inbox #:init-value '()
         #:accessor .inbox)
  ;; A mapping of id -> foo
  (outbox #:init-thunk make-hash-table
          #:accessor .outbox))

(define-method (pseudoactor-url (pseudoactor <pseudoactor>) path)
  "PATH is a list of additional path-parts"
  (uri->string
   (uri-set (ctx-ref 'base-uri)
            #:path (string-append
                    "/"
                    (string-join (append (list "ap" "u"
                                               (number->string (.client-id pseudoactor))
                                               (.username pseudoactor))
                                         path)
                                 "/")))))


(define-method (%pseudoactor-id (pseudoactor <pseudoactor>))
  (pseudoactor-url pseudoactor '()))

(define (pseudoactor-id to-address)
  (<-wait to-address 'get-pseudoactor-id))

(define-method (%pseudoactor-asobj (pseudoactor <pseudoactor>))
  (as:person #:id (pseudoactor-id pseudoactor)
             #:name (.username pseudoactor)
             #:preferredUsername (.username pseudoactor)
             #:inbox (pseudoactor-url pseudoactor '("inbox"))
             #:outbox (pseudoactor-url pseudoactor '("outbox"))))

(define (pseudoactor-asobj pseudoactor m)
  (%pseudoactor-asobj pseudoactor))

(define-method (%pseudoactor-asobj->outbox! (pseudoactor <pseudoactor>) m
                                            asobj)
  "Save asobj to outbox and assign an id to it, returning asobj saved with id."
  (define post-id (random-token))
  (define id-uri (pseudoactor-url pseudoactor `("post" ,post-id)))
  (define new-asobj
    (asobj-set asobj "id" id-uri))
  (hash-set! (.outbox pseudoactor) post-id new-asobj)
  new-asobj)

(define (pseudoactor-asobj->outbox! actor-address asobj)
  (<-wait actor-address 'asobj->outbox! asobj))

(define-method (pseudoactor-view-user-page (pseudoactor <pseudoactor>)
                                           request body)
  (values (build-response
           #:code 200
           #:headers '((content-type . (application/activity+json))))
          (asobj->string (%pseudoactor-asobj pseudoactor))))

(define-method (pseudoactor-view-inbox (pseudoactor <pseudoactor>)
                                       request body)
  (match (request-method request)
    ('GET
     'TODO)
    ('POST
     (let ((asobj (string->asobj
                   (if (bytevector? body)
                       (utf8->string body)
                       body)
                   (%default-env))))
       (hash-set! (.outbox pseudoactor)
                  (asobj-id asobj) asobj)
       (respond "")))))

(define-method (pseudoactor-view-outbox (pseudoactor <pseudoactor>)
                                        request body)
  'TODO)

(define-method (pseudoactor-view-post (pseudoactor <pseudoactor>)
                                      request body post-id)
  (let ((asobj (hash-ref (.outbox pseudoactor)
                         post-id)))
    (respond (asobj->string asobj)
             #:content-type 'application/activity+json)))

(define-method (pseudoactor-route (pseudoactor <pseudoactor>) path)
  (match path
    (() (values pseudoactor-view-user-page '()))
    (("inbox") (values pseudoactor-view-inbox '()))
    (("outbox") (values pseudoactor-view-outbox '()))
    (("post" post-id) (values pseudoactor-view-post (list post-id)))))

(define (pseudoactor-view pseudoactor m path request body)
  (let-values (((view args)
                (pseudoactor-route pseudoactor path)))
    (if pseudoactor
        (apply view pseudoactor request body args)
        (respond-not-found))))


;;; Clearing out blocked ports
;;;
;;; Why have a "reclusive actor?"
;;; Some context here... in 8sync/fibers/etc, ports normally shouldn't
;;; block because we use ice-9 suspendable-ports.  Unfortunately
;;; that's not the reality for anything that uses https / ssl; such
;;; operations are wrapped in a gnutls custom-binary-i/o-port, which
;;; doesn't (yet) support suspendable ports.
;;;
;;; The following "solution" works around things by putting the actor
;;; in its own hive and thread.  It can still interact with other
;;; actors.
;;;
;;; If this looks like a terrible kludge, that's because it is.  The right
;;; thing to do is to make custom-binary-i/o ports suspendable.  More on
;;; work toward that here:
;;;   https://lists.gnu.org/archive/html/guile-devel/2017-07/msg00014.html

(use-modules (ice-9 threads)
             (fibers channels))

;;; This is a kind of actor that is spawned in its own thread+hive,
;;; and upon self-destruction, destroys that thread+hive.
(define-actor <reclusive-actor> (<actor>)
  ()
  (halt-condition #:init-keyword #:halt-condition
                  #:accessor .halt-condition))

(define-method (actor-cleanup! (actor <reclusive-actor>))
  (next-method)
  (signal-condition! (.halt-condition actor)))

(define (create-reclusive-actor actor-class . args)
  (define retrieve-address-channel
    (make-channel))
  (call-with-new-thread
   (lambda ()
     (let ((halt-condition (make-condition)))
       (run-hive
        (lambda (hive)
          (put-message retrieve-address-channel
                       (apply create-actor actor-class #:halt-condition halt-condition args))
          (wait halt-condition))))))
  (get-message retrieve-address-channel))



;; This handles the logic for working with a single client.
;; The <case-manager> handles the actual IO with that user, but delegates
;; to this <case-worker> class to do all the actual state management
;; and run of the tests, etc.
;;
;; When the user's websocket closes, the case-worker for that user
;; shuts down.
(define-actor <case-worker> (<reclusive-actor>)
  ((receive-input case-worker-receive-input)
   (rewind case-worker-rewind)
   (shutdown case-worker-shutdown)
   ;; (pseudoactor-view case-worker-pseudoactor-view)
   (start-script case-worker-start-script))
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
            #:accessor .apclient)
  (testing-client? #:init-value #f
                   #:accessor .testing-client?)
  (testing-c2s-server? #:init-value #f
                       #:accessor .testing-c2s-server?)
  (testing-s2s-server? #:init-value #f
                       #:accessor .testing-s2s-server?)
  (db-manager #:init-keyword #:db-manager
              #:getter .db-manager)
  ;; "extra debugging" mode
  (debug #:init-keyword #:debug
         #:accessor .debug
         #:init-value #f))

(define (case-worker-pseudoactor-new! case-worker)
  (define pseudoactor
    (<-wait (.manager case-worker) 'create-pseudoactor
            (.client-id case-worker)))
  pseudoactor)

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
             (lambda ()
               (thunk)
               'done)
             (lambda args
               (match args
                 ((kont '*user-io* payload)
                  (set! (.input-kont case-worker)
                        kont)
                  (<- (.manager case-worker) 'ws-send
                      (.client-id case-worker)
                      payload)
                  ;; return 'done to match
                  'done)
                 ((kont '*save-checkpoint*)
                  (set! (.next-checkpoint case-worker)
                        (make <checkpoint>
                          #:kont kont
                          #:report-state (.report case-worker)))
                  ;; TODO save checkpoint here
                  (list '*call-again* kont)))))
      ;; Maybe we didn't need this loop and we cluld have just
      ;; called (with-user-io-prompt) inside the call-with-prompt.
      ;; I'm not positive about whether it would be a proper tail
      ;; call tho...
      (('*call-again* kont)
       (lp (lambda ()
             (kont #f))))
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

(define* (show-user sxml #:key (case-worker (*current-actor*)))
  "Show the user the following SXML.

This must be done within the dynamic execution of both a case worker's
message handling, and within `with-user-io-prompt'."
  (<- (.manager case-worker) 'ws-send
      (.client-id case-worker)
      (gen-payload case-worker "notice" sxml)))

(define* (get-user-input sxml #:key
                         (checkpoint #t)
                         (case-worker (*current-actor*)))
  "Show user a prompt with SXML, possibly saving a CHECKPOINT

This must be done within the dynamic execution of both a case worker's
message handling, and within `with-user-io-prompt'."
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
  "Shortcut for case-worker-drop-top-checkpoint! using (*current-actor*) parameter"
  (case-worker-drop-top-checkpoint! (*current-actor*)))

(define-method (actor-init! (case-worker <case-worker>))
  (<- case-worker 'start-script))

(define (case-worker-start-script case-worker m . args)
  (with-user-io-prompt case-worker (lambda () (run-main-script case-worker))))

(define (report-it! case-worker key val)
  (set! (.report case-worker) (acons key val (.report case-worker))))

(define* (report-ref case-worker key #:key dflt)
  (match (assoc key (.report case-worker))
    ((key . val) val)
    (_ dflt)))



;;; test-items and responses

(define-class <test-item> ()
  (sym #:init-keyword #:sym
       #:getter test-item-sym)
  (req-level #:init-keyword #:req-level
             #:getter test-item-req-level)
  (desc #:init-keyword #:desc
        #:getter test-item-desc)
  (subitems #:init-keyword #:subitems
            #:getter test-item-subitems
            #:init-value '()))

(define req-levels
  '(MAY MUST SHOULD NON-NORMATIVE))
(define (req-level? obj)
  (member obj req-levels))

(define* (test-item sym req-level desc
                #:key (subitems '()))
  (make <test-item>
    #:sym sym #:req-level req-level #:desc desc
    #:subitems (build-test-items subitems)))
(define (build-test-items lst)
  (map (lambda (args) (apply test-item args)) lst))

(define-class <response> ()
  (type-for-report #:allocation #:class
                   #:getter .type-for-report)
  (sym #:init-keyword #:sym
       #:accessor .sym)
  (comment #:init-keyword #:comment
           #:accessor .comment
           #:init-value #f))

(define-syntax-rule (response-maker name type)
  (define (name . args)
    (apply make type args)))

(define (response-test-item response)
  (get-test-item (.sym response)))

(define-syntax-rule (define-response-class cls report-type)
  (define-class cls (<response>)
    (type-for-report #:allocation #:class
                     #:getter .type-for-report
                     #:init-value report-type)))

(define-response-class <success> "yes")
(define-response-class <fail> "no")
(define-response-class <inconclusive> "inconclusive")
(define-response-class <not-applicable> "not-applicable")

(define (report-on! sym response-type . args)
  (define case-worker (*current-actor*))
  (report-it! case-worker sym
              (apply make response-type
                     #:sym sym
                     args)))

(define-syntax-rule (report-protected items body ...)
  (begin
   (catch #t
     (lambda ()
       (catch 'report-abort
         (lambda ()
           body ...)
         (lambda (key reason)
           (for-each
            (lambda (item)
              (when (not (report-ref (*current-actor*) item))
                (report-on! item <fail>
                            #:comment reason)))
            items))))
     (lambda _
       (for-each
        (lambda (item)
          (when (not (report-ref (*current-actor*) item))
            (report-on! item <inconclusive>
                        #:comment "Unexpected server error while running test!")))
        items))
     (let ((err (current-error-port)))
       (lambda (key . args)
         (false-if-exception
          (let ((stack (make-stack #t 4)))
            (display-backtrace stack err)
            (print-exception err (stack-ref stack 0)
                             key args))))))
   (for-each
    (lambda (item)
      (when (not (report-ref (*current-actor*) item))
        (report-on! item <inconclusive>
                    #:comment "Test case not reported on...")))
    items)))

(define-syntax-rule (with-report items body ...)
  (begin
    (report-protected items body ...)
    (for-each (lambda (item)
                (show-response item))
              items)))

(define (log-wrapper . content)
  `(div (@ (class "test-log"))
        ,content))

(define-method (response-as-log-result-text (response <success>))
  `(span (@ (class "result-log-success"))
         "Yes"))

(define-method (response-as-log-result-text (response <fail>))
  `(span (@ (class "result-log-fail"))
         "No"))

(define-method (response-as-log-result-text (response <inconclusive>))
  `(span (@ (class "result-log-inconclusive"))
         "Inconclusive"))

(define-method (response-as-log-result-text (response <not-applicable>))
  `(span (@ (class "result-log-not-applicable"))
         "N/A"))

(define-method (response-as-log (response <response>))
  (log-wrapper (test-item-desc (response-test-item response))
               ": " (response-as-log-result-text response)
               (if (.comment response)
                   `((br)
                     (span (@ (class "test-log-comment"))
                           ,(.comment response)))
                   '())))

(define (show-response sym)
  (define case-worker (*current-actor*))
  (define response (assoc-ref (.report case-worker) sym))
  (if response
      (show-user (response-as-log response))
      (show-user
       (log-wrapper
        (warn "Something went wrong... test didn't run for: "
              (symbol->string sym))))))



;;; Descriptions of the activitystreams requirements

;; This is used to build the implementation report, and is also decent
;; documentation on "what needs to be done."

(define c2s-server-items
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

     ;; (outbox:upload-media
     ;;  MUST
     ;;  "Accepts Uploaded Media in submissions"
     ;;  #:subitems ((outbox:upload-media:file-parameter
     ;;               MUST
     ;;               "accepts uploadedMedia file parameter")
     ;;              (outbox:upload-media:object-parameter
     ;;               MUST
     ;;               "accepts uploadedMedia object parameter")
     ;;              (outbox:upload-media:201-or-202-status
     ;;               MUST
     ;;               "Responds with status code of 201 Created or 202 Accepted as described in 6.")
     ;;              (outbox:upload-media:location-header
     ;;               MUST
     ;;               "Response contains a Location header pointing to the to-be-created object's id.")
     ;;              (outbox:upload-media:appends-id
     ;;               MUST
     ;;               "Appends an id property to the new object")
     ;;              (outbox:upload-media:url
     ;;               SHOULD
     ;;               "After receiving submission with uploaded media, the server should include the upload's new URL in the submitted object's url property")))
     (outbox:update
      MUST
      "Update"
      ;; #:subitems ((outbox:update:check-authorized
      ;;              MUST
      ;;              "Server takes care to be sure that the Update is authorized to modify its object before modifying the server's stored copy"))
      )
    ;;; SHOULD
     ;; (outbox:not-trust-submitted
     ;;  SHOULD
     ;;  "Server does not trust client submitted content")
     ;; (outbox:validate-content
     ;;  SHOULD
     ;;  "Validate the content they receive to avoid content spoofing attacks.")
     ;; (outbox:do-not-overload
     ;;  SHOULD
     ;;  "Take care not to overload other servers with delivery submissions")
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
      #:subitems ((outbox:like:adds-object-to-liked
                   SHOULD
                   "Adds the object to the actor's Liked Collection.")))
     (outbox:block
      SHOULD
      "Block"
      #:subitems ((outbox:block:prevent-interaction-with-actor
                   SHOULD
                   "Prevent the blocked object from interacting with any object posted by the actor."))))))


(define server-inbox-delivery
  (build-test-items
   `(;;; MUST
     (inbox:delivery:performs-delivery
      MUST
      "Performs delivery on all Activities posted to the outbox")
     (inbox:delivery:addressing
      MUST
      "Utilizes `to`, `bto`, `cc`, and `bcc` to determine delivery recipients.")
     (inbox:delivery:adds-id
      MUST
      "Provides an `id` all Activities sent to other servers, unless the activity is intentionally transient.")
     (inbox:delivery:submit-with-credentials
      MUST
      "Dereferences delivery targets with the submitting user's credentials")
     (inbox:delivery:deliver-to-collection
      MUST
      "Delivers to all items in recipients that are Collections or OrderedCollections"
      #:subitems ((inbox:delivery:deliver-to-collection:recursively
                   MUST
                   "Applies the above, recursively if the Collection contains Collections, and limits recursion depth >= 1")))
     (inbox:delivery:delivers-with-object-for-certain-activities
      MUST
      "Delivers activity with 'object' property if the Activity type is one of Create, Update, Delete, Follow, Add, Remove, Like, Block, Undo")
     (inbox:delivery:delivers-with-target-for-certain-activities
      MUST
      "Delivers activity with 'target' property if the Activity type is one of Add, Remove")
     (inbox:delivery:deduplicates-final-recipient-list
      MUST
      "Deduplicates final recipient list")
     (inbox:delivery:do-not-deliver-to-actor
      MUST
      "Does not deliver to recipients which are the same as the actor of the Activity being notified about")
     (inbox:delivery:do-not-deliver-block
      SHOULD
      "SHOULD NOT deliver Block Activities to their object."))))

(define server-inbox-accept
  (build-test-items
   `(;; MUST
     (inbox:accept:deduplicate
      MUST
      "Deduplicates activities returned by the inbox by comparing activity `id`s")
     (inbox:accept:special-forward
      MUST
      "Forwards incoming activities to the values of to, bto, cc, bcc, audience if and only if criteria in 7.1.2 are met.")
     (inbox:accept:special-forward:recurses
      SHOULD
      "Recurse through to, bto, cc, bcc, audience object values to determine whether/where to forward according to criteria in 7.1.2")
     (inbox:accept:special-forward:limits-recursion
      SHOULD
      "Limit recursion in this process")

     ;; * Update
     (inbox:accept:update:is-authorized
      MUST
      "Take care to be sure that the Update is authorized to modify its object")
     (inbox:accept:update:completely-replace
      SHOULD
      "Completely replace its copy of the activity with the newly received value")

     ;; SHOULD
     (inbox:accept:dont-blindly-trust
      SHOULD
      "Don't trust content received from a server other than the content's origin without some form of verification.")
     ;; * Follow
     (inbox:accept:follow:add-actor-to-users-followers
      SHOULD
      "Add the actor to the object user's Followers Collection.")
     ;; * Add
     (inbox:accept:add:to-collection
      SHOULD
      "Add the object to the Collection specified in the target property, unless not allowed to per requirements in 7.8")
     (inbox:accept:remove:from-collection
      SHOULD
      "Remove the object from the Collection specified in the target property, unless not allowed per requirements in 7.9")
     ;; * Like
     (inbox:accept:like:indicate-like-performed
      SHOULD
      "Perform appropriate indication of the like being performed (See 7.10 for examples)")
     ;; @@: The same as dont-blindly-trust...
     ;; (inbox:accept:validate-content
     ;;  SHOULD
     ;;  "Validate the content they receive to avoid content spoofing attacks.")
     )))

(define server-common-test-items
  (build-test-items
   `(;; Inbox Retrieval
     (server:inbox:responds-to-get
      NON-NORMATIVE
      "Server responds to GET request at inbox URL")
     (server:inbox:is-orderedcollection
      MUST
      "inbox is an OrderedCollection")
     (server:inbox:filtered-per-permissions
      SHOULD
      "Server filters inbox content according to the requester's permission")

     ;; @@: Does this one even make sense to test for?  It's implied by the others
     ;; Object retrieval
     (server:object-retrieval:get-id
      MAY
      "Allow dereferencing Object `id`s by responding to HTTP GET requests with a representation of the Object")
     ;; "if the above is true, the server"
     (server:object-retrieval:respond-with-as2-re-ld-json
      MUST
      "Respond with the ActivityStreams object representation in response to requests that primarily Accept the media type `application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"`")
     (server:object-retrieval:respond-with-as2-re-activity-json
      SHOULD
      "Respond with the ActivityStreams object representation in response to requests that primarily Accept the media type `application/activity+json`")
     (server:object-retrieval:deleted-object:tombstone
      MAY
      "Responds with response body that is an ActivityStreams Object of type `Tombstone` (if the server is choosing to disclose that the object has been removed)")
     (server:object-retrieval:deleted-object:410-status
      SHOULD
      "Respond with 410 Gone status code if Tombstone is in response body, otherwise responds with 404 Not Found")
     (server:object-retrieval:deleted-object:404-status
      SHOULD
      "Respond with 404 status code for Object URIs that have never existed")

     (server:object-retrieval:private-403-or-404
      SHOULD
      "Respond with a 403 Forbidden status code to all requests that access Objects considered Private (or 404 if the server does not want to disclose the existence of the object, or another HTTP status code if specified by the authorization method)")

     ;; Non-normative security considerations
     (server:security-considerations:actually-posted-by-actor
      NON-NORMATIVE
      ;; [B.1](https://w3c.github.io/activitypub/#security-verification)
      "Server verifies that the new content is really posted by the actor indicated in Objects received in inbox and outbox")
     (server:security-considerations:do-not-post-to-localhost
      NON-NORMATIVE
      "By default, implementation does not make HTTP requests to localhost when delivering Activities")
     (server:security-considerations:uri-scheme-whitelist
      NON-NORMATIVE
      "Implementation applies a whitelist of allowed URI protocols before issuing requests, e.g. for inbox delivery")
     (server:security-considerations:filter-incoming-content
      NON-NORMATIVE
      "Server filters incoming content both by local untrusted users and any remote users through some sort of spam filter")
     (server:security-considerations:sanitize-fields
      NON-NORMATIVE
      "Implementation takes care to santizie fields containing markup to prevent cross site scripting attacks"))))

(define client-test-items
  (build-test-items
   `(;; MUST
     (client:submission:discovers-url-from-profile
      MUST
      "Client discovers the URL of a user's outbox from their profile")
     (client:submission:submit-post-with-content-type
      MUST
      "Client submits activity by sending an HTTP post request to the outbox URL with the Content-Type of application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
     (client:submission:submit-objects
      MUST
      "Client submission request body is either a single Activity or a single non-Activity Object"
      #:subitems ((client:submission:submit-objects:provide-object
                   MUST
                   "Clients provide the object property when submitting the following activity types to an outbox: Create, Update, Delete, Follow, Add, Remove, Like, Block, Undo.")
                  (client:submission:submit-objects:provide-target
                   MUST
                   "Clients provide the target property when submitting the following activity types to an outbox: Add, Remove.")))
     (client:submission:authenticated
      MUST
      "Client sumission request is authenticated with the credentials of the user to whom the outbox belongs")
     ;; (client:submission:uploading-media
     ;;  MUST
     ;;  "Client supports [uploading media](https://www.w3.org/TR/activitypub/#uploading-media) by sending a multipart/form-data request body")

     (client:submission:recursively-add-targets
      SHOULD
      "Before submitting a new activity or object, Client infers appropriate target audience by recursively looking at certain properties (e.g. `inReplyTo`, See Section 7), and adds these targets to the new submission's audience."
      #:subitems ((client:submission:recursively-add-targets:limits-depth
                   SHOULD
                   "Client limits depth of this recursion.")))
     (client:retrieval:accept-header
      MUST
      "When retrieving objects, Client specifies an Accept header with the `application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"` media type ([3.2](https://w3c.github.io/activitypub/#retrieving-objects))"))))


;;; TODO: Continue at Inbox Retrieval
;;; @@: Do these apply to both c2s and s2s?

(define* (flatten-test-items test-items)
  (define (flatten items onto)
    (fold (lambda (test-item prev)
            (flatten
             (test-item-subitems test-item)
             (cons test-item prev)))
          onto
          items))
  (reverse (flatten test-items '())))

(define all-test-items
  (flatten-test-items
   (append client-test-items
           c2s-server-items server-inbox-delivery
           server-inbox-accept
           server-common-test-items)))

(define all-test-items-hashed
  (let ((table (make-hash-table)))
    (let lp ((items all-test-items))
      (for-each (lambda (test-item)
                  (hashq-set! table (test-item-sym test-item) test-item)
                  ;; recursively add all subitems, if any
                  (lp (test-item-subitems test-item)))
                items))
    table))

(define (get-test-item sym)
  "Get a test item by its symbol"
  (hashq-ref all-test-items-hashed sym))


;;; More debuggable apclient
(define-class <debug-apc> (<apclient>)
  (case-worker #:init-keyword #:case-worker
               #:accessor .case-worker))

(define (%get-and-show-response apclient next-method)
  (receive (response body)
      (next-method)
    (show-user
     `((p (b "<<< DEBUG:") " Got back a " ,(number->string (response-code response))
          " with headers:")
       (div (@ (class "req-block"))
            (pre ,(call-with-output-string
                    (lambda (p) (write-headers (response-headers response)
                                               p)))))
       " and body: "
       (div (@ (class "req-block"))
            (pre
             ,(match (response-content-type response)
                (#f "<<binary contents>>")
                (((or 'application/activity+json
                      'application/ld+json
                      'text/json) rest ...)
                 (with-output-to-string
                   (lambda ()
                     (json-pprint
                      (read-json-from-string (utf8->string body))))))
                (((? (lambda (resource)
                       (string-prefix? (symbol->string resource) "text/")))
                  rest ...)
                 (utf8->string body))
                (_ "<<binary contents>>")))))
     #:case-worker (.case-worker apclient))
    (values response body)))

;;; Override get/post methods so we can show the user what the request/response
;;; looks like
(define (uri-as-string uri)
  (match uri
    ((? string?) uri)
    ((? uri?) (uri->string uri))))

(define-method* (apclient-get (apclient <debug-apc>) uri
                              #:key (headers '()))
  (show-user
   `((p (b ">>> DEBUG:") " Sending " (code "GET") " request to "
        (code ,(uri-as-string uri))
        " with headers: ")
     (div (@ (class "req-block"))
          (pre ,(call-with-output-string
                  (lambda (p) (write-headers headers p))))))
   #:case-worker (.case-worker apclient))
  (%get-and-show-response apclient next-method))

(define-method* (apclient-post (apclient <debug-apc>) uri body
                               #:key (headers '()))
  (define (body-as-string)
    (match body
      ((? bytevector?)
       (utf8->string body))
      ((? string?)
       body)))
  (show-user
   `((p (b ">>> DEBUG:") " Sending " (code "POST") " request to "
        (code ,(uri-as-string uri))
        " with headers: ")
     (div (@ (class "req-block"))
          (pre ,(call-with-output-string
                      (lambda (p) (write-headers headers p)))))
     (p "and body:")
     (div (@ (class "req-block"))
          (pre ,(match (assoc-ref headers 'content-type)
                  (#f "<<binary contents>>")
                  (((or 'application/activity+json
                        'application/ld+json
                        'text/json) rest ...)
                   (with-output-to-string
                     (lambda ()
                       (json-pprint
                        (read-json-from-string
                         (body-as-string))))))
                  (((? (lambda (resource)
                         (string-prefix? (symbol->string resource) "text/")))
                    rest ...)
                   (body-as-string))
                  (_ "<<binary contents>>")))))
   #:case-worker (.case-worker apclient))
  (%get-and-show-response apclient next-method))


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
                              "I'm testing an ActivityPub server which follows the commands "
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
                              ".")))
               (table (@ (class "input-table"))
                      (tr (td (@ (style "padding-top: 1em; padding-left: 1em;"))
                              (input (@ (name "verbose-debugging")
                                        (type "checkbox"))))
                          (td (@ (style "padding-top: 1em;"))
                              (i "Check if you'd like verbose debugging about what HTTP "
                                 "requests the server is running.")))))
             #:checkpoint checkpoint))
           (testing-client (jsobj-ref user-input "testing-client"))
           (testing-c2s-server (jsobj-ref user-input "testing-c2s-server"))
           (testing-s2s-server (jsobj-ref user-input "testing-s2s-server")))
      (define (mark-not-applicable! tests)
        (for-each
         (lambda (test)
           (report-on! (test-item-sym test) <not-applicable>))
         (flatten-test-items tests)))

      ;; Set whether or not we're using verbose debugging here
      (set! (.debug case-worker)
            (jsobj-ref user-input "verbose-debugging"))

      (if (or testing-client testing-c2s-server testing-s2s-server)
          ;; We need at least one to continue
          (begin
            (set! (.testing-client? case-worker) testing-client)
            (set! (.testing-c2s-server? case-worker) testing-c2s-server)
            (set! (.testing-s2s-server? case-worker) testing-s2s-server)
            ;; Run all the c2s server tests, since we can do those "immediately"
            ;; without prompting the user
            (if testing-c2s-server
                (test-c2s-server case-worker)
                (mark-not-applicable! c2s-server-items))
            ;; ;; Now, the "observed" tests
            ;; (when (or testing-c2s-server testing-s2s-server)
            ;;   ;; Set up observables
            ;;   (when testing-c2s-server
            ;;     (setup-c2s-server-observables! case-worker))
            ;;   (when testing-s2s-server
            ;;     (setup-s2s-server-observables! case-worker))
            ;;   ;; Now we need to tell the user about the observables and
            ;;   ;; probably wait
            ;;   (run-or-ask-to-run-tests-and-wait! case-worker))
            ;; Here are the "prompty" tests
            (if testing-client
                (test-client case-worker)
                (mark-not-applicable! client-test-items))
            (if testing-s2s-server
                (test-s2s-server case-worker)
                (mark-not-applicable! (append server-inbox-delivery server-inbox-accept)))
            (if (or testing-c2s-server testing-s2s-server)
                (test-server-common case-worker)
                (mark-not-applicable! server-common-test-items)))
          ;; We didn't get anything, so let's loop until we do
          (begin (show-user (warn
                             '("It looks like you didn't select anything. "
                               "Please select at least one implementation type to test.")))
                 (drop-top-checkpoint!)
                 (get-input-loop #t)))))

  (show-results-page case-worker))

(define (show-results-page case-worker)
  (define report (.report case-worker))
  (define (item-table test-items)
    `(table
      (@ (cellpadding 2))
      ,@(map (lambda (test-item)
               (let ((response
                      (assoc-ref report (test-item-sym test-item))))
                 `(tr 
                   (td (b ,(symbol->string (test-item-req-level test-item))
                          ": ")
                       ,(test-item-desc test-item))
                   (td (@ (style "text-align: right;"))
                       (b "["
                          ,(if response
                               (response-as-log-result-text response)
                               "missing???")
                          "]")))))
             (flatten-test-items test-items))))
  (let-syntax ((inline-when
                (syntax-rules ()
                  ((inline-when test consequent ...)
                   (if test
                       (list consequent ...)
                       '())))))
    (let* ((user-input
            (get-user-input
             `((h2 (@ (style "text-align: center;"))
                   "Results")
               ,@(inline-when (.testing-client? case-worker)
                              '(h3 "Client tests")
                              (item-table client-test-items))

               ,@(inline-when (.testing-c2s-server? case-worker)
                              '(h3 "Server: Client-to-Server tests")
                              (item-table c2s-server-items))

               ,@(inline-when (.testing-s2s-server? case-worker)
                              '(h3 "Server: Federation tests")
                              (item-table (append server-inbox-delivery
                                                  server-inbox-accept)))

               ,@(inline-when (or (.testing-c2s-server? case-worker)
                                  (.testing-s2s-server? case-worker))
                              '(h3 "Server: Common tests")
                              (item-table server-common-test-items))
               (hr)
               (h3 (@ (style "text-align: center; padding: 10px;"))
                   "Looks good?")
               (p "If the above looks correct, enter a name for your project "
                  "and (if appropriate) a website, then press submit to "
                  "generate an implementation report:")
               (p (b "Project name: ") (br)
                  (input (@ (name "project-name")
                            (style "width: 100%;"))))
               (p (b "Website: ") (br)
                  (input (@ (name "website")
                            (style "width: 100%;"))))
               (p (b "Notes: ")
                  (br)
                  (i "Tested interop with other projects?  Tell us their names / "
                     "websites!")
                  (br)
                  (textarea (@ (name "notes")
                               (style "width: 100%;")
                               (rows "4")) "")))))
           (project-name (jsobj-ref user-input "project-name"))
           (website (jsobj-ref user-input "website"))
           (notes (jsobj-ref user-input "notes"))
           (final-report
            `(@ (project-name ,project-name)
                (website ,website)
                (date ,(date->rfc3339-string (current-date 0)))
                (notes ,notes)
                (testing-client ,(.testing-client? case-worker))
                (testing-c2s-server ,(.testing-c2s-server? case-worker))
                (testing-s2s-server ,(.testing-s2s-server? case-worker))
                (results
                 (@ ,@(map
                       (lambda (test-item)
                         (let* ((sym (test-item-sym test-item))
                                (response (assoc-ref report sym)))
                           (if response
                               `(,sym
                                 (@ (result ,(.type-for-report response))
                                    ,@(if (.comment response)
                                          `((comment ,(.comment response)))
                                          '())))
                               `(,sym (@ (result null))))))
                       all-test-items)))))
           (report-id
            (<-wait (.db-manager case-worker) 'store-document!
                    (with-output-to-string
                      (lambda ()
                        (json-pprint final-report)))))
           (report-link
            (string-append "/download-report/" report-id)))
      (show-user
       `(div (@ (style "padding-left: 10%; padding-right: 10%; padding-bottom: 15px;"))
             (h1 (@ (style "text-align: center;"))
                 "You're all done!  (Almost!)")
             (p (@ (style "text-align: center; margin-bottom: 1.5em;"))
                (span (@ (style "font-family: 'Inconsolata', monospace; font-weight: bold; border-top-left-radius: 4px; border-top-right-radius: 4px; border-bottom-left-radius: 4px; border-bottom-right-radius: 4px; border: 3px solid #d5d5d5; background: #dcdad5;"))
                      ,(link report-link "Download Report!")))
             (p "Nice job!  Now all that's left is to "
                "submit your report to the "
                ,(link "https://activitypub.rocks/implementations/"
                       "implementation reports page")
                "!  All you have to do is:")
             (ol (li "Right click -> Copy Link Location on the "
                     ,(link report-link "download report")
                     " link.")
                 (li ,(link "https://github.com/w3c/activitypub/issues/new"
                            "File an issue")
                     " on the ActivityPub tracker, listing your project's name "
                     "and pasting in the link to your "
                     "implementation report.")
                 (li "That's it!  There is no step three!"))
             (p "Horray!  Thank you for helping the ActivityPub network grow!"))))))


;;; Client tests

(define (%check-in case-worker title description questions)
  (let ((user-input
         (get-user-input
          `((h2 ,title)
            ,@(if description
                  (list description)
                  '())
            (table (@ (class "input-table"))
                   ,@(map
                      (match-lambda
                        ((sym text-question)
                         `(tr (td (input (@ (name ,(symbol->string sym))
                                            (type "checkbox"))))
                              (td ,text-question))))
                      questions))))))
    (for-each
     (match-lambda
       ((sym text-question)
        (report-on! sym
                    (if (jsobj-ref user-input (symbol->string sym))
                        <success> <fail>))))
     questions)))


(define (test-client case-worker)
  (define (check-in title description questions)
    (%check-in case-worker title description questions))
  
  (check-in "Client: Basic submission"
            "Construct a basic activity and submit it to the actor's outbox."
            '((client:submission:discovers-url-from-profile
               "Client discovers the URL of a user's outbox from their profile")
              (client:submission:submit-post-with-content-type
               ("Client submits activity by sending an HTTP post request to the outbox URL with the "
                (code "Content-Type")
                " of "
                (code "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")))
              (client:submission:submit-objects
               "Client submission request body is either a single Activity or a single non-Activity Object")
              (client:submission:authenticated
               "Client sumission request is authenticated with the credentials of the user to whom the outbox belongs (this could be using an OAuth 2.0 bearer token or something else)")))

  (check-in "Client: Required properties"
            #f
            `((client:submission:submit-objects:provide-object
               "Client provides the object property when submitting the following activity types to an outbox: Create, Update, Delete, Follow, Add, Remove, Like, Block, Undo.")
              (client:submission:submit-objects:provide-target
               "Client provides the target property when submitting the following activity types to an outbox: Add, Remove.")))
   ;; (client:submission:uploading-media
   ;;  MUST
   ;;  "Client supports [uploading media](https://www.w3.org/TR/activitypub/#uploading-media) by sending a multipart/form-data request body")
 
  (check-in "Client: Add targets"
            "Reply to a post with multiple recipients."
            '((client:submission:recursively-add-targets
               "The client suggests audience targeting based on participants in the referenced thread")
              (client:submission:recursively-add-targets:limits-depth
               "The client also limits depth of recursion used to gather targeting.")))
  (check-in "Client: Accept header on object retrieval"
            "Trigger the client to retrieve some remote object."
            '((client:retrieval:accept-header
               ("When retrieving objects, Client specifies an "
                (code "Accept")
                " header with the "
                (code "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"") " media type")))))


;;; client-to-server server tests

(define (test-c2s-server case-worker)
  (set-up-c2s-server-client-auth case-worker)
  (test-outbox-activity-posted case-worker)
  (test-outbox-removes-bto-and-bcc case-worker)
  (test-outbox-non-activity case-worker)
  (test-outbox-update case-worker)
  ;;; We HAVE these tests, but since they didn't make it into
  ;;; ActivityPub proper they're commented out of the test suite for now...
  ;; (test-outbox-upload-media case-worker)
  (test-outbox-activity-follow case-worker)
  ;; (test-outbox-verification case-worker)
  ;; (test-outbox-subjective case-worker)
  (test-outbox-activity-create case-worker)
  (test-outbox-activity-add-remove case-worker)
  (test-outbox-activity-like case-worker)
  (test-outbox-activity-block case-worker))

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
           (user-id (jsobj-ref user-input "actor-id"))
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
            (let ((apclient
                   (if (.debug case-worker)
                       (make <debug-apc>
                         #:id (string->uri user-id)
                         #:case-worker case-worker)
                       (make <apclient>
                         #:id (string->uri user-id)))))
              (apclient-user apclient) ; called for init side effect :\
              apclient))))
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
        (cond ((jsobj-ref user-input "auth-token") =>
               (lambda (auth-token)
                 (set! (apclient-auth-token apclient) auth-token)))
              (else
               (drop-top-checkpoint!)
               (lp)))))

    ;; Add the fully setup apclient to the case-worker
    (set! (.apclient case-worker) apclient))

  (show-user (center-text `(h2 "Testing server's client-to-server support...")))
  (setup-auth-info (get-user-obj))
  (.apclient case-worker))

(define no-location-present-message
  "Couldn't verify since no Location header present in response")

(define* (test-outbox-activity-posted case-worker)
  ;;   Maybe?  This is a bit federation'y, requires that we have
  ;;   a server it can talk to
  (define apclient (.apclient case-worker))
  (define activity-to-submit
    (as:create #:id "http://tsyesika.co.uk/act/foo-id-here/"  ; id should be removed
               #:actor (uri->string (apclient-id apclient))
               #:object (as:note #:id "http://tsyesika.co.uk/chat/sup-yo/"  ; same with object id
                                 #:attributedTo (uri->string (apclient-id apclient))
                                 #:content "Up for some root beer floats?")))

  (define activity-submitted #f)

  (with-report
   '(outbox:responds-201-created
     outbox:location-header
     outbox:ignores-id
     outbox:accepts-activities)
   (receive (response body)
       (apclient-submit apclient activity-to-submit)
     ;; [outbox:responds-201-created]
     (match (response-code response)
       (201
        (set! activity-submitted #t)
        (report-on! 'outbox:responds-201-created
                    <success>))
       (other-status-code
        (report-on! 'outbox:responds-201-created
                    <fail>
                    #:comment (format #f "Responded with status code ~a"
                                      other-status-code))))

     ;; [outbox:location-header]
     (match (response-location response)
       ((? uri? location-uri)
        (set! activity-submitted #t)
        (report-on! 'outbox:location-header
                    <success>)

        ;; [outbox:ignores-id]
        ;; Now we fetch the object at the location...
        (receive (loc-response loc-asobj)
            (apclient-get-local-asobj apclient location-uri)
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
                                          (apclient-get-asobj apclient obj-uri)
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
                (report-on! 'outbox:ignores-id <success>))
              (report-on! 'outbox:ignores-id <fail>))))
       (#f
        (report-on! 'outbox:location-header <fail>)
        (report-on! 'outbox:ignores-id <inconclusive>
                    #:comment no-location-present-message)))

     (if activity-submitted
         (report-on! 'outbox:accepts-activities <success>)
         (report-on! 'outbox:accepts-activities <inconclusive>
                     #:comment "Response code neither 200 nor 201, and no Location header present")))))

(define (%submit-asobj-and-retrieve apclient asobj)
  "Helper procedure for submitting ASOBJ to APCLIENT's inbox.
Retrieve the submitted object as an activitystreams object.
Will abort if any of such steps fail."
  (receive (submit-response submit-body)
      (apclient-submit apclient asobj)
    (when (not (member (response-code submit-response)
                       '(200 201)))
      (throw 'report-abort
             "Neither 200 nor 201 status code returned in submission response."))
    (match (response-location submit-response)
      ((? uri? location-uri)
       (receive (loc-response loc-asobj)
           (apclient-get-local-asobj apclient location-uri)
         (when (not (asobj? loc-asobj))
           (throw 'report-abort
                  "Submitted object, but no ActivityStreams object at response's Location"))
         loc-asobj))
      (#f
       (throw 'report-abort
              no-location-present-message)))))

(define* (%create-and-retrieve-object apclient asobj
                                      #:key wrap-in-create?)
  "Like %submit-asobj-and-retrieve but explicitly retrieve the
object from a returned Create object."
  (define to-submit
    (if wrap-in-create?
        (as:create #:actor (uri->string (apclient-id apclient))
                   #:object asobj)
        asobj))

  (let ((returned-asobj
         (%submit-asobj-and-retrieve apclient to-submit)))
    (cond
     ((asobj-is-a? returned-asobj ^Create)
      (match (asobj-ref returned-asobj "object")
        ((? string-uri? object-id)
         (receive (response object)
             (apclient-get-local-asobj apclient object-id)
           (when (not (asobj? object))
             (throw 'report-abort
                    "Could not retrieve an ActivityStreams object from object uri."))
           object))
        ((? asobj? object)
         object)
        (#f
         (throw 'report-abort
                "Create object returned with no object"))))
     ;; Not wrapped in a Create?  Return as-is.
     (else returned-asobj))))

(define (test-outbox-removes-bto-and-bcc case-worker)
  (define apclient (.apclient case-worker))
  (with-report
   '(outbox:removes-bto-and-bcc)
   (let* ((bcc-pseudoactor (case-worker-pseudoactor-new! case-worker))
          (bto-pseudoactor (case-worker-pseudoactor-new! case-worker))
          (submitted-asobj
           (%submit-asobj-and-retrieve
            apclient
            (as:create #:actor (uri->string (apclient-id apclient))
                       #:bto (pseudoactor-id bto-pseudoactor)
                       #:bcc (pseudoactor-id bcc-pseudoactor)
                       #:object (as:note #:attributedTo (uri->string (apclient-id apclient))
                                         #:content "Up for some root beer floats?")))))
     (if (and (not (asobj-ref submitted-asobj "bto"))
              (not (asobj-ref submitted-asobj "bcc")))
         (report-on! 'outbox:removes-bto-and-bcc
                     <success>)
         (report-on! 'outbox:removes-bto-and-bcc
                     <fail>)))))


(define (test-outbox-non-activity case-worker)
  (define activity-to-submit
    (as:note #:content "Up for some root beer floats?"))
  (define apclient (.apclient case-worker))

  ;; [outbox:accepts-non-activity-objects]
  (with-report
   '(outbox:accepts-non-activity-objects)
   (let ((retrieved-asobj
          (%submit-asobj-and-retrieve apclient activity-to-submit)))
     (if (asobj-is-a? retrieved-asobj ^Create)
         (report-on! 'outbox:accepts-non-activity-objects
                     <success>)
         (report-on! 'outbox:accepts-non-activity-objects
                     <fail>
                     #:comment "ActivityStreams object pointed to by response Location is not of type Create")))))

(define (test-outbox-verification case-worker)
  ;; [outbox:not-trust-submitted]
  ;; [outbox:validate-content]
  'TODO
  )

(define (test-outbox-upload-media case-worker)
  ;; [outbox:upload-media]
  ;; [outbox:upload-media:file-parameter]
  ;; [outbox:upload-media:object-parameter]
  ;; [outbox:upload-media:201-or-202-status]
  ;; [outbox:upload-media:location-header]
  ;; [outbox:upload-media:appends-id]
  ;; [outbox:upload-media:url]
  (define apclient (.apclient case-worker))

  (with-report
   '(outbox:upload-media
     outbox:upload-media:201-or-202-status
     outbox:upload-media:location-header
     outbox:upload-media:file-parameter
     outbox:upload-media:object-parameter
     outbox:upload-media:appends-id
     outbox:upload-media:url)
   (when (not (apclient-media-uri apclient))
     (throw 'report-abort
            "No media endpoint given"))
   
   (let ((submit-response
          (apclient-submit-file apclient
                                (as:image #:name "A red ghostie!"
                                          #:content "Isn't it cute?")
                                (web-static-filepath "/images/red-ghostie.png"))))
     (report-on! 'outbox:upload-media
                 <success>)
     (report-on! 'outbox:upload-media:201-or-202-status
                 (if (member (response-code submit-response)
                             '(200 201))
                     <success>
                     <fail>))

     (match (response-location submit-response)
       ((? uri? location-uri)
        (receive (loc-response loc-asobj)
            (apclient-get-local-asobj apclient location-uri)
          (report-on! 'outbox:upload-media:location-header
                      <success>)
          ;; So either we got back the object, or its Create.
          ;; Create would be The Right Thing but we aren't really testing
          ;; that bit here.
          (let ((image-asobj
                 (cond
                  ((asobj-is-a? loc-asobj ^Image)
                   loc-asobj)
                  ((and (asobj-is-a? loc-asobj ^Create)
                        (asobj-ref loc-asobj "object"))
                   (receive (_ image-asobj)
                       (apclient-get-local-asobj
                        apclient (asobj-id (asobj-ref loc-asobj "object")))
                     (when (or (not (asobj? image-asobj))
                               (not (asobj-is-a? image-asobj ^Image)))
                       (throw 'report-abort
                              "Location points to a Create object, but no Image activitystreams object at that location."))
                     image-asobj))
                  (else
                   (throw 'report-abort
                          "Location points to something that isn't a Create or an Image.  Huh?")))))
            ;; Was the object parameter really accepted?  If it is, the object
            (if (and (equal? (asobj-ref image-asobj "name")
                             "A red ghostie!")
                     (equal? (asobj-ref image-asobj "content")
                             "Isn't it cute?"))
                (report-on! 'outbox:upload-media:object-parameter
                            <success>)
                (report-on! 'outbox:upload-media:object-parameter
                            <fail>
                            #:comment "Image object doesn't contain the values of the uploaded object's original `name' or `content' fields."))
            ;; But what about the file parameter?
            ;; We're going to have to check the url field.
            ;; We can't determine exactly that the file is based off the url field,
            ;; because it might not be the same contents... maybe the server resized it,
            ;; or whatever.
            ;; But we can at least check that a url field got added.
            (cond ((asobj-ref image-asobj "url")
                   (report-on! 'outbox:upload-media:file-parameter
                               <success>)
                   (report-on! 'outbox:upload-media:url
                               <success>))
                  (else
                   (report-on! 'outbox:upload-media:file-parameter
                               <inconclusive>
                               #:comment
                               "Unable to determine if file object was accepted, not where it was expected to be (in the `url' parameter).")
                   (report-on! 'outbox:upload-media:url
                               <fail>)))

            ;; id should be appended to both the Image and (if it exists)
            ;; the wrapping Create object
            (if (and (asobj-ref image-asobj "id")
                     (asobj-ref loc-asobj "id"))
                (report-on! 'outbox:upload-media:appends-id
                            <success>)
                (report-on! 'outbox:upload-media:appends-id
                            <fail>)))))
       (#f
        (report-on! 'outbox:upload-media:location-header
                    <fail>))))))

(define (test-outbox-update case-worker)
  "Test the Update activity"
  ;; [outbox:update]
  ;; TODO: [outbox:update:check-authorized]
  (define %nothing '(nothing))
  (define apclient (.apclient case-worker))

  (define (abort-because-no-location abort)
    (report-on! 'outbox:update <fail>  ; fail or inconclusive?
                #:comment no-location-present-message)
    (abort))

  (define (abort-because-no-asobj abort uri)
    (report-on! 'outbox:update <fail>  ; fail or inconclusive?
                #:comment
                (format #f "Couldn't find an ActivityStreams object at the expected URI: ~a"
                        (match uri
                          ((? uri? _)
                           (uri->string uri))
                          ((? string? _) uri))))
    (abort))

  (define (submit-create abort)
    "Try to submit the Create activity, and return the location-uri"
    (define to-submit
      (as:create
       #:actor (uri->string (apclient-id apclient))
       #:object (as:note #:content "I'm feeling indecisive!"
                         #:name "An indecisive note")))
    ;; Submit initial activity
    (receive (response body)
        (apclient-submit apclient to-submit)
      (match (response-location response)
        ((? uri? location-uri)
         location-uri)
        (#f (abort-because-no-location abort)))))

  (define (get-object-id-from-location abort location)
    (define asobj-at-location
      (receive (_ asobj)
          (apclient-get-local-asobj apclient location)
        (unless (asobj? asobj)
          (abort-because-no-asobj abort location))
        asobj))
    (or (asobj-ref-id asobj-at-location "object")
        (begin
          (report-on! 'outbox:update <fail>  ; fail or inconclusive?
                      #:comment
                      "Create object contained no id for \"object\"")
          (abort))))

  (define (submit-update abort object-location)
    ;; Submit with name removed and content changed
    (apclient-submit apclient
                     (as:update
                      #:actor (uri->string (apclient-id apclient))
                      #:object (as:note #:id object-location
                                        #:content "I've changed my mind!"
                                        #:name 'null)))

    (receive (_ updated-asobj)
        (apclient-get-local-asobj apclient object-location)
      ;; content should be changed
      (unless (equal? (asobj-ref updated-asobj "content")
                      "I've changed my mind!")
        (report-on! 'outbox:update <fail>  ; fail or inconclusive?
                    #:comment
                    "Failed to update field with replacement data")
        (abort))
      ;; name should be gone
      (unless (eq? (asobj-ref updated-asobj "name" %nothing)
                   %nothing)
        (report-on! 'outbox:update <fail>  ; fail or inconclusive?
                    #:comment
                    "Unable to delete field by passing an Update with null value")
        (abort)))

    ;; Now let's try one more update, where we add back the name with
    ;; new content
    (apclient-submit apclient
                     (as:update
                      #:actor (uri->string (apclient-id apclient))
                      #:object (as:note #:id object-location
                                        #:name "new name, same flavor")))

    (receive (_ updated-asobj)
        (apclient-get-local-asobj apclient object-location)
      ;; content should be the same as last time
      (unless (equal? (asobj-ref updated-asobj "content")
                      "I've changed my mind!")
        (report-on! 'outbox:update <fail>
                    #:comment
                    "Field changed, despite not being included in update")
        (abort))

      (unless (equal? (asobj-ref updated-asobj "name")
                      "new name, same flavor")
        (report-on! 'outbox:update <fail>  ; fail or inconclusive?
                    #:comment
                    "Failed to update field with replacement data")
        (abort)))

    ;; Make it this far?  Looks like it passed!
    (report-on! 'outbox:update <success>))
  
  (with-report
   '(outbox:update)
   (call/ec
    (lambda (abort)
      (define create-location (submit-create abort))
      (define object-id (get-object-id-from-location abort create-location))
      (submit-update abort object-id)))))

(define (test-outbox-subjective case-worker)
  ;; [outbox:do-not-overload]
  ;; [outbox:not-trust-submitted]
  'TODO
  )

(define (test-outbox-activity-create case-worker)
  (define apclient (.apclient case-worker))
  (with-report
   '(outbox:create
     outbox:create:merges-audience-properties
     outbox:create:actor-to-attributed-to)

   (let* (;; Create some local pseudoactors for addressing.
          (psa1 (case-worker-pseudoactor-new! case-worker))
          (psa2 (case-worker-pseudoactor-new! case-worker))
          (psa3 (case-worker-pseudoactor-new! case-worker))
          (psa4 (case-worker-pseudoactor-new! case-worker))
          (psa5 (case-worker-pseudoactor-new! case-worker))
          (ids-of (lambda pseudoactors
                    (map pseudoactor-id pseudoactors)))
          (create-asobj
           (%submit-asobj-and-retrieve
            apclient
            (as:create #:to (ids-of psa1 psa2)
                       #:cc (pseudoactor-id psa3)
                       #:actor (uri->string (apclient-id apclient))
                       #:object (as:note #:cc (ids-of psa4 psa5)
                                         #:content "Hi there!"))))
          (object-asobj
           (cond
            ((asobj-ref-id create-asobj "object") =>
             (lambda (obj-id)
               (receive (response object)
                   (apclient-get-local-asobj apclient obj-id)
                 (when (not (asobj? object))
                   (throw 'report-abort
                          "Could not retrieve an ActivityStreams object from object uri."))
                 object)))
            (else
             (throw 'report-abort
                    "No object id found."))))
          (same-addressing?
           (lambda (object field compared-to)
             (define addresses
               (map (match-lambda
                      ((? string-uri? obj)
                       obj)
                      ((? asobj? obj)
                       (asobj-id obj))
                      (_ 'wtf))
                    (asobj-ref create-asobj field '())))
             (equal? (sort addresses string<) (sort compared-to string<)))))
     ;; [outbox:create]
     (report-on! 'outbox:create <success>)
     ;; [outbox:create:merges-audience-properties]
     (if (and (same-addressing? create-asobj "to"
                                (ids-of psa1 psa2))
              (same-addressing? create-asobj "cc"
                                (ids-of psa3 psa4 psa5))
              (same-addressing? object-asobj "to"
                                (ids-of psa1 psa2))
              (same-addressing? object-asobj "cc"
                                (ids-of psa3 psa4 psa5)))
         (report-on! 'outbox:create:merges-audience-properties
                     <success>)
         (report-on! 'outbox:create:merges-audience-properties
                     <fail>))
     ;; [outbox:create:actor-to-attributed-to]
     (if (equal? (uri->string (apclient-id apclient))
                 (match (asobj-ref object-asobj "attributedTo")
                   ((? string-uri? str-uri) str-uri)
                   ((? asobj? actor-asobj) (asobj-id actor-asobj))))
         (report-on! 'outbox:create:actor-to-attributed-to
                     <success>)
         (report-on! 'outbox:create:actor-to-attributed-to
                     <fail>)))))

(define (test-outbox-activity-follow case-worker)
  ;; @@: These should move into the let

  ;; [outbox:follow]
  ;; [outbox:follow:adds-followed-object]
  (with-report
   '(outbox:follow
     outbox:follow:adds-followed-object)
   (let* ((actor-to-follow
           (case-worker-pseudoactor-new! case-worker))
          (follow-id (pseudoactor-id actor-to-follow))
          (apclient (.apclient case-worker))
          (activity-to-submit
           (as:follow #:actor (uri->string (apclient-id apclient))
                      #:object follow-id))
          (retrieved-asobj
           (%submit-asobj-and-retrieve apclient activity-to-submit)))
     (report-on! 'outbox:follow <success>)
     ;; TODO: now to look through the following collection
     (let* ((f-stream
             ;; ;; We restrict it to 1000 items, which should be waaaay
             ;; ;; more than enough, so that we don't accidentally end up
             ;; ;; searching forever
             (stream-take 1000
                          (apclient-following-stream apclient)))
            (item-in-stream?
             (let lp ((stream f-stream))
               (cond
                ;; guess we didn't find it
                ((stream-null? stream)
                 #f)
                ((equal? (asobj-id (stream-car stream))
                         follow-id)
                 #t)
                (else (lp (stream-cdr stream)))))))
       (if item-in-stream?
           (report-on! 'outbox:follow:adds-followed-object <success>)
           (report-on! 'outbox:follow:adds-followed-object <fail>))))))


(define (test-outbox-activity-add-remove case-worker)
  (define apclient (.apclient case-worker))
  (define collection-to-submit
    (as:collection #:name "test collection"))

  (with-report
   '(outbox:add
     outbox:add:adds-object-to-target
     outbox:remove
     outbox:remove:removes-from-target)
   ;; Make a new collection
   (let* ((collection (%create-and-retrieve-object
                       apclient collection-to-submit))
          (simple-note (%create-and-retrieve-object
                        apclient
                        (as:note #:content "I'm a note!")))
          (added-note (%submit-asobj-and-retrieve
                       apclient
                       (as:add #:actor (uri->string (apclient-id apclient))
                               #:object (asobj-id simple-note)
                               #:target (asobj-id collection))))
          (note-is-member?
           (lambda ()
             (define collection-refetched
               (receive (_ asobj)
                   (apclient-get-local-asobj apclient (asobj-id collection))
                 asobj))
             (define collection-stream
               ;; Limit to 200 items so we don't search forever.
               ;; That should be more than enough.
               (stream-take 200
                            (apclient-collection-item-stream
                             apclient collection-refetched
                             #:local? #t)))
             (stream-member collection-stream
                            (lambda (asobj)
                              (equal? (asobj-id asobj)
                                      (asobj-id simple-note)))))))
     ;; If we got this far, we were able to add it ok
     (report-on! 'outbox:add <success>)

     ;; [outbox:add:adds-object-to-target]
     ;; The object should now be in the collection
     (if (note-is-member?)
         (report-on! 'outbox:add:adds-object-to-target <success>)
         (begin
           (report-on! 'outbox:remove <inconclusive>
                       #:comment "Couldn't test removing from collection because couldn't add initially.")
           (report-on! 'outbox:remove:removes-from-target <inconclusive>
                       #:comment "Couldn't test removing from collection because couldn't add initially.")
           (throw 'report-abort
                  "Object does not appear in collection after adding it.")))

     ;; Now remove it
     ;; @@: We're using this procedure instead of simply apclient-submit
     ;;   because it does some extra sanity checks
     (%submit-asobj-and-retrieve
      apclient
      (as:remove #:actor (uri->string (apclient-id apclient))
                 #:object (asobj-id simple-note)
                 #:target (asobj-id collection)))
     ;; Assuming we got this far, it must have submitted okay
     ;; [outbox:remove]
     (report-on! 'outbox:remove <success>)

     ;; [outbox:remove:removes-from-target]
     ;; The object shouldn't be in the collection any more
     (if (not (note-is-member?))
         (report-on! 'outbox:remove:removes-from-target
                     <success>)
         (report-on! 'outbox:remove:removes-from-target
                     <fail>)))))

(define (test-outbox-activity-like case-worker)
  (define apclient (.apclient case-worker))
  (with-report
   '(outbox:like
     outbox:like:adds-object-to-liked)
   ;; @@: We could maybe switch this to being a post by a local
   ;;   pseudoactor, which is maybe less weird than liking your own
   ;;   post?
   (let* ((a-note (%create-and-retrieve-object
                   apclient
                   (as:note #:content "A very likable post!")))
          (like (%submit-asobj-and-retrieve
                 apclient
                 (as:like #:actor (uri->string (apclient-id apclient))
                          #:object (asobj-id a-note))))
          (liked-stream
           ;; Limit to 200 items so we don't search forever.
           ;; That should be more than enough.
           (stream-take 200 (apclient-liked-stream apclient))))
     (report-on! 'outbox:like <success>)
     (if (stream-member liked-stream
                        (lambda (asobj)
                          (equal? (asobj-id a-note)
                                  (asobj-id asobj))))
         (report-on! 'outbox:like:adds-object-to-liked <success>)
         (report-on! 'outbox:like:adds-object-to-liked <fail>)))))

(define (test-outbox-activity-block case-worker)
  (define apclient (.apclient case-worker))

  (with-report
   '(outbox:block
     outbox:block:prevent-interaction-with-actor)
   ;; First we need to create an actor
   (let* ((obnoxious-pseudoactor
           (case-worker-pseudoactor-new! case-worker))
          (block-asobj
           (%submit-asobj-and-retrieve
            apclient
            (as:block #:actor (uri->string (apclient-id apclient))
                      #:object (pseudoactor-id obnoxious-pseudoactor)))))
     ;; [outbox:block]
     (report-on! 'outbox:block <success>)

     ;; [outbox:block:prevent-interaction-with-actor]
     ;; Now we need to post a new post to the actor's inbox...
     (let*-values (((obnoxious-post)
                    (pseudoactor-asobj->outbox!
                     obnoxious-pseudoactor
                     (as:note #:content "Well, actually..."
                              #:attributedTo (pseudoactor-id obnoxious-pseudoactor)
                              #:to (uri->string (apclient-id apclient)))))
                   ((obnoxious-post-in-create)
                    (pseudoactor-asobj->outbox!
                     obnoxious-pseudoactor
                     (as:create #:object obnoxious-post
                                #:actor (pseudoactor-id obnoxious-pseudoactor)
                                #:to (uri->string (apclient-id apclient)))))
                   ((post-response _)
                    (apclient-post-asobj apclient (apclient-inbox-uri apclient)
                                         obnoxious-post-in-create)))
       (match (response-code post-response)
         (405 (report-on! 'outbox:block:prevent-interaction-with-actor
                          <inconclusive>
                          #:comment "Server does not appear to support federation (but might prevent local interactions appropriately)"))
         ;; @@: I guess the spec doesn't say what codes are acceptable
         ;;   for federated posts?
         (_
          ;; Now we have to see if it shows up in our inbox...
          (if (stream-member (stream-take 200 (apclient-inbox-stream apclient))
                             (lambda (asobj)
                               (member (asobj-id asobj)
                                       (list (asobj-id obnoxious-post)
                                             (asobj-id obnoxious-post-in-create)))))
              (report-on! 'outbox:block:prevent-interaction-with-actor
                          <fail>)
              (report-on! 'outbox:block:prevent-interaction-with-actor
                          <success>))))))))

(define (test-outbox-pseudoactors-stub case-worker)
  (let* ((pseudoactor
          (case-worker-pseudoactor-new! case-worker))
         (ps-id
          (pseudoactor-id pseudoactor)))
    (show-user
     `(p "Okay, check out "
         (a (@ (href ,ps-id))
            ,ps-id)))))


;;; server to server server tests

(define (test-s2s-server case-worker)
  ;; questions are lists of ('symbol text-question)
  (define (check-in title description questions)
    (%check-in case-worker title description questions))

  ;; *DELIVERY TESTS*

  (if (.testing-c2s-server? case-worker)
      (begin
        (check-in "Federating from the outbox"
                  '(p "Construct and submit activities to your actor's outbox making "
                      "use of the " (code "to") ", " (code "cc") ", " (code "bcc")
                      ", and " (code "bto") " addressing fields. ")
                  '((inbox:delivery:performs-delivery
                     "Server performed delivery on all Activities posted to the outbox")
                    (inbox:delivery:addressing
                     ("Server utilized " (code "to") ", " (code "cc") ", " (code "bcc")
                      ", and " (code "bto") " to determine delivery recipients."))))
        (check-in "Adding an id"
                  '("Submit an activity to your outbox without specifying an "
                    (code "id") ".  The server should add an " (code "id")
                    " to the object before delivering.")
                  '((inbox:delivery:adds-id
                     ("The server added an " (code "id") " to the activity."))))

        (check-in "Delivering with credentials"
                  '("Construct and deliver an activity with addressing pointing at "
                    "the id of a collection the activity's actor can access but "
                    "which is not on their server.")
                  `((inbox:delivery:submit-with-credentials
                     ("Did the server retrieve the members of the collection "
                      "by using the credentials of the actor? "
                      "(For example, if the actor has a public key on their profile, "
                      "the request may be signed with "
                      ,(link "https://tools.ietf.org/html/draft-cavage-http-signatures-08"
                             "HTTP Signatures") ".)"))
                    (inbox:delivery:deliver-to-collection
                     ("Did the server traverse the collection to deliver to the "
                      "inboxes of all items in the collection?"))
                    ;; TODO: This should be nested
                    (inbox:delivery:deliver-to-collection:recursively
                     ("Does the implementation deliver recursively to collections "
                      "within a collection (with some limit on recursion >= 1)?")))))
      (begin
        (for-each
         (lambda (sym)
           (report-on! sym <not-applicable>
                       #:comment "Tests only apply if supporting both C2S and S2S"))
         '(inbox:delivery:performs-delivery
           inbox:delivery:addressing
           inbox:delivery:adds-id
           inbox:delivery:submit-with-credentials
           inbox:delivery:deliver-to-collection
           inbox:delivery:deliver-to-collection:recursively))))

  (check-in "Activities requiring the object property"
            '("The distribution of the following activities require that they contain the "
              (code "object") " property: "
              (code "Create") ", " (code "Update") ", " (code "Delete")
              ", " (code "Follow") ", " (code "Add") ", " (code "Remove")
              ", " (code "Like") ", " (code "Block") ", " (code "Undo") ".")
            '((inbox:delivery:delivers-with-object-for-certain-activities
               '("Implementation always includes " (code "object") " property "
                 "for each of the above supported activities."))))
  (check-in "Activities requiring the target property"
            '("The distribution of the following activities require that they contain the "
              (code "target") " property: "
              (code "Add") ", " (code "Remove")".")
            '((inbox:delivery:delivers-with-target-for-certain-activities
               '("Implementation always includes " (code "target") " property "
                 "for each of the above supported activities."))))
  (check-in "Deduplication of recipient list"
            '("Attempt to submit for delivery an activity that addresses the "
              "same actor (ie an actor with the same " (code "id") ") twice. "
              "(For example, the same actor could appear on both the "
              (code "to") " and " (code "cc") " fields, or the actor could "
              "be explicitly addressed in " (code "to") " but could also be "
              "a member of the addressed " (code "followers") " collection "
              "of the sending actor.)  The server should deduplicate the "
              "list of inboxes to deliver to before delivering.")
            '((inbox:delivery:deduplicates-final-recipient-list
               '("The final recipient list is deduplicated before delivery."))))
  (check-in "Do-not-deliver considerations"
            #f
            '((inbox:delivery:do-not-deliver-to-actor
               ("Server does not deliver to recipients which are the same as the "
                "actor of the Activity being notified about"))
              (inbox:delivery:do-not-deliver-block
               ("Server does not deliver " (code "Block") " activities to "
                "their " (code "object") "."))))

  (show-user
   '(h3 "Tests for receiving objects to inbox"))

  ;; *RECEIVE TESTS*
  (check-in "Deduplicating received activities"
            #f
            '((inbox:accept:deduplicate
               ("Server deduplicates activities received in inbox by comparing "
                "activity " (code "id") "s"))))

  (check-in "Special forwarding mechanism"
            `("ActivityPub contains a "
              ,(link "https://www.w3.org/TR/activitypub/#inbox-delivery"
                     "special mechanism for forwarding replies")
              " to avoid \"ghost replies\".")

            `((inbox:accept:special-forward
               ("Forwards incoming activities to the values of "
                (code "to") ", " (code "bto") ", " (code "cc") ", " (code "bcc")
                ", " (code "audience")
                " if and only if "
                ,(link "https://www.w3.org/TR/activitypub/#inbox-delivery"
                       "criteria")
                " are met."))
              (inbox:accept:special-forward:recurses
               ("Recurse through "
                (code "to") ", " (code "bto") ", " (code "cc") ", " (code "bcc")
                ", " (code "audience")
                " object values to determine whether/where to forward "
                "according to criteria in 7.1.2"))
              (inbox:accept:special-forward:limits-recursion
               "Limits depth of this recursion.")))

  (check-in "Verification of content authorship"
            `("Before accepting activities delivered to an actor's inbox "
              "some sort of verification should be performed.  "
              "(For example, if the delivering actor has a public key on their profile, "
              "the request delivering the activity may be signed with "
              ,(link "https://tools.ietf.org/html/draft-cavage-http-signatures-08"
                     "HTTP Signatures") ".)")
            '((inbox:accept:dont-blindly-trust
               ("Don't trust content received from a server other than the "
                "content's origin without some form of verification."))))

  ;; SHOULD
  (check-in "Update activity"
            '("On receiving an " (code "Update") " activity to an actor's inbox, "
              "the server:")
            '((inbox:accept:update:is-authorized
               "Takes care to be sure that the Update is authorized to modify its object")
              (inbox:accept:update:completely-replace
               "Completely replaces its copy of the activity with the newly received value")))

  ;; * Follow
  (check-in "Activity acceptance side-effects"
            "Test accepting the following activities to an actor's inbox and observe the side effects:"
            `((inbox:accept:follow:add-actor-to-users-followers
               ((code "Follow")
                " should add the activity's actor to the receiving actor's "
                "Followers Collection."))
              ;; * Add
              (inbox:accept:add:to-collection
               ((code "Add")
                " should add the activity's " (code "object")
                " to the Collection specified in the "
                (code "target")
                " property, unless "
                ,(link "https://www.w3.org/TR/activitypub/#add-activity-inbox"
                       "not allowed per requirements")))
              (inbox:accept:remove:from-collection
               ((code "Remove")
                " should remove the object from the Collection specified in the "
                (code "target")
                " property, unless "
                ,(link "https://www.w3.org/TR/activitypub/#remove-activity-inbox"
                       "not allowed per requirements")))
              ;; * Like
              (inbox:accept:like:indicate-like-performed
               ((code "Like")
                " increments the object's count of likes by adding the "
                "received activity to the "
                ,(link "https://www.w3.org/TR/activitypub/#likes"
                       "likes")
                " collection if this collection is present"))))
  ;; (inbox:accept:validate-content
  ;;  SHOULD
  ;;  "Validate the content they receive to avoid content spoofing attacks.")
  )

(define (test-server-common case-worker)
  (define (check-in title description questions)
    (%check-in case-worker title description questions))

  ;; @@: Does this belong in c2s section?
  (check-in "Fetching the inbox"
            '("Try retrieving the actor's " (code "inbox") " of an actor.")
            '((server:inbox:responds-to-get
               "Server responds to GET request at inbox URL")
              (server:inbox:is-orderedcollection
               "inbox is an OrderedCollection")
              (server:inbox:filtered-per-permissions
               "Server filters inbox content according to the requester's permission")))

  ;; @@: Does this one even make sense to test for?  It's implied by the others
  ;; Object retrieval
  (check-in "Server: Retrieving objects"
            '("Retrieve an object from the server by its id by performing "
              "a GET request, presumably using "
              (code "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
              " and possibly "
              (code "application/activity+json")
              " in the " (code "Accept") " header.")
            '((server:object-retrieval:get-id
               "Successfully retrieved the object by performing GET against its id.")
              (server:object-retrieval:respond-with-as2-re-ld-json
               ("Server responded with the ActivityStreams object representation in response to " (code "Accept") " header of " (code "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")))
              (server:object-retrieval:respond-with-as2-re-activity-json
               ("Server responded with the ActivityStreams object representation in response to " (code "Accept") " header of " (code "application/activity+json")))))
  (check-in "Server: Retrieving deleted objects"
            '("Test deleting objects on the server and then retrieving "
              "the object by its " (code "id") ".")
            '((server:object-retrieval:deleted-object:tombstone
               ("Server responds with response body that is an ActivityStreams Object of type "
                (code "Tombstone")
                " (if the server is choosing to disclose that the object has been removed)"))
              (server:object-retrieval:deleted-object:410-status
               ("Respond with 410 Gone status code if " (code "Tombstone")
                " is in response body, otherwise responds with 404 Not Found"))
              (server:object-retrieval:deleted-object:404-status
               "Respond with 404 status code for Object URIs that have never existed")))

  (check-in "Server: Forbidding retrieval of private objects"
            #f
            '((server:object-retrieval:private-403-or-404
               "Respond with a 403 Forbidden status code to all requests that access Objects considered Private (or 404 if the server does not want to disclose the existence of the object, or another HTTP status code if specified by the authorization method)")))

  (check-in "Server security considerations"
            #f
            '((server:security-considerations:actually-posted-by-actor
               "Server verifies that the new content is really posted by the actor indicated in Objects received in inbox and outbox")
              (server:security-considerations:do-not-post-to-localhost
               "By default, implementation does not make HTTP requests to localhost when delivering Activities")
              (server:security-considerations:uri-scheme-whitelist
               "Implementation applies a whitelist of allowed URI protocols before issuing requests, e.g. for inbox delivery")
              (server:security-considerations:filter-incoming-content
               "Server filters incoming content both by local untrusted users and any remote users through some sort of spam filter")
              (server:security-considerations:sanitize-fields
               "Implementation takes care to santizie fields containing markup to prevent cross site scripting attacks"))))


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
  ((create-pseudoactor case-manager-create-pseudoactor))
  (workers #:init-thunk make-hash-table
           #:accessor .workers)
  (db-manager #:init-keyword #:db-manager
              #:accessor .db-manager)
  ;; This is a kludge... we really shouldn't have to double
  ;; record these, should we?
  (client-worker-map #:init-thunk make-hash-table
                     #:accessor .client-worker-map)
  ;; Pseudoactors represent actors in the activitypub sense
  (client-pseudoactors #:init-thunk make-hash-table
                       #:accessor .client-pseudoactors))

(define (case-manager-create-pseudoactor case-manager m client-id)
  "Create and register pseudoactor for client-id"
  (let ((client-psdas (hash-ref (.client-pseudoactors case-manager)
                                client-id))
        (psdo-actor
         (create-actor <pseudoactor>
                       #:client-id client-id)))
    (hash-set! client-psdas (<-wait psdo-actor 'get-username)
               psdo-actor)
    psdo-actor))

(define (case-manager-ws-client-connect case-manager client-id)
  (pk 'connected!)
  (let ((worker (create-reclusive-actor
                 <case-worker>
                 #:client-id client-id
                 #:manager (actor-id case-manager)
                 #:db-manager (.db-manager case-manager))))
    (hash-set! (.workers case-manager)
               client-id worker)
    (hash-set! (.client-pseudoactors case-manager)
               client-id (make-hash-table))
    'done))

(define* (case-manager-worker-ref case-manager client-id
                                  #:key (error-on-nothing #t))
  "Fetch worker with CLIENT-ID.

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
    (hash-remove! (.workers case-manager) client-id)
    (hash-for-each
     (lambda (psda _)
       (<- psda 'shutdown))
     (hash-ref (.client-pseudoactors case-manager) client-id))
    (hash-remove! (.client-pseudoactors case-manager) client-id)
    'done))

(define (case-manager-ws-new-message case-manager client-id raw-data)
  (let ((worker (case-manager-worker-ref case-manager client-id))
        ;; shadow data with version parsed from json
        (json-data (read-json-from-string raw-data)))
    (match (jsobj-ref json-data "action")
      ("send-input"
       (<- worker 'receive-input (jsobj-ref json-data "data")))
      ("rewind"
       (<- worker 'rewind)))))

(define (http-handler request body)
  (receive (view args)
      (route request)
    (apply view request body args)))


(define (main args)
  (run-hive
   (lambda (hive)
     (define (run db-path base-uri-arg)
       (with-extended-ctx
        `((base-uri . ,(string->uri base-uri-arg)))
        (lambda ()
          (define db-manager
            (create-actor <db-manager>
                          #:db-path db-path))
          (create-actor <case-manager>
                        #:db-manager db-manager
                        #:http-handler (live-wrap http-handler)
                        #:port 8989
                        #:on-ws-client-connect (live-wrap case-manager-ws-client-connect)
                        #:on-ws-client-disconnect (live-wrap case-manager-ws-client-disconnect)
                        #:on-ws-message (live-wrap case-manager-ws-new-message))
          (spawn-repl)
          (format #t "Running on ~a\n" base-uri-arg)
          (wait (make-condition)))))
     (match args
       ((_ db-path base-uri-arg)
        (run db-path base-uri-arg))
       ((_ db-path)
        (run db-path "http://localhost:8989/"))
       (_ (display "aptestsuite.scm DB-PATH [BASE-URI]"))))))
