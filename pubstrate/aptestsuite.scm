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
             (pubstrate apclient)
             (pubstrate contrib html)
             (pubstrate webapp utils)
             (pubstrate webapp ctx)
             (pubstrate webapp form-widgets)
             ((pubstrate webapp views)
              #:renamer (symbol-prefix-proc 'ps-view:)))


(define* (base-template body #:key title)
  `(html (@ (xmlns "http://www.w3.org/1999/xhtml"))
         (head (meta (@ (charset "utf-8")))
               (title ,(if title
                           (string-append title " -- Pubstrate")
                           "Pubstrate"))
               ;; (link (@ (rel "stylesheet")
               ;;          (href ,(local-uri "static" "css" "main.css"))))
               (link (@ (rel "stylesheet")
                        (href ,(local-uri "static" "aptestsuite" "aptestsuite.css"))))
               (script (@ (type "text/javascript")
                          (src ,(local-uri "static" "aptestsuite" "testsuite.js")))
                       ""))
         ))

(define (view:main-display request body)
  (define (one-entry msg)
    `(div (@ (class "stream-entry"))
          (p ,msg)))
  (define test-stream-contents
    (list
     ;; (one-entry "Your drink order sir?")
     ;; (one-entry "Sir, your drinks!")
     ;; (one-entry "Sir, are you listening to me?")
     ;; `(div (@ (class "simple-centered-wrap"))
     ;;       (div (@ (class "prompt-user")
     ;;               (id "prompt-active"))
     ;;            (h2 "What would you like to eat?")
     ;;            (p "I hope you like cafeteria food")
     ;;            (ul (li (input (@ (name "sandwich")
     ;;                              (type "checkbox")))
     ;;                    " Sandwich")
     ;;                (li (input (@ (name "drink")
     ;;                              (type "checkbox")))
     ;;                    " Drink")
     ;;                (li (input (@ (name "dessert")
     ;;                              (type "checkbox")))
     ;;                    " Dessert"))
     ;;            (div (@ (class "prompt-button-metabox"))
     ;;                 (button "Submit")
     ;;                 (button "Back"))))
     ;; (one-entry "Your drink order sir?")
     ;; (one-entry "Sir, your drinks!")
     ;; (one-entry "Sir, are you listening to me?")
     ))
  (define body-tmpl
    `((div (@ (id "stream-metabox"))
           (div (@ (id "stream"))
                ,@test-stream-contents))
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
               #:accessor .checkpoints))

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
  (with-case-worker-user-io
   case-worker
   (lambda (_ show-user get-user-input)
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
        ;; Restore checkpoints state to what it was
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

(define (with-case-worker-user-io case-worker proc)
  "Call script PROC with IO procedures that are case-worker driven.

This passes two useful arguments to PROC:
 - show-user: pass it any sxml and it will render it as html
   and pass it to the stream of notifications the user gets.
 - get-user-input: pass it sxml including html input elements.
   This will be presented to the user; PROC will suspend its continuation
   until a response comes back to the user, so the calling this magically
   will just return the appropriate values when ready.
   (The client js takes care of extracting the values from the input
   fields.)"
  (with-user-io-prompt
   case-worker
   (lambda ()
     (define (gen-payload type sxml)
       (write-json-to-string
        `(@ ("type" ,type)
            ("can-go-back" ,(not (eq? (.checkpoints case-worker)
                                      '())))
            ("content" 
             ,(with-output-to-string
                (lambda ()
                  (sxml->html sxml (current-output-port))))))))
     (define (show-user sxml)
       (<- (.manager case-worker) 'ws-send
           (.client-id case-worker)
           (gen-payload "notice" sxml)))
     (define* (get-user-input sxml #:key (checkpoint #t))
       (when checkpoint
         (abort-to-prompt %user-io-prompt '*save-checkpoint*))
       (abort-to-prompt %user-io-prompt '*user-io*
                        (gen-payload "input-prompt" sxml)))
     (proc case-worker show-user get-user-input))))

(define (case-worker-init-and-run case-worker m . args)
  (with-case-worker-user-io
   case-worker demo-script))

(define (report-it! case-worker key val)
  (set! (.report case-worker) (acons key val (.report case-worker))))

(define* (report-ref case-worker key #:key dflt)
  (match (assoc key (.report case-worker))
    ((key . val) val)))

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
    (show-user "and we're done"))

  ;; (main-menu)
  ;; (when (hashq-ref report 'sandwich)
  ;;   (let ((user-input)))

    ;; > What kind of sandwich?
    ;; .-----------------------.
    ;; | (o) Tofu reuben       |
    ;; | ( ) Philly seitan     |
    ;; | ( ) Veggie delite     |
    ;; |               [submit]|
    ;; '-----------------------'

    ;; > Okay hold on while we get that ready for you.
    ;; > *Butchering tofu...*
    ;; > "Hey Timmy, could ya bring me some condiments?"
    ;; > *Waiting on condiments...*
    ;; > *Received condiments!*
    ;; > "Thanks Timmy!"
    ;; > *Assembles bread*
    ;; > "Say, did you want pickles or cheese with this?"
    ;; .----------------------.
    ;; | [ ] Pickles          |
    ;; | Cheese:              |
    ;; |  ( ) Daiya           |
    ;; |  (X) Cheddar         |
    ;; |  ( ) Mozzerella      |
    ;; |              [submit]|
    ;; '----------------------'
    ;; > "Ok, no pickles and Mozzerella."
    ;; > *Assembles rest of sandwich*
    ;; > "Ok, here's your sandwich!"

  ;; > That'll be $11.50.
  ;; .----------------------.
  ;; | Pay amount $[12.00]  |
  ;; |              [submit]|
  ;; '----------------------'
  ;; > "Here's your change."  *hands $0.50*
  ;; > "Here's your cup.  You can fill your drink over there."
  ;; .----------------------.
  ;; | Pick yer soda        |
  ;; |  (O) Starberry       |
  ;; |  ( ) Rube Bear       |
  ;; |  ( ) Corpus Cola     |
  ;; |              [submit]|
  ;; '----------------------'
  ;; > Congrats, you're now eating a delicious meal of
  ;;   Tofu reuben with cheddar cheese and a starberry soda
  )


(define (receive-input case-worker m input)
  (match (.input-kont case-worker)
    ;; Ignore if we don't have input
    (#f #f)
    ;; Otherwise, resume with this input provided
    (kont (kont input))))

(define-actor <case-manager> (<websocket-server>)
  ((send-msg-to-client case-manager-send-msg-to-client)
   (input-request-to-client case-manager-input-request-to-client))
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

;; @@: Are these needed really?
(define (case-manager-send-msg-to-client case-manager m msg)
  'TODO)

(define (case-manager-input-request-to-client case-manager m msg)
  'TODO)

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
