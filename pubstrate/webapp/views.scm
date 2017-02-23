;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2016, 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (pubstrate webapp views)
  #:use-module (oop goops) ; for form-widgets
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate package-config)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate date)
  #:use-module (pubstrate contrib mime-types)
  #:use-module (pubstrate webapp auth)
  #:use-module (pubstrate webapp cookie)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp federation)
  #:use-module (pubstrate webapp fat-lean)
  #:use-module (pubstrate webapp form-widgets)
  #:use-module (pubstrate webapp store)
  #:use-module (pubstrate webapp sessions)
  #:use-module (pubstrate webapp templates)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp utils)
  #:use-module (pubstrate webapp side-effects)
  #:use-module ((pubstrate webapp http-status)
                #:renamer (symbol-prefix-proc 'status:))
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:export (index mockup
            user-page user-inbox user-outbox
            user-followers user-following
            login logout
            display-post asobj
            oauth-authorize

            ;; @@: Temporary!
            bearer-token-test

            standard-four-oh-four render-static))

(define %items-per-page 20)

(define (index request body)
  (respond-html (index-tmpl)))

(define (mockup request body)
  (respond-html (mockup-tmpl)))

(define (user-page request body username)
  (define (render-user-page)
    (let ((activities
           (user-collection-first-page
            (ctx-ref 'store) user "outbox"
            %items-per-page)))
      (respond-html
       (user-homepage-tmpl user activities
                           #f #f))))
  (define user (store-user-ref (ctx-ref 'store) username))
  (define (user-with-extra-endpoints)
    (asobj-cons user "endpoints"
                `(@ ("getAuthToken" ,(abs-local-uri "api" "get-auth-token"))
                    ("uploadMedia" ,(abs-local-uri "api" "upload-media")))))

  (cond
   ;; User not found, so 404
   ((not user)
    (respond-not-found))
   ;; Looks like they want the activitystreams object version..
   ((requesting-asobj? request)
    (respond (asobj->string (user-with-extra-endpoints))
             #:content-type 'application/activity+json))
   ;; Otherwise, give them the human-readable HTML!
   (else (render-user-page))))

(define (user-inbox request body username)
  (define store (ctx-ref 'store))
  ;; Obviously needs to be changed
  (define inbox-user
    (store-user-ref (ctx-ref 'store) username))
  ;; TODO: This is basically the same as in user-outbox.  DRY!
  (define (api-user-can-read?)
    (match (assoc-ref (request-headers request) 'authorization)
      (('bearer . (? string? token))
       (store-bearer-token-valid?
        store token inbox-user))
      (_ #f)))
  (define (logged-in-user-can-read?)
    (and=> (ctx-ref 'user)
           (lambda (u)
             (equal? (asobj-id inbox-user)
                     (asobj-id u)))))
  (define (user-can-read?)
    (or (api-user-can-read?)
        (logged-in-user-can-read?)))
  (define (post-to-inbox)
    ;; TODO: Add filtering hooks here
    ;; TODO: Validate content here
    (define asobj (string->asobj
                   (if (bytevector? body)
                       (utf8->string body)
                       body)
                   (%default-env)))
    (actor-post-asobj-to-inbox! inbox-user asobj)
    (respond ""))
  (define (read-from-inbox)
    (as2-paginated-user-collection request inbox-user username "inbox"))
  (match (request-method request)
    ('GET
     (if (user-can-read?)
         (read-from-inbox)
         (respond "Sorry, you don't have permission to read that."
                  #:status status:unauthorized
                  #:content-type 'text/plain)))
    ('POST
     (post-to-inbox))
    (_ (respond #:status status:method-not-allowed))))

;; Fixed, for now...
(define %items-per-page 10)

;; Not an actual view, but used to build inbox/outbox views
(define* (as2-paginated-user-collection request user username collection
                                        #:key
                                        (title
                                         (format #f "~a's ~a"
                                                 (user-name-str user)
                                                 collection)))
  (define* (abs-col-url-str #:optional page)
    (let ((url-str (abs-local-uri "u" username collection)))
      (if page
          (uri->string
           (uri-set (string->uri url-str)
                    #:query '((page . page))))
          url-str)))

  ;; TODO: in the future, we'll want to filter this based upon
  ;;   who's logged in / supplied auth.  For now, everything is public
  ;;   anyway.
  (define (maybe-add-next-prev ocp next prev)
    ((compose (lambda (ocp)
                (if next
                    (asobj-cons ocp "next"
                                (abs-col-url-str next))
                    ocp))
              (lambda (ocp)
                (if next
                    (asobj-cons ocp "prev"
                                (abs-col-url-str prev))
                    ocp)))
     ocp))

  (define (request-wants-as2?)
    (find (lambda (x)
            (member (car x) '(application/activity+json
                              application/ld+json)))
          (request-accept request)))

  (define (fatten-items asobjs)
    ;; We use the same retriever to keep around the cache
    (let ((retriever (make-retriever)))
      (map (lambda (asobj)
             (asobj-fatten asobj retriever))
           asobjs)))

  (let*-values (((form) (request-query-form request))
                ((page-id) (assoc-ref form "page"))
                ((is-first) (not page-id))
                ((col-url) (abs-col-url-str))
                ((page-items prev next)
                 (if is-first
                     (user-collection-first-page (ctx-ref 'store)
                                                 user collection
                                                 %items-per-page)
                     (user-collection-page (ctx-ref 'store)
                                           user collection
                                           page-id %items-per-page)))
                ((ordered-collection-page)
                 (maybe-add-next-prev
                  (make-as ^OrderedCollectionPage (%default-env)
                           #:partOf col-url
                           #:orderedItems (fatten-items page-items))
                  prev next))
                ((return-asobj)
                 (if is-first
                     ;; So, we want to return the toplevel of the paging
                     (make-as ^OrderedCollection (%default-env)
                              #:name title
                              #:id col-url
                              #:first ordered-collection-page)
                     ;; This is some page
                     ordered-collection-page)))

    (if (request-wants-as2?)
        (respond (asobj->string return-asobj)
                 #:content-type 'application/activity+json)
        (respond-html
         (base-tmpl
          `(div (@ (id "main-feed-block")
                   (class "generic-content-box"))
                (div (@ (class "pre-content-header"))
                     (h2 (@ (style "text-align: center;"))
                         "== " ,title " =="))
                ,(toplevel-activity-tmpl return-asobj)))))))

(define (user-outbox request body username)
  (define store (ctx-ref 'store))
  (define outbox-user
    (store-user-ref (ctx-ref 'store) username))
  (define (post-to-outbox)
    ;; TODO: Handle side effects appropriately.
    ;;   Currently doing a "dumb" version of things where we just dump it
    ;;   into the database.
    (define tweak-incoming-asobj
      ;; TODO: Also strip out any @id that may have been attached...
      (compose (lambda (asobj)
                 (let ((unique-id (abs-local-uri "u" username "p"
                                                 (gen-bearer-token 30))))
                   (asobj-cons asobj "id" unique-id)))
               ;; Copy the id of our actor onto the actor field
               (lambda (asobj)
                 (asobj-cons asobj "actor" (asobj-id outbox-user)))
               (lambda (asobj)
                 (asobj-cons asobj "published"
                             (date->rfc3339-string (current-date 0))))))
    (let* (;; This is the asobj as it first comes in from the body of the
           ;; request.
           (initial-asobj (string->asobj
                           (if (bytevector? body)
                               (utf8->string body)
                               body)
                           (%default-env)))
           ;; Here we've first done some common "tweaks" on the asobj,
           ;; then allowed our asobj to handle all its appropriate side
           ;; effects, including saving to the store.
           (asobj (asobj-outbox-effects!
                   (tweak-incoming-asobj initial-asobj)
                   outbox-user)))
      ;; @@: Do we do this here?  Or do we do this in the 
      ;; (store-asobj-set! store asobj)
      (user-add-to-outbox! store outbox-user (asobj-id asobj))
      (deliver-asobj asobj)
      (respond (asobj->string asobj)
               #:status status:created
               #:content-type 'application/activity+json)))
  (define* (abs-outbox-url-str #:optional page)
    (let ((url-str (abs-local-uri "u" username "outbox")))
      (if page
          (uri->string
           (uri-set (string->uri url-str)
                    #:query '((page . page))))
          url-str)))
  (define (read-from-outbox)
    ;; TODO: in the future, we'll want to filter this based upon
    ;;   who's logged in / supplied auth.  For now, everything is public
    ;;   anyway.
    (as2-paginated-user-collection request outbox-user username "outbox"))
  (define (api-user-can-post?)
    (match (assoc-ref (request-headers request) 'authorization)
      (('bearer . (? string? token))
       (store-bearer-token-valid?
        store token outbox-user))
      (_ #f)))
  (match (request-method request)
    ('GET
     (read-from-outbox))
    ('POST
     (if (api-user-can-post?)
         (post-to-outbox)
         (respond "Sorry, you don't have permission to post that."
                  #:status status:unauthorized
                  #:content-type 'text/plain)))
    (_ (respond #:status status:method-not-allowed))))

(define* (make-read-user-collection-view collection-name
                                         #:key
                                         (gen-title
                                          (lambda (user-name-str collection)
                                            (format #f "~a's ~a" user-name-str collection)))
                                         (on-nothing "There's nothing here."))
  (lambda (request body username)
    (define store (ctx-ref 'store))
    (define view-user
      (store-user-ref (ctx-ref 'store) username))

    (match (request-method request)
      ('GET
       (as2-paginated-user-collection request view-user username collection-name
                                      #:title (gen-title (user-name-str view-user)
                                                         collection-name)))
      (_ (respond #:status status:method-not-allowed)))))

(define user-followers
  (make-read-user-collection-view "followers"))

(define user-following
  (make-read-user-collection-view "following"
                                  #:gen-title
                                  (lambda (user-name-str collection)
                                    (format #f "following ~a" user-name-str))))

(define login-form
  (make-form
   (make <text-field>
     #:name "username"
     #:label "Username")
   (make <password-field>
     #:name "password"
     #:label "Password")))

(define (login request body)
  (define store (ctx-ref 'store))
  (define session-manager
    (ctx-ref 'session-manager))
  (match (request-method request)
    ('GET
     (let* ((form (request-query-form request))
            (next (assoc-ref form "next")))
       (respond-html (login-tmpl login-form
                                 #:next next))))
    ('POST
     (let* ((form (urldecode body))
            (username (assoc-ref form "username"))
            (password (assoc-ref form "password"))
            (user (if username
                      (store-user-ref store username)
                      #f))
            (next (assoc-ref form "next"))
            (redirect-to (or next (local-uri ""))))
       (if (and  user (user-password-matches? user password))
           (respond-redirect redirect-to
                             #:extra-headers
                             (list
                              ;; TODO: this should just *add to* the session,
                              ;;   not clobber it...
                              (set-session session-manager
                                           `((user . ,username)))))
           (respond-html (login-tmpl login-form
                                     #:try-again #t
                                     #:next next)))))))

(define (logout request body)
  (respond-redirect (local-uri "")
                    #:extra-headers
                    (list (delete-session (ctx-ref 'session-manager)))))

(define (display-post request body username post-id)
  ;; GET only.
  (let* ((post-url (abs-local-uri "u" username "p" post-id))
         (asobj (store-asobj-ref (ctx-ref 'store) post-url)))
    (match (request-method request)
      ('GET
       ;; TODO: authorization check?
       (cond
        ((requesting-asobj? request)
         (respond (asobj->string asobj)
                  #:content-type 'application/activity+json))
        (else
         (if asobj
             (respond-html (base-tmpl (toplevel-activity-tmpl asobj)))
             (respond-not-found)))))
      (_ (respond #:status status:method-not-allowed)))))

(define (oauth-authorize request body)
  (require-login
   request
   (lambda ()
     (match (request-method request)
       ('GET
        (respond-html
         (base-tmpl
          (centered-content-tmpl
           `(div (@ (style "text-align: center;"))
                 (h1 "Authorize application?")
                 (p "An application is requesting access to your stream. "
                    "Grant them access?")
                 (form (@ (action "")
                          (method "POST")
                          (enctype "application/x-www-form-urlencoded"))
                       (div (button (@ (type "submit")
                                       (name "access")
                                       (value "granted"))
                                    "Yes")
                            " "
                            (button (@ (type "submit")
                                       (name "access")
                                       (value "denied"))
                                    "No")))))
          #:title "Authorize application?")))
       ('POST
        (cond
         ((equal? (assoc-ref (urldecode body)
                             "access")
                  "granted")
          (respond-html
           (let ((token (store-bearer-token-new!
                         (ctx-ref 'store) (ctx-ref 'user))))
             (base-tmpl
              (centered-content-tmpl
               '(h1 "Authorization granted!")
               '(p "Paste this token back in the application:")
               `(blockquote (pre ,token)))))))
         (else
          (respond-redirect (local-uri "")))))))))

(define (standard-four-oh-four request body)
  ;; TODO: Add 404 status message
  (respond-not-found))

;;; Static site rendering... only available on devel instances (hopefully!)
(define (render-static request body static-path)
  (respond
   (call-with-input-file (web-static-filepath static-path) get-bytevector-all)
   #:content-type (mime-type static-path)))
