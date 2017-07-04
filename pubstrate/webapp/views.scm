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
  #:use-module (gcrypt random)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (webutils cookie)
  #:use-module (webutils sessions)
  #:use-module (webutils multipart)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate generics)
  #:use-module (pubstrate package-config)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate contrib mime-types)
  #:use-module (pubstrate webapp auth)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp inbox-outbox)
  #:use-module (pubstrate webapp fat-lean)
  #:use-module (pubstrate webapp filestore)
  #:use-module (pubstrate webapp form-widgets)
  #:use-module (pubstrate webapp db)
  #:use-module (pubstrate webapp templates)
  #:use-module (pubstrate webapp user)
  #:use-module (pubstrate webapp utils)
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
            upload-media

            standard-four-oh-four
            render-static render-media))

;; Fixed, for now...
(define %items-per-page 10)

(define (index request body)
  (respond-html (index-tmpl)))

(define (mockup request body)
  (respond-html (mockup-tmpl)))

(define (user-page request body username)
  (define (render-user-page)
    (let ((activities
           (fatten-asobjs
            (user-collection-first-page
             (ctx-ref 'db) user "outbox"
             %items-per-page))))
      (respond-html
       (user-homepage-tmpl user activities
                           #f #f))))
  (define user (db-user-ref (ctx-ref 'db) username))
  (define (user-with-extra-endpoints)
    (asobj-set user "endpoints"
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

(define (%logged-in-as? this-user)
  (define current-user
    (ctx-ref 'user))
  (and current-user
       (equal? (asobj-id current-user)
               (asobj-id this-user))))

(define (user-inbox request body username)
  (define db (ctx-ref 'db))
  ;; Obviously needs to be changed
  (define inbox-user
    (db-user-ref (ctx-ref 'db) username))
  ;; TODO: This is basically the same as in user-outbox.  DRY!
  (define (user-can-read?)
    (%logged-in-as? inbox-user))
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

;; Not an actual view, but used to build inbox/outbox views
(define* (paginated-collection collection #:optional page-id
                               (container-key (asobj-private-ref collection "container")))
  "Generate a paginated collection, starting with stub COLLECTION
asobj, and if PAGE-ID returning that specific page, or returning the
toplevel COLLECTION with the \"first\" page property set."
  (define col-id
    (asobj-id collection))
  (define col-id-uri
    (string->uri col-id))
  (define* (page-uri-str page)
    (uri->string
     (uri-set col-id-uri
              #:query `((page . ,page)))))

  ;; TODO: in the future, we'll want to filter this based upon
  ;;   who's logged in / supplied auth.  For now, everything is public
  ;;   anyway.
  (define (maybe-add-next-prev ocp next prev)
    ;; ocp is OrderedCollectionPage
    ((compose (lambda (ocp)
                (if next
                    (asobj-set ocp "next"
                               (page-uri-str next))
                    ocp))
              (lambda (ocp)
                (if prev
                    (asobj-set ocp "prev"
                               (page-uri-str prev))
                    ocp)))
     ocp))

  (define db (ctx-ref 'db))

  (let*-values (((page-items prev next)
                 (if page-id
                     (db-container-page db container-key
                                        page-id %items-per-page)
                     (db-container-first-page db container-key
                                              %items-per-page)))
                ;; retrieve the full objects of these, assuming we have them
                ((page-items)
                 (map (lambda (id)
                        (or (db-asobj-ref db id)
                            id))
                      page-items))
                ((ordered-collection-page)
                 (maybe-add-next-prev
                  (make-as ^OrderedCollectionPage (%default-env)
                           #:partOf col-id
                           #:orderedItems (fatten-asobjs page-items))
                  next prev)))
    (if page-id
        ;; This is a specific page
        ordered-collection-page
        ;; We want the toplevel, with the first page
        (asobj-set collection
                   "first" ordered-collection-page))))

(define* (as2-paginated-user-collection request user username collection-name
                                        #:key
                                        (title
                                         (format #f "~a's ~a"
                                                 (user-name-str user)
                                                 collection-name)))
  (let* ((db (ctx-ref 'db))
         (collection-id (abs-local-uri "u" username collection-name))
         (container-key (db-user-container-key db user collection-name))
         (page-id (assoc-ref (request-query-form request) "page"))
         (initial-collection
          (make-as ^OrderedCollection (%default-env)
                   #:id collection-id
                   #:name title))
         (return-asobj (paginated-collection initial-collection
                                             page-id container-key))
         (wants-as2? (find (lambda (x)
                             (member (car x) '(application/activity+json
                                               application/ld+json)))
                           (request-accept request))))
    (if wants-as2?
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


(define (%post-to-outbox initial-asobj outbox-user)
  (define db (ctx-ref 'db))
  ;; TODO: Handle side effects appropriately.
  ;;   Currently doing a "dumb" version of things where we just dump it
  ;;   into the database.
  (let*-values (;; Here we've first done some common "tweaks" on the asobj,
                ;; then allowed our asobj to handle all its appropriate side
                ;; effects.
                ((asobj) (asobj-outbox-effects! initial-asobj
                                                outbox-user))
                ;; In determining the recipients of an activitystreams object,
                ;; we very well may determine a new structure of the asobj,
                ;; especially because we're likely to strip off the bcc/bto
                ;; fields.
                ((recipients asobj)
                 (collect-recipients asobj)))
    (db-asobj-set! db asobj)                           ; save asobj
    (user-add-to-outbox! db outbox-user (asobj-id asobj)) ; add to inbox
    (deliver-asobj asobj recipients)                         ; deliver it
    (respond (asobj->string asobj)
             #:status status:created
             #:content-type 'application/activity+json
             #:extra-headers `((location . ,(string->uri (asobj-id asobj)))))))

(define (%body->asobj body)
  (string->asobj
   (if (bytevector? body)
       (utf8->string body)
       body)
   (%default-env)))

(define (user-outbox request body username)
  (define db (ctx-ref 'db))
  (define outbox-user
    (db-user-ref (ctx-ref 'db) username))
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
    (%logged-in-as? outbox-user))
  (match (request-method request)
    ('GET
     (read-from-outbox))
    ('POST
     (if (api-user-can-post?)
         (%post-to-outbox (%body->asobj body) outbox-user)
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
    (define db (ctx-ref 'db))
    (define view-user
      (db-user-ref (ctx-ref 'db) username))

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
  (define db (ctx-ref 'db))
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
                      (db-user-ref db username)
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

(define-as-generic %tweak-for-display
  "Tweak an asobj for display in display-post.")

;; TODO: This too should use access control.
;; By default, we don't do any tweaks for objects.
(define-as-method (%tweak-for-display (asobj ^Object) request)
  asobj)

;; But for collections we definitely do!
(define-as-method (%tweak-for-display (asobj ^Collection) request)
  (paginated-collection asobj (assoc-ref (request-query-form request) "page")))

(define (display-post request body username post-id)
  ;; GET only.
  (let* ((post-url (abs-local-uri "u" username "p" post-id))
         (asobj (and=> (db-asobj-ref (ctx-ref 'db) post-url)
                       (compose asobj-fatten
                                (lambda (asobj)
                                  (%tweak-for-display asobj request))))))
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
           (let ((token (db-bearer-token-new!
                         (ctx-ref 'db) (ctx-ref 'user))))
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
;;; TODO: This should really stream output to a port, not read/write
;;;   it as one big bytevector.  That requires transitioning from guile's
;;;   builtlin server to a more stream-y fibers or 8sync server though.
(define (static-renderer get-file-port)
  (lambda (request body path-parts)
    (let* ((file-port
            (get-file-port path-parts))
           (file-bv
            (get-bytevector-all file-port)))
      (if file-port
          (begin
            (close file-port)
            (respond file-bv
                     #:content-type (mime-type (last path-parts))))
          (respond-not-found)))))

(define render-static
  (static-renderer
   (lambda (path-parts)
     (let* ((partial-path (string-append "/" (string-join
                                              path-parts "/")))
            (full-path (web-static-filepath partial-path)))
       (and (file-exists? full-path)
            (open-file full-path "r"))))))

;;; Likewise with media serving...
(define render-media
  (static-renderer
   (lambda (path-parts)
     (let ((filestore (ctx-ref 'filestore)))
       (and (filestore-file-exists? filestore path-parts)
            (filestore-open-read filestore path-parts))))))

(define (upload-media request body)
  (cond
   ((ctx-ref 'user) =>
    (lambda (user)
      (match (request-method request)
        ('GET
         (respond-html
          (base-tmpl
           (centered-content-tmpl
            `(form (@ (action "")
                      (method "POST")
                      (enctype "multipart/form-data"))
                   (table
                    (tr (th "Object")
                        (td (textarea (@ (name "object"))
                                      "")))
                    (tr (th "File")
                        (td (input (@ (name "file")
                                      (type "file")))))
                    (tr (td)
                        (td (button
                             (@ (type "submit"))
                             "Submit")))))))))
        ('POST
         (let* ((parts (parse-request-body request body))
                (form-file-part (parts-ref parts "file"))
                (form-file-body (part-body form-file-part))
                (filename (basename ;basename so nobody makes us make a bunch of subdirs
                           (assoc-ref (part-content-disposition-params
                                       form-file-part)
                                      'filename)))
                (username (asobj-ref user "preferredUsername"))
                (full-filepath (list username (random-token) filename))
                (file-object (filestore-open-write (ctx-ref 'filestore) full-filepath))
                (file-link (apply abs-local-uri "media" full-filepath))
                ;; Add file path to the activitystreams object
                (asobj (asobj-set (string->asobj
                                   (get-string-all (part-body
                                                    (parts-ref parts "object")))
                                   (%default-env))
                                  "url" (make-as ^Link (%default-env)
                                                 #:href file-link))))
           ;; stream data to file object
           (let lp ()
             (match (get-bytevector-n form-file-body 512) ;512 bytes at a time
               ((? bytevector? bv)
                (put-bytevector file-object bv)
                (lp))
               ((? eof-object? _)
                ;; (close form-file-body) ; ???
                (close file-object))))
           ;; Now make final modifications and save in the db
           (%post-to-outbox asobj user))))))
   (else
    (respond-html
     (base-tmpl
      `(p "Sorry, you don't seem to be logged in?"))
     #:status status:unauthorized))))
