;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (pubstrate apclient)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate contrib define-method-star)
  #:use-module (pubstrate webapp utils) ;webapp?? well, for uri-set
  #:use-module (pubstrate webapp auth)  ; allow bearer tokens, etc
  #:use-module (sjson utils)
  #:use-module (srfi srfi-11)  ; let-values
  #:use-module (srfi srfi-41)  ; streams
  #:use-module (srfi srfi-26)  ; cut
  #:use-module (webutils multipart)
  #:export (<apclient>
            apclient-id apclient-auth-token

            make-apclient
            apclient-user
            apclient-get apclient-post
            apclient-get-asobj apclient-post-asobj
            apclient-inbox-uri apclient-outbox-uri
            apclient-followers-uri apclient-following-uri
            apclient-liked-uri
            apclient-media-uri
            apclient-inbox apclient-outbox
            apclient-followers apclient-following
            apclient-liked
            apclient-inbox-stream apclient-outbox-stream
            apclient-followers-stream apclient-following-stream
            apclient-liked-stream
            apclient-get-local apclient-get-local-asobj
            apclient-post-local apclient-post-local-asobj

            apclient-collection-page-stream
            apclient-collection-item-stream

            apclient-submit
            apclient-submit-media apclient-submit-file

            http-get-asobj http-post-asobj

            stream-member

            generate-reply-shell))

;; TODO: Add authentication info, etc...
(define-class <apclient> ()
  (id #:init-keyword #:id
      #:getter apclient-id)
  (auth-token #:init-keyword #:auth-token
              #:accessor apclient-auth-token)
  ;; Filled in as needed
  (user #:init-value #f)   ; @@: Why not let the user supply this one?
  (inbox-uri #:init-value #f)
  (outbox-uri #:init-value #f))

(define (ensure-uri obj)
  (match obj
    ((? string? obj)
     (string->uri obj))
    ((? uri? obj)
     obj)))

(define* (make-apclient id #:key auth-token)
  (let* ((id (ensure-uri id))
         (apclient
          (make <apclient> #:id id)))
    (if auth-token
        (slot-set! apclient 'auth-token auth-token))
    (apclient-user apclient)
    apclient))

(define as2-content-type-header
  '(content-type . (application/ld+json
                    (profile . "https://www.w3.org/ns/activitystreams"))))

(define as2-accept-header
  '(accept . ((application/ld+json
               (profile . "https://www.w3.org/ns/activitystreams")))))

(define (headers-look-like-as2? headers)
  (match (assoc-ref headers 'content-type)
    (((or 'application/activity+json 'application/ld+json) rest ...)
     #t)
    (_ #f)))

(define (apclient-user apclient)
  "Return user object for apclient, and possibly set up if not already fetched"
  (define (retrieve-user)
    (receive (response body)
        (http-get (apclient-id apclient)
                  #:headers (list as2-accept-header))
      (catch 'json-error
        (lambda ()
          (make-asobj
           (read-json-from-string
            (if (bytevector? body)
                (utf8->string body)
                body))
           (%default-env)))
        (lambda _
          (throw 'invalid-as2
                 #:response response
                 #:body body)))))
  (match (slot-ref apclient 'user)
    (#f
     (let ((user (retrieve-user)))
       (slot-set! apclient 'user user)
       user))
    ;; If it's already set up, return it as-is..
    (user
     user)))

(define (%apclient-uri-property property)
  (lambda (apclient)
    (let ((user (apclient-user apclient)))
      (and=> (asobj-ref user property)
             string->uri))))

(define (%apclient-get-local-asobj uri-fetcher)
  (lambda (apclient)
    (match (uri-fetcher apclient)
      (#f #f)
      (fetched
       (receive (response body)
           (apclient-get-local-asobj apclient fetched)
         (when (not (= (response-code response) 200))
           (throw 'apclient-response-not-ok
                  "Got a response that wasn't 200 OK"
                  #:response response))
         (when (not (asobj? body))
           (throw 'apclient-response-not-as2
                  "No ActivityStreams object returned"))
         body)))))

(define* (apclient-collection-page-stream apclient collection
                                          #:key local?)
  "Return a stream of pages from AS2 COLLECTION, traversed through APCLIENT"
  (letrec* ((fetch-page
             (lambda (page-id)
               (if local?
                   ;; @@: what happens if next is included
                   ;;   recursively as an asobj?  yikes that would
                   ;;   be bad tho
                   (receive (response asobj)
                       (apclient-get-local-asobj apclient page-id)
                     asobj)
                   ;; TODO: non-local fetch
                   'TODO)))
            (get-next-page
             (lambda (this-page)
               (cond
                ((asobj-ref this-page "next") =>
                 (lambda (next-page-id)
                   (let ((page (fetch-page next-page-id)))
                     (stream-cons page
                                  (get-next-page page)))))
                (else
                 stream-null)))))
    (match (asobj-ref collection "first")
      ((? asobj? first-page)
       (stream-cons first-page (get-next-page first-page)))
      ((? string-uri? first-page-uri)
       (let ((page (fetch-page first-page-uri)))
         (stream-cons page
                      (get-next-page page))))
      (#f
       (cond
        ;; if the object itself has items, we consider it the
        ;; first and only page
        ((or (asobj-ref collection "orderedItems")
             (asobj-ref collection "items"))
         (stream-cons collection
                      stream-null))
        (else
         stream-null))))))

(define* (apclient-collection-item-stream apclient collection
                                          #:key local?)
  "Return a stream of items from AS2 COLLECTION, traversed through APCLIENT"
  (define pages-stream
    (apclient-collection-page-stream apclient collection
                                     #:local? local?))
  (define (item-as-asobj item)
    (match item
      ((? asobj? item)
       item)
      ((? string-uri? item)
       (receive (response asobj)
           (apclient-get-local-asobj apclient item)
         asobj))))
  (let pages-loop ((pages pages-stream))
    (if (stream-null? pages)
        stream-null
        (let ((page (stream-car pages)))
          (let items-loop ((items 
                            (or (asobj-ref page "orderedItems")
                                (asobj-ref page "items")
                                '())))
            (match items
              ;; we've reached the end of this page...
              (() (pages-loop (stream-cdr pages)))
              ((item rest-items ...)
               (stream-cons (item-as-asobj item)
                            (items-loop rest-items)))))))))

(define* (%apclient-get-item-stream get-collection)
  (lambda (apclient)
    (match (get-collection apclient)
      (#f stream-null)
      (collection
       (apclient-collection-item-stream
        apclient collection
        #:local? #t)))))

(define apclient-inbox-uri
  (%apclient-uri-property "inbox"))
(define apclient-outbox-uri
  (%apclient-uri-property "outbox"))
(define apclient-followers-uri
  (%apclient-uri-property "followers"))
(define apclient-following-uri
  (%apclient-uri-property "following"))
(define apclient-liked-uri
  (%apclient-uri-property "liked"))
(define apclient-media-uri
  (%apclient-uri-property '("endpoints" "uploadMedia")))

(define apclient-inbox
  (%apclient-get-local-asobj apclient-inbox-uri))
(define apclient-outbox
  (%apclient-get-local-asobj apclient-outbox-uri))
(define apclient-followers
  (%apclient-get-local-asobj apclient-followers-uri))
(define apclient-following
  (%apclient-get-local-asobj apclient-following-uri))
(define apclient-liked
  (%apclient-get-local-asobj apclient-liked-uri))

(define apclient-inbox-stream
  (%apclient-get-item-stream apclient-inbox))
(define apclient-outbox-stream
  (%apclient-get-item-stream apclient-outbox))
(define apclient-followers-stream
  (%apclient-get-item-stream apclient-followers))
(define apclient-following-stream
  (%apclient-get-item-stream apclient-following))
(define apclient-liked-stream
  (%apclient-get-item-stream apclient-liked))

(define-method (apclient-auth-headers apclient)
  "Return whatever headers are appropriate for authorization given apclient"
  (apclient-user apclient)  ; called for side effects, make sure user asobj is fetched
  `((authorization . (bearer . ,(apclient-auth-token apclient)))))

(define (response-with-body-maybe-as-asobj response body)
  (values response
          (if (headers-look-like-as2? (response-headers response))
              (make-asobj (read-json-from-string
                           (if (bytevector? body)
                               (utf8->string body)
                               body))
                          (%default-env))
              body)))

(define-method* (apclient-get (apclient <apclient>) uri
                              #:key (headers '()))
  (http-get-async uri #:headers headers))

(define-method* (apclient-get-asobj (apclient <apclient>) uri
                                    #:key (headers '()))
  "Get URI with BODY as asobj, using appropriate local authentication"
  (call-with-values
      (lambda ()
        (apclient-get apclient uri
                      #:headers (cons as2-accept-header headers)))
    response-with-body-maybe-as-asobj))

(define-method* (apclient-get-local (apclient <apclient>) uri
                                    #:key (headers '())
                                    #:allow-other-keys)
  "Get URI with BODY, using appropriate local authentication"
  (apclient-get apclient uri
                #:headers (append (apclient-auth-headers apclient)
                                  headers)))

(define-method* (apclient-get-local-asobj (apclient <apclient>) uri
                                          #:key (headers '())
                                          #:allow-other-keys)
  "Get URI with BODY as asobj, using appropriate local authentication"
  (call-with-values
      (lambda ()
        (apclient-get-local apclient uri
                            #:headers (cons as2-accept-header headers)))
    response-with-body-maybe-as-asobj))

(define-method* (apclient-post (apclient <apclient>) uri body
                               #:key (headers '()))
  (http-post-async uri
                   #:headers headers
                   #:body body))

(define-method* (apclient-post-asobj (apclient <apclient>) uri asobj
                                     #:key (headers '()))
  (call-with-values
      (lambda ()
        (http-post-async uri
                         #:headers (cons as2-accept-header headers)
                         #:body (asobj->string asobj)))
    response-with-body-maybe-as-asobj))

(define-method* (apclient-post-local (apclient <apclient>) uri body
                                     #:key (headers '()))
  (apclient-post apclient uri body 
                 #:headers (append (apclient-auth-headers apclient)
                                   headers)))

(define-method* (apclient-post-local-asobj (apclient <apclient>) uri asobj
                                           #:key (headers '())
                                           #:allow-other-keys)
  "Post ASOBJ to local URI using APCLIENT's credentials.

 (Warning!  If this is used to post to a remote object, it'll
expose local credentials...)"
  (call-with-values
      (lambda ()
        (apclient-post-local apclient uri
                             (asobj->string asobj)
                             #:headers (cons as2-content-type-header
                                             (cons as2-accept-header headers))))
    response-with-body-maybe-as-asobj))

(define* (apclient-submit apclient asobj)
  "Submit ASOBJ to APCLIENT's outbox."
  (apclient-post-local-asobj apclient (apclient-outbox-uri apclient)
                             asobj))

(define* (http-get-asobj uri #:key (headers '()))
  (call-with-values
      (lambda ()
        (http-get-async uri
                        #:headers (cons as2-accept-header headers)))
    response-with-body-maybe-as-asobj))

(define (apclient-submit-media apclient asobj media filename)
  "Submit MEDIA (a port or bytevector) with shell ASOBJ to the user's
media upload endpoint with filename listed as FILENAME."
  (let*-values (((file-headers)
                 `((content-disposition . (form-data
                                           (name . "file")
                                           (filename . ,filename)))))
                ((body boundary) (format-multipart-body
                                  `(("object" . ,(asobj->string asobj))
                                    ,(make-part file-headers media))))
                ((headers)
                 `((content-type . (multipart/form-data (boundary . ,boundary))))))
    (call-with-values
        (lambda ()
          (apclient-post-local apclient (apclient-media-uri apclient)
                               body #:headers headers))
      response-with-body-maybe-as-asobj)))

(define (apclient-submit-file apclient asobj filepath)
  "Like apclient-submit-media, but pulling a file from on disk at FILEPATH."
  (let ((file (open-file filepath "r")))
    (call-with-values
        (lambda ()
          (apclient-submit-media apclient asobj
                                 file (basename filepath)))
      (lambda result
        ;; Clean up by closing the file
        (close file)
        ;; Return the values that would have been returned
        (apply values result)))))

(define* (http-post-asobj uri asobj #:key (headers '()))
  (call-with-values
      (lambda ()
        (http-post-async uri
                         #:body (asobj->string asobj)
                         #:headers
                         (cons as2-accept-header
                               (cons as2-content-type-header headers))))
    response-with-body-maybe-as-asobj))

;;; Not really an apclient-specific thing, but...
(define* (stream-member stream pred #:optional limit)
  "Search for member matching PRED in STREAM. Optionally limit to
LIMIT items (a non-negative integer).  Returns #f if no match or the
stream at point where stream-car of the stream matches PRED."
  (let lp ((stream stream)
           (limit limit))
    (cond ((and limit (= limit 0))    ; hit the traversal limit
           #f)
          ((stream-null? stream)      ; end of the stream
           #f)
          ((pred (stream-car stream)) ; yay, we got it
           stream)
          (else
           (lp (stream-cdr stream)
               (and limit (- limit 1)))))))

(define* (generate-reply-shell apclient asobj
                               #:key (max-depth 3)
                               (astype ^Object)
                               (extra-fields '()))
  "Returns a minimal object with addressing set based on the ASOBJ"
  ;; We only set to once, unlike cc.
  (define to (make-hash-table))
  (define cc (make-hash-table))
  (define in-reply-to
    (or (asobj-id asobj)
        (asobj-ref asobj '("object" "id"))))
  (define (add-actors-to! table data)
    (match data
      (#f #f)  ; do nothing
      ((? asobj?)
       (hash-set! table (asobj-id data) #t))
      ((? string?)
       (hash-set! table data #t))
      ((items ...)
       (for-each
        (lambda (item)
          (add-actors-to! table item))
        items))))
  (define (set-table->list set-table)
    (hash-map->list (lambda (key _) key) set-table))

  ;; First let's generate the to field
  (add-actors-to! to (asobj-ref asobj "actor"))
  (add-actors-to! to (asobj-ref asobj "attributedTo"))
  (add-actors-to! to (asobj-ref asobj '("object" "actor")))
  (add-actors-to! to (asobj-ref asobj '("object" "attributedTo")))

  (let cc-lp ((asobj asobj)
              (depth 0))
    (add-actors-to! cc (asobj-ref asobj "to"))
    (add-actors-to! cc (asobj-ref asobj "cc"))
    (add-actors-to! cc (asobj-ref asobj "actor"))
    (add-actors-to! cc (asobj-ref asobj "attributedTo"))
    (when (< depth max-depth)
      (for-each
       (lambda (field)
         (match (asobj-ref asobj field)
           (#f #f)  ; nothing to do
           ((? asobj? field-asobj)
            (cc-lp field-asobj (1+ depth)))
           ((items ...)
            (for-each
             (lambda (item)
               (match item
                 ((? asobj?)
                  (cc-lp item (1+ depth)))
                 (_ #f)))
             items))
           (_ #f)))  ; who knows what this is, but don't fail
       '("object" "target" "inReplyTo" "tag"))))

  ;; Filter out the apclient-id
  (hash-remove! to (uri->string (apclient-id apclient)))
  (hash-remove! cc (uri->string (apclient-id apclient)))

  ;; Filter everything out of cc that's in to
  (hash-for-each
   (lambda (to-item _)
     (hash-remove! cc to-item))
   to)

  ;; Finally build the object to return
  (apply make-as astype (%default-env)
         #:to (set-table->list to)
         #:cc (set-table->list cc)
         #:inReplyTo in-reply-to
         extra-fields))
