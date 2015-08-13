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

;;; Work in progress experiment to convert atom -> activitystreams 2.0


(use-modules (web client)
             (ice-9 receive)
             (ice-9 hash-table)
             (sxml simple)
             (sxml match)
             (web client)
             (ice-9 rdelim)
             (srfi srfi-1)
             (json))


(define (get-dustycloud-data)
  (xml->sxml
   (receive (response body)
       (http-get "http://dustycloud.org/blog/index.xml") body)
   #:namespaces '((atom . "http://www.w3.org/2005/Atom"))
   #:trim-whitespace? #t))


(define dustycloud-data
  (xml->sxml
   (call-with-input-file "dustycloud_less_feed.xml"
     (lambda (f)
       (read-string f)))
   #:namespaces '((atom . "http://www.w3.org/2005/Atom"))
   #:trim-whitespace? #t))


(define (feed->activitystream feed actor)
  ;; Things we don't know, we just return nil, so this is a lazy way
  ;; to clean things up for this circumstance.
  (define (clean-result result)
    (delete #nil result))

  ;; Process a single entry.
  ;; We go through all the pieces and use pattern matching to
  ;; build up the result.
  (define (process-entry . entry-pieces)
    (fold
     (lambda (piece activity)
       (let ((object-set! (lambda (key val)
                            (hash-set! (hash-ref activity "object")
                                       key val))))
         (sxml-match
          piece
          ;; Why not just use the atom:id as an activity id? ;)
          [(atom:id ,activity-id)
           (hash-set! activity "@id" activity-id)]
          ;; If html or xhtml, use title
          [(atom:title (@ (type "xhtml") . ,__) ,title . ,_)
           (object-set! "title" title)]
          [(atom:title (@ (type "html") . ,__) ,title . ,_)
           (object-set! "title" title)]
          ;; Default is type="text", or displayName if no type set
          [(atom:title ,title . ,_)
           (object-set! "displayName" title)]
          [(atom:link (@ (rel "alternate") (href ,href) . ,_))
           (object-set! "@id" href)]
          [(atom:updated ,when)
           (hash-set! activity "published" when)]
          [(atom:summary ,summary)
           (object-set! "summary" summary)]
          [(atom:content ,content)
           (object-set! "content" content)]
          [(atom:category (@ (term ,tagname) . ,rest))
           (object-set!
            "tag"
            ;; Add this tag to existing tags
            (cons (alist->hash-table
                   (list (cons "@type" "Object")
                         (cons "displayName" tagname)))
                  (hash-ref (hash-ref activity "object")
                            "tag" '())))]
          ;; TODO: expand this to support more author definition stuff
          [(atom:author (atom:name ,author-name))
           (object-set! "attributedTo"
                        (alist->hash-table
                         (list
                          '("@type" . "Person")
                          (cons "displayName" author-name))))]
          [,else
           (begin
             (format #t "TODO: ~a\n" else)
             #nil)]))
       activity)
     (alist->hash-table
      `(("@context" . "http://www.w3.org/ns/activitystreams")
        ("@type" . "Post")
        ("actor" . ,actor)
        ("object" .
         ,(alist->hash-table
           '(("@type" . "Article"))))))
     entry-pieces))

  (define (wrap-in-collection items)
    (alist->hash-table
     `(("@type" . "Collection")
       ("items" . ,items))))

  (wrap-in-collection
   (clean-result
    (let loop ((feed feed))
      (sxml-match
       feed
       ;; Process the top-level, loop over feed items
       ;; (Not sure why but I can't seem to use the catamorphism
       ;; feature here...)
       [(*TOP* ,_ ... (atom:feed ,feed-item ...))
        (map loop feed-item)]
       ;; Pass off parts a feed entry over to the entry processor
       [(atom:entry ,entry-part ...)
        (process-entry entry-part ...)]
       [,else #nil])))))

;; (fluid-set! %default-port-encoding "UTF-8")
;; (display (scm->json-string (feed->activitystream dustycloud-data "http://dustycloud.org/") #:pretty #t))

;; (map (lambda (x) (hash-map->list cons x)) (car (feed->activitystream dustycloud-data "derp")))
