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

(define-module (pubstrate webapp fat-lean)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (web uri)
  #:use-module (sjson utils)
  #:use-module (pubstrate generics)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate webapp ctx)
  #:use-module (pubstrate webapp db)
  #:export (make-retriever
            asobj-fatten fatten-asobjs

            asobj-leanify))

(define-record-type <retriever>
  (%make-retriever max-depth cache)
  retriever?
  (max-depth retriever-max-depth)
  (cache retriever-cache)
  ;; TODO
  ;; (fetch-external? retriever-fetch-external?)
  )

(define* (make-retriever #:optional (max-depth 5) (cache (make-hash-table)))
  (%make-retriever max-depth cache))


(define (decrement-retriever retriever)
  "Make a clone of the retriever, but decrement the max-depth by one"
  (make-retriever (1- (retriever-max-depth retriever))
                  (retriever-cache retriever)))

(define (retriever-retrieve retriever id)
  (define (recur-retrieve asobj)
    (asobj-fatten asobj (decrement-retriever retriever)))

  (cond
   ;; We've reached the max depth, so stop retrieving
   ((zero? (retriever-max-depth retriever))
    ;; return the id as-is
    id)
   ;; If we have a memoized copy of this in the db
   ;; retrieve that
   ((hash-ref (retriever-cache retriever) id) =>
    identity)
   ;; Otherwise else, look up what's in the db
   (else
    ;; TODO: Also support when there's nothing in the db
    ;;   by looking things up
    (let ((result (or (and=> (db-asobj-ref (ctx-ref 'db) id)
                             recur-retrieve)
                      ;; if we don't have anything in the db,
                      ;; just memoize and return the id.  This is stupid,
                      ;; but it works for now.
                      id)))
      (hash-set! (retriever-cache retriever) id result)
      result))))

(define (retriever-fatten-field retriever asobj field)
  (define (uri? obj)
    (and (string? obj)
         (string->uri obj)))
  (let ((cur-val (asobj-ref asobj field)))
    (cond
      ((uri? cur-val)
       (asobj-set asobj field
                  (retriever-retrieve retriever cur-val)))
      ((json-array? cur-val)
       (asobj-set asobj field
                  (map (lambda (item)
                         (if (uri? item)
                             (retriever-retrieve retriever item)
                             item))
                       cur-val)))

      ;; anything else, return asobj as-is
      (else asobj))))

(define (retriever-fatten-fields retriever asobj fields)
  (fold (lambda (field asobj)
          (retriever-fatten-field retriever asobj field))
        asobj
        fields))

(define-syntax-rule (a-list (key val) ...)
  (list (cons key val) ...))

(define astype-db-uri-fields
  (alist->hashq-table
   (a-list (^Object '("attachment" "attributedTo" "audience"
                      "context" "generator" "icon" "image"
                      "inReplyTo" "location" "preview"
                      "replies" "tag" "to" "bto" "cc" "bcc"
                      ;; @@: This technically isn't part of the Object
                      ;;   per as2 spec!  But it is how we identify who posted
                      ;;   the object in AP...
                      ;;   While attributedTo exists, it's not quite the
                      ;;   same as "who posted this object"... you'd want to be able
                      ;;   to properly credit someone else, but it feels like it's
                      ;;   easier to claim someone else made something (as opposed to,
                      ;;   who posted it?) but it's also possible to lie...
                      ;;   (... verifiable claims will fix this? ;p)
                      ;; @@: Okay yeah we should use attributedTo...
                      "actor"))
           (^Activity '("actor" "object" "target" "result"
                        "origin" "instrument"))
           (^Collection '("current"
                          ;; @@: Probably we ought to expand out "current"
                          ;;   but leave "first" and "last" as uris?
                          "first" ;; "last"
                          "items"))
           ;; Why not "partOf" "next" "prev"?  Kind of a mess to embed those...
           (^CollectionPage '())
           (^OrderedCollectionPage '("startIndex"))
           (^Question '("oneOf" "anyOf" "closed"))
           (^Relationship '("subject" "object" "relationship")))))

(define* (asobj-fatten asobj #:optional (retriever (make-retriever 5)))
  (fold
   (lambda (astype asobj)
     (let ((fields (hashq-ref astype-db-uri-fields
                              astype
                              '())))
       (retriever-fatten-fields retriever asobj fields)))
   asobj
   (asobj-inherits asobj))) 

(define* (fatten-asobjs asobjs #:optional (retriever (make-retriever 5)))
  ;; We use the same retriever to keep around the cache
  (let ((retriever (make-retriever)))
    (map (lambda (asobj)
           (asobj-fatten asobj retriever))
         asobjs)))




;; TODO: It might not be the right structure to use the retriever
;;   since we're not doing exactly the same thing with a recursion limit?
;;   Should we instead have some sort of setter?
(define* (asobj-leanify asobj #:optional (setter 'TODO))
  'TODO)
