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

;;; Demo used to prove that propagation of replies in ActivityPub spec
;;; works correctly.

(use-modules (pubstrate shorthand)
             (pubstrate vocab)
             (pubstrate asobj)
             (pubstrate webapp auth)
             (ice-9 match)
             (ice-9 format))

(define cwebber
  (person #:id "http://dustycloud.org/"
          #:name "Chris Webber"))

(define tantek
  (person #:id "http://tantek.org/"
          #:name "Tantek"))

(define amy
  (asobj-set-private*
   (person #:id "http://amy.gy/"
           #:name "Amy Guy")
   #:followers
   (cons "http://amy.gy/followers/"
         (list tantek))))

(define participants
  (map
   (lambda (p)
     (cons (asobj-id p) p))
   (list cwebber tantek amy)))

(define dbs
  (map
   (lambda (p)
     (cons (asobj-id p) (make-hash-table)))
   (list cwebber tantek amy)))

(define (db-for-actor actor)
  (assoc-ref dbs (asobj-id actor)))

(define (db-see! actor asobj)
  (hash-set! (db-for-actor actor)
             (asobj-id asobj) asobj))

(define (db-seen? actor asobj)
  (hash-ref (db-for-actor actor)
            (asobj-id asobj)))

(define (official-actor actor)
  (assoc-ref participants (asobj-id actor)))

(define (actor-followers-uri actor)
  (car (asobj-private-ref actor "followers" (cons #f #f))))

(define (wrap-in-create asobj)
  (let ((random-id (gen-bearer-token)))
    (create #:object asobj
            #:id random-id
            #:to (asobj-ref asobj "to")
            #:actor (asobj-ref asobj "actor"))))


(define initial-note
  (wrap-in-create
   (note #:id "http://amy.gy/cool-note/"
         #:actor amy
         #:to (list cwebber
                    (car
                     (asobj-private-ref
                      amy
                      "followers"))))))

(define (cwebbers-client actor incoming)
  (when (equal? (asobj-ref incoming '("actor" "id"))
                (asobj-id amy))
    ;; First, comment publicly
    (post-to-outbox
     actor
     (wrap-in-create
      (asobj-cons
       (note #:actor actor
             #:content "cool!"
             #:inReplyTo incoming
             #:id "http://dustycloud.org/cool/")
       "to"
       (cons amy
             (asobj-ref incoming "to")))))
    ;; Reply privately
    (post-to-outbox
     actor
     (wrap-in-create
      (note #:actor actor
            #:content "Psst, you have a typo"
            #:to (list amy)
            #:inReplyTo incoming
            #:id "http://dustycloud.org/have-a-typo/")))))

(define (amys-client actor incoming)
  ;; Does nothing
  #f)

(define (tanteks-client actor incoming)
  ;; complains publicly
  (post-to-outbox
   actor
   (wrap-in-create
    (asobj-cons
     (note #:actor actor
           #:content "RDF sucks"
           #:inReplyTo incoming
           #:id "http://tantek.org/rdf-sucks/")
     "to" (asobj-ref incoming "to")))))

(define clients
  `((,cwebber . ,cwebbers-client)
    (,amy . ,amys-client)
    (,tantek . ,tanteks-client)))

(define (maybe-send-to-followers actor outgoing to-whom)
  (let* ((followers (asobj-private-ref actor "followers" '(#f #f)))
         (followers-uri (car followers))
         (followers-list (cdr followers)))
    (if (equal? to-whom followers-uri)
        (for-each
         (lambda (follower)
           (inbox-receive follower outgoing))
         (list->asobj followers-list))
        ;; (format #t "~a doesn't know who ~s is! Ignoring.\n"
        ;;         (asobj-ref actor "name")
        ;;         to-whom)
        )))

(define (list->asobj lst)
  (map (lambda (obj)
         (match obj
           ((? string? obj)
            obj)
           ((? asobj? obj)
            obj)
           (else
            (make-asobj obj
                         (%default-env)))))
       lst))

(define (post-to-outbox actor outgoing)
  (format #t "*** -> OUTBOX of ~a ***\n"
          (asobj-ref actor "name"))
  (asobj-pprint outgoing)
  (let ((actor (official-actor actor)))
    (db-see! actor outgoing)
    (for-each
     (lambda (to-whom)
       (match to-whom
         ;; don't send this to yourself
         ((? (lambda (send-to)
               (and (asobj? send-to)
                    (equal? (asobj-id send-to)
                            (asobj-id actor)))) _)
          #f)
         ((? asobj? _)
          (inbox-receive to-whom outgoing))
         ((? string? _)
          (maybe-send-to-followers actor outgoing to-whom))))
     (list->asobj (asobj-ref outgoing "to")))))

(define (in-reply-to-mine? actor asobj)
  (let ((in-reply-to (asobj-ref asobj '("object" "inReplyTo"))))
    (and in-reply-to
         (equal? (asobj-id actor)
                 (asobj-ref in-reply-to '("actor" "id"))))))

(define (inbox-receive actor incoming)
  (format #t "*** -> INBOX of ~a ***\n"
          (asobj-ref actor "name"))
  (asobj-pprint incoming)
  (let ((actor (official-actor actor)))
    (format #t "~a got from ~a: ~s :: ~s\n"
            (asobj-ref actor "name")
            (asobj-ref incoming '("actor" "name"))
            (asobj-ref incoming '("object" "id"))
            (asobj-ref incoming '("object" "content")))
    (let ((client (assoc-ref clients actor)))
      (client actor incoming))
    (when (not (db-seen? actor incoming))
      (db-see! actor incoming)
      (when (in-reply-to-mine? actor incoming)
        (for-each
         (lambda (recipient)
           (if (string? recipient)
               (maybe-send-to-followers actor incoming recipient)))
         (list->asobj (asobj-ref incoming "to")))))))
