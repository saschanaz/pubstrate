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

(define-module (pubstrate webapp views)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate json-utils)
  #:use-module (pubstrate webapp ctx)
  #:export (collect-recipients
            federate-asobj))


;;; Federation delivery tooling


;; TODO: This needs *way* more async support
(define (collect-recipients asobj)
  "Collect a list of all actors to deliver to

Note that this has potential side effects; it fetches objects from
the store, but also if it does not yet have references to these objects,
it will fetch them remotely from the web.  It may even stick them
in the store!"
  (define (append-actor-or-collection id lst)
    ;; TODO: Dereference collections...!
    (cons id lst))
  (define (asobj-key-as-list asobj key)
    (let ((result (asobj-ref asobj key '())))
      (if (sjson-array? result)
          result
          (list result))))
  (define (field-collector key)
    (lambda (asobj collected)
      (values asobj
              (fold (lambda (item prev)
                      (append-actor-or-collection item prev))
                    collected
                    (asobj-key-as-list asobj key)))))
  (define collect-to (field-collector "to"))
  (define collect-cc (field-collector "cc"))
  (define collect-bcc (field-collector "bcc"))
  (define collect-em-all
    (compose collect-to collect-cc collect-bcc))

  (collect-em-all asobj '()))

(define (asobj-list-inboxes asobj-lst)
  "Extract a list of inboxes from a list of activitystreams actors."
  )


(define (federate-asobj asobj)
  "Send activitystreams object to recipients."
  'TODO)

