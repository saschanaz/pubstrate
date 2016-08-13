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


;;; Pagination on lists
;;; ===================

(define-module (pubstrate webapp list-pagination)
  #:use-module (ice-9 q)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:export (list-paginate list-paginate-first))

(define* (list-paginate lst member how-many #:key (is-equal? equal?))
  "Search for MEMBER in LST with a page of HOW-MANY items, as well as
returning information on previous and next pages.

To search for the first page, pass in any object as MEMBER, and
use (const #t) for IS-EQUAL?.  Or, just use the list-paginate-first
procedure.

Returns three values to its continuation: a list of items (or #f if
not found) in the range of MEMBER and HOW-MANY, as well as the key for
the previous page (or #f), and the key for the next page (or #f).

Note that the usefulness of the \"previous\" and \"next\" return
values is dependent on there being no duplicate items in the list.
After all, if you search for member 'foo and your \"next\" value is
also 'foo, don't expect to advance forward anywhere. :)
"
  (define (find-member)
    ;; Returns 2 values to its continuation: the cons cell with KEY as
    ;; its car and all later elements as its cdr
    (define history (make-q))
    (let keep-looking ((lst lst)
                       (history-len 0))
      (cond
       ;; If it's empty, well, we didn't find anything.
       ((null? lst)
        (values #f #f))
       ;; Looks like we found it!
       ((is-equal? (car lst) member)
        (values
         lst
         ;; Only return
         (if (eq? history-len how-many)
             (deq! history)
             #f)))
       (else
        (enq! history (car lst))
        ;; If the history's already full, remove
        ;; the last item
        (if (eq? history-len how-many)
            (deq! history))
        (keep-looking (cdr lst)
                      (if (< history-len how-many)
                          (+ history-len 1)
                          ;; otherwise we're full
                          history-len))))))

  (define (get-page-and-next member-lst)
    ;; Returns 2 values to its continuation: the subset of the list
    ;; of size how-many (or however much is left) and whatever comes
    ;; right after (if there is anything)
    (let lp ((countdown (- how-many 1))
             (remaining member-lst)
             (prev '()))
      (cond
       ;; Well there aren't any more, so return what we've got
       ((null? remaining)
        (values (reverse prev) #f))
       ;; Okay, that's the limit of what we're looking for, so
       ;; return it as well as whatever's immediately next
       ((eqv? countdown 0)
        (let ((final-lst (cons (car remaining)
                               prev))
              (whats-next
               (match remaining
                 ((_ next _ ...)
                  next)
                 (_ #f))))
          (values (reverse final-lst)
                  whats-next)))
       ;; Keep looking...
       (else
        (lp (- countdown 1)
            (cdr remaining)
            (cons (car remaining) prev))))))

  (receive (member-lst prev)
      (find-member)
    (if member-lst
        ;; horray, get the whole page and whatever's next
        (receive (page next)
            (get-page-and-next member-lst)
          (values page prev next))
        ;; well we never found it, so... return a whole lot of nothin'
        (values #f #f #f))))


(define (list-paginate-first lst how-many)
  "Like list-paginate, but returns the first page."
  (list-paginate lst #f how-many #:is-equal? (const #t)))
