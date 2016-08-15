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

(define-module (tests test-list-pagination)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils)
  #:use-module (pubstrate webapp list-pagination))

(test-begin "test-list-pagination")

(define alphabet
  '(a b c d e f g h i j k l m
      n o p q r s t u v w x y z))

;;; Testing list-paginate-first
(receive (page prev next)
    (list-paginate-first alphabet 5)
  (test-equal page '(a b c d e))
  (test-eq prev #f)
  (test-eq next 'f))

;;; A different how-many value
(receive (page prev next)
    (list-paginate-first alphabet 10)
  (test-equal page '(a b c d e f g h i j))
  (test-eq prev #f)
  (test-eq next 'k))

;;; Selecting the first element should also have no prev element
(receive (page prev next)
    (list-paginate alphabet 'a 5)
  (test-equal page '(a b c d e))
  (test-eq prev #f)
  (test-eq next 'f))

;;; A few items in.  Even though the 'a is less than 5 members previous,
;;; we should have it be the prev because a user wouldn't expect to
;;; totally lack a back button.
(receive (page prev next)
    (list-paginate alphabet 'c 5)
  (test-equal page '(c d e f g))
  (test-eq prev 'a)
  (test-eq next 'h))

;;; This one should have a prev, easily.
(receive (page prev next)
    (list-paginate alphabet 'j 5)
  (test-equal page '(j k l m n))
  (test-eq prev 'e)
  (test-eq next 'o))

;;; This one goes far enough that it should be shorter than
;;; the HOW-MANY and shouldn't return a next.
(receive (page prev next)
    (list-paginate alphabet 'x 5)
  (test-equal page '(x y z))
  (test-eq prev 's)
  (test-eq next #f))

;;; This one doesn't exist in the alphabet, so we should get
;;; all #f's back.
(receive (page prev next)
    (list-paginate alphabet 'not-a-letter 5)
  (test-equal page #f)
  (test-eq prev #f)
  (test-eq next #f))

(test-end "test-list-pagination")

(test-exit)

