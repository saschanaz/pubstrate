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

(define-module (pubstrate webapp template-utils)
  #:export (maybe-render maybe-render-inline
            render-if render-inline-if))

(define (maybe-render exp)
  (if exp (list exp) '()))

(define (maybe-render-inline exp)
  (if exp exp '()))

(define-syntax-rule (render-if test exp)
  (if test
      (list exp) '()))

(define-syntax-rule (render-inline-if test exp)
  (if test
      exp '()))

