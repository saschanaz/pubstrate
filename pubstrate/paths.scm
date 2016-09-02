;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright (C) 2015-2016 Christopher Allan Webber <cwebber@dustycloud.org>
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

;;; Path manipulation utilties

(define-module (pubstrate paths)
  #:export (path-join))

(define (path-join base-path . paths)
  "Join together BASE-PATH and all remaining PATHS with filename separator.

Note that if there's a forward slash at the end of the final path, this
procedure will not preserve it."
  (let* ((separator-char (string-ref file-name-separator-string 0))
         (stripped-paths
          (map (lambda (path)
                 (string-trim-both path separator-char))
               paths))
         (stripped-base-path (string-trim-right base-path separator-char)))
    (string-join (append (list stripped-base-path)
                         stripped-paths)
     file-name-separator-string)))
