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
;;;
;;; Note that these assume that the file name separator is a single character,
;;; but I've yet to run an OS where this isn't true, so...
;;; Also, this is probably not portable to MS Windows.  Sorry!
;;; Patches welcome.

(define-module (pubstrate paths)
  #:export (path-join clean-path mkdir-recursive))

(define file-name-separator-char
  (string-ref file-name-separator-string 0))

(define (path-join base-path . paths)
  "Join together BASE-PATH and all remaining PATHS with filename separator.

Note that if there's a forward slash at the end of the final path, this
procedure will not preserve it."
  (let* ((separator-char file-name-separator-char)
         (stripped-paths
          (map (lambda (path)
                 (string-trim-both path separator-char))
               paths))
         (stripped-base-path (string-trim-right base-path separator-char)))
    (string-join (append (list stripped-base-path)
                         stripped-paths)
     file-name-separator-string)))

(define (clean-path path)
  "Remove any nasty .. stuff from the path"
  (string-join (delete ".." (string-split path file-name-separator-char))
               file-name-separator-string))

(define* (mkdir-recursive path #:optional mode)
  "Like `mkdir', but recursively make PATH"
  ;; make use of mode, if provided
  (define (make-dir path)
    (if mode
        (mkdir path mode)
        (mkdir path)))
  
  (define dirs-to-make
    (let lp ((path-remaining (string-trim-right path file-name-separator-char))
             (paths '()))
      ;; TODO: Check to see if it's a directory or file
      (if (file-exists? path-remaining)
          paths
          (let ((next-sep-idx (string-index-right path-remaining
                                                  file-name-separator-char)))
            (if (= next-sep-idx 0)
                ;; Well we aren't making the root of things
                paths
                (lp (substring path-remaining 0 next-sep-idx)
                    (cons path-remaining paths)))))))

  (for-each make-dir dirs-to-make))
