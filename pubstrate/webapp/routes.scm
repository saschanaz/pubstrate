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

(define-module (pubstrate webapp routes)
  #:use-module (ice-9 match)
  #:use-module (pubstrate webapp views)
  #:use-module (web request)
  #:use-module (web uri)
  #:export (route))

(define (route request)
  (match (split-and-decode-uri-path (uri-path (request-uri request)))
    (() (values index '()))
    (("u" username)
     (values user-page (list username)))
    (("u" username "p" post-id)
     (values display-post (list username post-id)))
    (("u" username "inbox")
     (values user-inbox (list username)))
    (("u" username "outbox")
     (values user-outbox (list username)))
    (("u" username "followers")
     (values user-followers (list username)))
    (("u" username "following")
     (values user-following (list username)))
    (("u" username "liked")
     (values user-liked (list username)))
    (("static" static-path ...)
     ;; TODO: make this toggle'able
     (values render-static (list static-path)))

    (("media" media-path ...)
     ;; TODO: make this toggle'able
     (values render-media (list media-path)))

    (("login")
     (values login '()))
    (("logout")
     (values logout '()))
    (("mockup")
     (values mockup '()))
    (("api" "get-auth-token")
     (values oauth-authorize '()))

    (("api" "upload-media")
     (values upload-media '()))

    ;; Not found!
    (_ (values standard-four-oh-four '()))))
