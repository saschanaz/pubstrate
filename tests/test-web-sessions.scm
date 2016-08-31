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

(define-module (tests test-web-sessions)
  #:use-module (tests utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (pubstrate crypto)
  #:use-module (pubstrate contrib base64)
  #:use-module (pubstrate webapp sessions))

(test-begin "test-web-sessions")

(define-syntax-rule (import-from-sessions name)
  (define name
    (@@ (pubstrate webapp sessions) name)))

;; Pull in some non-exported procedures
(import-from-sessions session-manager-future-expires)
(import-from-sessions still-fresh-by-date-string?)
(import-from-sessions still-fresh-by-date?)
(import-from-sessions split-session-string)

;; Fix the current time for easier testing
(import-from-sessions %current-time)
(define a-time
  (make-time 'time-utc 0 1472514613))  ; "2016-08-29T23:50:13"

(define-syntax-rule (at-fixed-time body1 body2 ...)
  (parameterize ((%current-time (const a-time)))
    body1 body2 ...))

;; This time shouldn't be expired yet
(at-fixed-time
 (test-assert (still-fresh-by-date-string?
               "2016-08-31T14:01:59.977681000-05:00"))  ; the expires-by time
 ;; Neither should the time it currently believes it is
 (test-assert (still-fresh-by-date?
               (session-manager-future-expires
                (make-session-manager (gen-signing-key))))))

;; This should be though
(at-fixed-time
 (test-assert (not (still-fresh-by-date-string?
                    "2016-02-28T14:01:59Z")))) ; that was like, yesterday, man!

;; An invalid http date string will be considered not-fresh
(at-fixed-time
 (test-assert (not (still-fresh-by-date-string? "I'm a date, honest!!!!"))))

;; A date explicitly in 2 days, 1 hour, 30 minutes
(at-fixed-time
 (test-equal (make-date 0 13 20 01 1 9 2016 0)
   (session-manager-future-expires
    (make-session-manager (gen-signing-key)
                          #:expire-delta '(2 1 30)))))


;;; Sessions tests
;;; ==============

(define our-key
  #vu8(252 37 107 2 66 0 168 137 9 168 198 225 153 220 231 85 106 204 78 114 40
       195 23 20 132 120 31 182 47 63 209 50 175 17 120 123 14 17 171 236 130
       151 32 175 89 171 179 83 185 65 149 0 21 77 49 177 7 118 172 63 174 230
       158 236 161 111 191 186 104 196 168 123 252 156 189 166 244 37 132 178
       215 78 18 86 93 218 122 7 107 211 57 147 62 207 46 98 130 18 36 205 89 92
       17 36 62 246 38 152 250 66 220 242 129 174 190 167 81 33 201 242 232 122
       118 81 176 2 238 99))

(define test-session-manager
  (make-session-manager our-key
                        #:algorithm 'sha512
                        #:expire-delta '(30 0 0)))

(define (set-cookie->session-str set-cookie-header)
  (match set-cookie-header
    (('set-cookie "session" (? string? session-str) (("Expires" . (? date? _))))
     session-str)))

(define signed-cookie-str
  (at-fixed-time
   (set-cookie->session-str (set-session test-session-manager '(its fine)))))

(match (split-session-string signed-cookie-str)
  ((sig date-str b64-data)
   ;; Maybe not the most useful test but it would be strange if this changed
   (test-equal sig
     "VvcskyTMO4LWMOwhrxNKgLd8EB/F/nwosQ5XwfceYEsNra1VmWndEf9RtP7TR7yeOiGZhRJaYFB+/u1POtiwQw==")
   ;; These are more useful
   (test-equal date-str "2016-09-28T23:50:13Z")
   (test-equal (utf8->string (base64-decode b64-data))
     "(its fine)")))

;;; Now let's make a new session based on this data
(at-fixed-time
 (let ((fake-request
        (build-request (string->uri "https://example.who/cares/")
                       #:headers `((cookie ("session" . ,signed-cookie-str))))))
   (test-equal
       (session-data test-session-manager fake-request)
     '(its fine))))
  
(define cookie-str-with-bad-sig
  "c6SD1S6It8HRYIXLYzRvLln0/yOWrIhy+XV86m42eSvKhv8U8NqZNqAny35qDd1QObZrwXhq1jjVNUiAwU1I0w==$2016-09-28T23:50:13Z$KGl0cyBmaW5lKQ==")

;;; Now let's do a cookie signature that's not legitimate
(at-fixed-time
 (let ((fake-request
        (build-request (string->uri "https://example.who/cares/")
                       #:headers `((cookie ("session" .
                                            ,cookie-str-with-bad-sig))))))
   (test-equal
       (session-data test-session-manager fake-request)
     #f)))

(define expired-time
  (make-time 'time-utc 0 1443484213))  ; "2015-09-28T23:50:13Z" ... a year prior

;;; Here's one that's legitimate, but it's expired by now
(define expired-cookie-str
  (parameterize ((%current-time (const expired-time)))
    (set-cookie->session-str
     (set-session test-session-manager '(its fine)))))

;;; This is expired, so it should be invalid.
(at-fixed-time
 (let ((fake-request
        (build-request (string->uri "https://example.who/cares/")
                       #:headers `((cookie ("session" .
                                            ,expired-cookie-str))))))
   (test-equal
       (session-data test-session-manager fake-request)
     #f)))

;;; And now, deleting cookies!  It really ought to create a cookie
;;; with an empty value, set to Expire at the epoch.
(test-equal (delete-session test-session-manager)
  `(set-cookie "session" ""
               (("Expires" . ,(@@ (pubstrate webapp cookie) %the-epoch)))))

(test-end "test-web-sessions")
(test-exit)

