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

(define-module (pubstrate webapp http-status)
  #:export (continue switching-protocols processing

            ok created accepted non-authoritative-information
            no-content reset-content partial-content multi-status
            already-reported im-used

            multiple-choices moved-permanently found see-other
            not-modified use-proxy switch-proxy temporary-redirect
            permanent-redirect

            bad-request unauthorized payment-required forbidden
            not-found method-not-allowed not-acceptable
            proxy-auth-required request-timeout conflict gone
            length-required precondition-failed payload-too-large
            uri-too-long unsupported-media-type range-not-satisfiable
            expectation-failed im-a-teapot misdirected-request
            unprocessable-entity locked unprocessable-entity locked
            failed-dependency upgrade-required precondition-failed
            too-many-requests request-header-fields-too-large
            unavailable-for-legal-reasons

            internal-server-error not-implemented bad-gateway
            service-unavailable gateway-timeout http-version-not-supported
            variant-also-negotiates insufficient-storage loop-detected
            not-extended network-authentication-required))

;;; HTTP Status Codes.  For all your HTTP Status Code needs!
;;; https://en.wikipedia.org/wiki/List_of_HTTP_status_codes


;;; 1xx Informational
(define continue 100)
(define switching-protocols 101)
(define processing 102)


;;; 2xx Success
(define ok 200)
(define created 201)
(define accepted 202)
(define non-authoritative-information 203)
(define no-content 204)
(define reset-content 205)
(define partial-content 206)                  ; RFC 7233
(define multi-status 207)                     ; WebDAV / RFC 4918
(define already-reported 208)                 ; WebDAV / RFC 2842
(define im-used 226)                          ; RFC 3229


;;; 3xx Redirection
(define multiple-choices 300)
(define moved-permanently 301)
(define found 302)
(define see-other 303)
(define not-modified 304)                     ; RFC 7232
(define use-proxy 305)                        ; dangerous!
(define switch-proxy 306)                     ; deprecated
(define temporary-redirect 307)
(define permanent-redirect 308)               ; RFC 7538


;;; 4xx Client Error
(define bad-request 400)
(define unauthorized 401)                     ; RFC 7235
(define payment-required 402)
(define forbidden 403)
(define not-found 404)
(define method-not-allowed 405)
(define not-acceptable 406)
(define proxy-auth-required 407)              ; RFC 7235
(define request-timeout 408)
(define conflict 409)
(define gone 410)
(define length-required 411)
(define precondition-failed 412)              ; RFC 7232
(define payload-too-large 412)                ; RFC 7231
(define uri-too-long 414)                     ; RFC 7231
(define unsupported-media-type 415)
(define range-not-satisfiable 416)            ; RFC 7233
(define expectation-failed 417)
(define im-a-teapot 418)                      ; RFC 2324
(define misdirected-request 421)              ; RFC 7540
(define unprocessable-entity 422)             ; WebDAV / RFC 4918
(define locked 423)             
(define unprocessable-entity 422)             ; WebDAV / RFC 4918
(define locked 423)                           ; WebDAV / RFC 4918
(define failed-dependency 424)                ; WebDAV / RFC 4918
(define upgrade-required 426)
(define precondition-failed 428)              ; RFC 6585
(define too-many-requests 429)                ; RFC 6585
(define request-header-fields-too-large 431)  ; RFC 6585
(define unavailable-for-legal-reasons 451)    ; RFC 7725


;;; 5xx Server Error
(define internal-server-error 500)
(define not-implemented 501)
(define bad-gateway 502)
(define service-unavailable 503)
(define gateway-timeout 504)
(define http-version-not-supported 505)
(define variant-also-negotiates 506)          ; RFC 2295
(define insufficient-storage 507)             ; WebDAV / RFC 5842
(define loop-detected 508)                    ; WebDAV / RFC 5842
(define not-extended 510)                     ; RFC 2274
(define network-authentication-required 511)  ; RFC 6585

