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
  #:export (status-ref

            continue switching-protocols processing

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

;; code => description
(define status-codes
  (make-hash-table))

(define (status-ref code)
  "Get the description for HTTP status code CODE.  Return #f if not known."
  (hashv-ref status-codes code))

(define-syntax-rule (define-status sym code description)
  (begin
    (define sym code)
    (hashv-set! status-codes code description)))


;;; 1xx Informational
(define-status continue 100
  "Continue")
(define-status switching-protocols 101
  "Switching Protocols")
(define-status processing 102
  "Processing")


;;; 2xx Success
(define-status ok 200
  "OK")
(define-status created 201
  "Created")
(define-status accepted 202
  "Accepted")
(define-status non-authoritative-information 203
  "Non-Authoritative Information")
(define-status no-content 204
  "No Content")
(define-status reset-content 205
  "Reset Content")
(define-status partial-content 206                   ; RFC 7233
  "Partial Content")
(define-status multi-status 207                      ; WebDAV / RFC 4918
  "Multi-Status")
(define-status already-reported 208                  ; WebDAV / RFC 2842
  "Already Reported")
(define-status im-used 226                           ; RFC 3229
  "IM Used")


;;; 3xx Redirection
(define-status multiple-choices 300
  "Multiple Choices")
(define-status moved-permanently 301
  "Moved Permanently")
(define-status found 302
  "Found")
(define-status see-other 303
  "See Other")
(define-status not-modified 304                      ; RFC 7232
  "Not Modified")
(define-status use-proxy 305                         ; dangerous!
  "Use Proxy")
(define-status switch-proxy 306                      ; deprecated
  "Switch Proxy")
(define-status temporary-redirect 307
  "Temporary Redirect")
(define-status permanent-redirect 308                ; RFC 7538
  "Permanent Redirect")


;;; 4xx Client Error
(define-status bad-request 400
  "Bad Request")
(define-status unauthorized 401                      ; RFC 7235
  "Unauthorized")
(define-status payment-required 402
  "Payment Required")
(define-status forbidden 403
  "Forbidden")
(define-status not-found 404
  "Not Found")
(define-status method-not-allowed 405
  "Method Not Allowed")
(define-status not-acceptable 406
  "Not Acceptable")
(define-status proxy-auth-required 407               ; RFC 7235
  "Proxy Authentication Required")
(define-status request-timeout 408
  "Request Timeout")
(define-status conflict 409
  "Conflict")
(define-status gone 410
  "Gone")
(define-status length-required 411
  "Length Required")
(define-status precondition-failed 412               ; RFC 7232
  "Precondition Failed")
(define-status payload-too-large 412                 ; RFC 7231
  "Payload Too Large")
(define-status uri-too-long 414                      ; RFC 7231
  "URI Too Long")
(define-status unsupported-media-type 415
  "Unsupported Media Type")
(define-status range-not-satisfiable 416             ; RFC 7233
  "Range Not Satisfiable")
(define-status expectation-failed 417
  "Expectation Failed")
(define-status im-a-teapot 418                       ; RFC 2324
  "I'm A Teapot")
(define-status misdirected-request 421               ; RFC 7540
  "Misdirected Request")
(define-status unprocessable-entity 422              ; WebDAV / RFC 4918
  "Unprocessable Entity")
(define-status locked 423
  "Locked")
(define-status unprocessable-entity 422              ; WebDAV / RFC 4918
  "Unprocessable Entity")
(define-status failed-dependency 424                 ; WebDAV / RFC 4918
  "Failed Dependency")
(define-status upgrade-required 426
  "Upgrade Required")
(define-status precondition-failed 428               ; RFC 6585
  "Precondition Failed")
(define-status too-many-requests 429                 ; RFC 6585
  "Too Many Requests")
(define-status request-header-fields-too-large 431   ; RFC 6585
  "Request Header Fields Too Large")
(define-status unavailable-for-legal-reasons 451     ; RFC 7725
  "Unavailable For Legal Reasons")


;;; 5xx Server Error
(define-status internal-server-error 500
  "Internal Server Error")
(define-status not-implemented 501
  "Not Implemented")
(define-status bad-gateway 502
  "Bad Gateway")
(define-status service-unavailable 503
  "Service Unavailable")
(define-status gateway-timeout 504
  "Gateway Timeout")
(define-status http-version-not-supported 505
  "HTTP Version Not Supported")
(define-status variant-also-negotiates 506           ; RFC 2295
  "Variant Also Negotiates")
(define-status insufficient-storage 507              ; WebDAV / RFC 5842
  "Insufficient Storage")
(define-status loop-detected 508                     ; WebDAV / RFC 5842
  "Loop Detected")
(define-status not-extended 510                      ; RFC 2274
  "Not Extended")
(define-status network-authentication-required 511   ; RFC 6585
  "Network Authentication Required")

