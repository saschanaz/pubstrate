;; Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

(define-module (activitystuff activities)
  #:use-module (activitystuff json-utils)
  #:use-module (activitystuff contrib json)
  #:use-module (srfi srfi-9)
  #:export (activity?
            activity-implicit-contexts
            activity-get-sjson activity-get-vjson
            activity-get-expanded))


;; Also todo :)
(define activity-context 'todo)
(define default-implicit-contexts (list activity-context))


(define-record-type <activity>
  (make-activity-internal implicit-contexts
                          get-sjson get-vjson
                          get-type get-expanded)
  activity?
  (implicit-contexts activity-implicit-contexts)
  ;; none of the rest of these are exposed to users, they're
  ;; all promises
  (get-type activity-get-type)
  (get-sjson activity-get-sjson)
  (get-vjson activity-get-vjson)
  (get-expanded activity-get-expanded))

(define (activity-type activity)
  (force (activity-get-type activity)))

(define (activity-sjson activity)
  (force (activity-get-sjson activity)))

(define (activity-vjson activity)
  (force (activity-get-vjson activity)))

(define (activity-expanded activity)
  (force (activity-get-expanded activity)))

(define* (activity-pretty-print activity port
                                #:key (indent default-pprint-indent))
  (pprint-json (activity-sjson activity) port #:indent indent))

(define (common-make-activity implicit-contexts
                              get-sjson get-vjson)
  (let ((get-expanded (delay 'todo)))
    (define (simple-get-type)
      ;; @@: Should we use (get-vjson) instead?
      ;;   Ah well it only happens once
      (assoc-ref (force get-sjson) "@type"))
    (define get-type
      ;; TODO: also check get-expanded if simple-get-type is not enough
      (delay (simple-get-type)))
    (make-activity-internal implicit-contexts
                            get-sjson get-vjson
                            get-type get-expanded)))

(define* (vjson->activity vjson
                         #:key
                         (implicit-contexts default-implicit-contexts))
  (let ((get-vjson (delay vjson))
        (get-sjson (delay (sjson->vjson vjson))))
    (common-make-activity implicit-contexts get-vjson get-sjson)))

(define* (sjson->activity sjson
                         #:key
                         (implicit-contexts default-implicit-contexts))
  (let ((get-sjson (delay sjson))
        (get-vjson (delay (vjson->sjson sjson))))
    (common-make-activity implicit-contexts get-vjson get-sjson)))


(define (activity-valid? activity)
  ;; maybe check that @type is valid, and stuff
  'todo)

