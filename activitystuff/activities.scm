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
  #:export (default-implicit-contexts

            ;; creation functions
            sjson->activity
            vjson->activity
            string->activity

            activity?

            activity-implicit-contexts
            activity-sjson activity-vjson
            activity-type activity-expanded

            activity-valid?

            activity-pprint
            activity-pprint-to-string
            pp-activity))


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
  (get-sjson activity-get-sjson)
  (get-vjson activity-get-vjson)
  (get-type activity-get-type activity-get-json-set!)
  (get-expanded activity-get-expanded))

(define (activity-type activity)
  "Get the ACTIVITY's @type"
  (force (activity-get-type activity)))

(define (activity-sjson activity)
  "Get the sjson representation of ACTIVITY"
  (force (activity-get-sjson activity)))

(define (activity-vjson activity)
  "Get the vjson representation of ACTIVITY"
  (force (activity-get-vjson activity)))

(define (activity-expanded activity)
  "Get the json-ld expanded version of ACTIVITY"
  (force (activity-get-expanded activity)))

(define* (activity-pprint activity port
                          #:key (indent default-pprint-indent))
  "Pretty print ACTIVITY to PORT

Optionally takes an INDENT level as a key"
  (pprint-json (activity-sjson activity) port #:indent indent))

(define* (activity-pprint-to-string activity
                                    #:key (indent default-pprint-indent))
  "Pretty print ACTIVITY to string"
  (call-with-output-string
   (lambda (p) (activity-pprint activity p #:indent indent))))

(define* (pp-activity activity #:optional (indent default-pprint-indent))
  "Pretty print and display activity"
  (activity-pprint activity (current-output-port) #:indent indent)
  (newline))

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
  "Take a VJSON object, convert to an <activity>"
  (let ((get-vjson (delay vjson))
        (get-sjson (delay (vjson->sjson vjson))))
    (common-make-activity implicit-contexts get-vjson get-sjson)))

(define* (sjson->activity sjson
                          #:key
                          (implicit-contexts default-implicit-contexts))
  "Take an SJSON object, convert to an <activity>"
  (let ((get-sjson (delay sjson))
        (get-vjson (delay (sjson->vjson sjson))))
    (common-make-activity implicit-contexts get-sjson get-vjson)))

(define* (string->activity string)
  "Take a STRING, convert to an <activity>"
  (sjson->activity (read-json-from-string string)))

(define (activity-valid? activity)
  ;; maybe check that @type is valid, and stuff
  'todo)

