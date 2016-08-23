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

(define-module (pubstrate date-rfc3339)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:use-module (rx irregex)
  #:export (date->rfc3339-string rfc3339-string->date
            date->http-date-string http-date-string->date))

;;;
;;; A not particularly fast but nice looking implementation of RFC 3339
;;; using irregex.
;;; My 10 year old laptop can parse about 6k dates / second.
;;; Maybe a lot faster in guile 2.2? :)
;;;

(define* (digit=> to-var how-many)
  `(=> ,to-var (= ,how-many numeric)))

;;    date-fullyear   = 4DIGIT
;;    date-month      = 2DIGIT  ; 01-12
;;    date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on
;;                              ; month/year
;;    time-hour       = 2DIGIT  ; 00-23
;;    time-minute     = 2DIGIT  ; 00-59
;;    time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second
;;                              ; rules
;;    time-secfrac    = "." 1*DIGIT
;;    time-numoffset  = ("+" / "-") time-hour ":" time-minute
;;    time-offset     = "Z" / time-numoffset
;; 
;;    partial-time    = time-hour ":" time-minute ":" time-second
;;                      [time-secfrac]
;;    full-date       = date-fullyear "-" date-month "-" date-mday
;;    full-time       = partial-time time-offset
;; 
;;    date-time       = full-date "T" full-time

(define hour-sre
  (digit=> 'hour 2))
(define minute-sre
  (digit=> 'minute 2))
(define second-sre
  (digit=> 'second 2))
(define secfrac-sre
  '(: "." (=> secfrac (+ numeric))))

(define time-numoffset
  `(: (=> offset-plusminus (or "+" "-"))
      ,(digit=> 'offset-hour 2) ":"
      ,(digit=> 'offset-minute 2)))

(define time-offset
  `(or "Z"
       ,time-numoffset))

(define date-fullyear-sre
  (digit=> 'fullyear 4))
(define date-month-sre
  (digit=> 'month 2))
(define date-mday-sre
  (digit=> 'mday 2))

(define full-date-sre
  `(: ,date-fullyear-sre "-" ,date-month-sre "-" ,date-mday-sre))


;;; AS2 specific version of RFC3339
;;; This permits that seconds MAY be omitted
;; as2-partial-time = time-hour ":" time-minute [":" time-second]
;;                    [time-secfrac]
;; as2-full-time    = as2-partial-time time-offset
;; as2-date-time    = full-date "T" as2-full-time
(define partial-time-sre
  `(: ,hour-sre ":" ,minute-sre
      (? ":" ,second-sre)
      (? ,secfrac-sre)))

(define full-time-sre
  `(: ,partial-time-sre ,time-offset))

(define date-time-sre
  `(: ,full-date-sre "T" ,full-time-sre))

(define date-time-irx
  (sre->irregex date-time-sre))

(define (rfc3339-string->date str)
  "Convert an RFC3339 formatted date string into an srfi-19 date type."
  (define (rx-match->date rx-match)
    (define (rx-part name)
      (irregex-match-substring rx-match name))
    (define (rx-number name)
      (and=> (rx-part name)
             string->number))
    (let ((nsecs (min (or (rx-number 'secfrac) 0)
                      999999999))
          (seconds (or (rx-number 'second) 0))
          (minutes (rx-number 'minute))
          (hours (rx-number 'hour))
          (date (rx-number 'mday))
          (month (rx-number 'month))
          (year (rx-number 'fullyear))
          (offset (let ((plusminus (rx-part 'offset-plusminus))
                        (offset-hour (rx-number 'offset-hour))
                        (offset-minute (rx-number 'offset-minute)))
                    (match plusminus
                      ;; No offset, return 0 seconds
                      (#f 0)
                      ;; Positive offset
                      ("+"
                       (+ (* offset-hour 60 60)
                          (* offset-minute 60)))
                      ;; negative offset
                      ("-"
                       (* (+ (* offset-hour 60 60)
                             (* offset-minute 60))
                          -1))))))
      (make-date nsecs seconds minutes hours date month year offset)))
  (and=> (irregex-match date-time-irx str)
         rx-match->date))

;; @@: Well, this isn't very fast either.  Only about 5k / second.
;;   I guess Guile 2.0 isn't very fast with strings :)
(define (date->rfc3339-string date)
  "Convert an srfi-19 date type into an RFC3339 formatted date string."
  (define (format-2-digits digit)
    (format #f "~2,'0d" digit))

  (string-append
   (format #f "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
           (date-year date)
           (date-month date)
           (date-day date)
           (date-hour date)
           (date-minute date)
           (date-second date))
   ;; Append nanoseconds, if appropriate
   (let ((nsec (date-nanosecond date)))
     (cond ((and nsec  ; @@: is date-nanosecond ever #f?
                 (not (eqv? nsec 0)))
            (string-append "." (number->string nsec)))
           (else "")))
   (let ((offset (date-zone-offset date)))
     (cond ((and offset (not (eqv? offset 0)))
            (let* ((abs-offset (abs offset))
                   (hour (floor (/ abs-offset (* 60 60))))
                   (minute (floor (/ (- abs-offset (* hour 60 60)) 60))))
              (string-append (if (< offset 0)
                                 "-" "+")
                             (format-2-digits hour) ":"
                             (format-2-digits minute))))
           (else "")))))


;;; HTTP style dates
;;; ================

(define http-parse-date
  (@@ (web http) parse-date))
(define http-write-date
  (@@ (web http) write-date))

(define (http-date-string->date str)
  "Parse any of the date types defined in RFC2616 sec 3.3.1 into a <date>"
  (catch 'bad-header
    (lambda ()
      (catch 'bad-header-component
        (lambda ()
          (http-parse-date str))
        (const #f)))
    (const #f)))

(define (date->http-date-string date)
  "Parse a <date> into a string acceptable for HTTP headers, as defined
by RFC2616."
  (with-output-to-string
    (lambda ()
      (http-write-date date (current-output-port)))))
