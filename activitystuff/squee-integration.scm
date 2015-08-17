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

(define-module (activitystuff squee-integration)
  #:use-module (activitystuff base)
  #:use-module (json)
  #:use-module (squee)
  #:use-module (ice-9 match)
  #:export (insert-activity))

(define* (insert-as-obj conn as-obj #:key (local #t))
  "Insert AS-OBJ into database at connection CONN"
  (exec-query conn
              "INSERT INTO as_objects (data, local) VALUES ($1, $2)"
              (list (as->json as-obj) (if local "true" "false"))))

(define (mvreturn-single-as-obj results)
  "Friendly multi-value return of results for a single activitystreams object

Expect list of list style RESULTS where each row is (data local private-data)"
  (match results
    (((data local private-data))
     (values
      (json->as-obj data)
      ;; poor man's string->boolean cast
      (equal? local "t")
      (if (equal? private-data "")
          #nil
          (json->scm private-data))))))

(define (get-as-obj-by-db-id conn id)
  "Get an activitystreams object from the db"
  (mvreturn-single-as-obj
   (exec-query conn "SELECT data, local, private_data FROM as_objects WHERE id = $1"
               (list (number->string id)))))

(define (get-as-obj-by-object-id conn id)
  "Get an activitystreams object from the db"
  (mvreturn-single-as-obj
   (exec-query conn "SELECT data, local, private_data FROM as_objects WHERE data->>'@id' = $1"
               (list (number->string id)))))
