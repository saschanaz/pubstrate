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
  #:use-module (squee)
  #:export (insert-activity))

(define* (insert-as-obj conn as-obj #:key (local #t))
  "Insert AS-OBJ into database at connection CONN"
  (exec-query conn
              "INSERT INTO as_objects (data, local) VALUES ($1, $2)"
              (list (as->json as-obj) (if local "true" "false"))))

