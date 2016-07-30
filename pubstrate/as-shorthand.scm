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

(define-module (pubstrate as-shorthand)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
  #:export (object link activity intransitive-activity actor
            collection ordered-collection

            accept tentative-accept add arrive create delete
            arrive follow ignore join leave like offer invite
            reject tentative-reject remove undo update experience
            view listen read move travel announce block flag
            dislike

            application group person process service

            relationship content article album folder story
            document audio image video note page question
            event place mention profile))

(define-syntax-rule (shorthand name astype)
  (begin
    (define (name . kwargs)
      (apply make-as astype *basic-env* kwargs))
    (if (astype-notes astype)
        (set-procedure-property! name 'documentation
                                 (astype-notes astype)))))

(shorthand object ^Object)
(shorthand link ^Link)
(shorthand activity ^Activity)
(shorthand intransitive-activity ^IntransitiveActivity)
(shorthand actor ^Actor)
(shorthand collection ^Collection)
(shorthand ordered-collection ^OrderedCollection)

(shorthand accept ^Accept)
(shorthand tentative-accept ^TentativeAccept)
(shorthand add ^Add)
(shorthand arrive ^Arrive)
(shorthand create ^Create)
(shorthand delete ^Delete)
(shorthand arrive ^Arrive)
(shorthand follow ^Follow)
(shorthand ignore ^Ignore)
(shorthand join ^Join)
(shorthand leave ^Leave)
(shorthand like ^Like)
(shorthand offer ^Offer)
(shorthand invite ^Invite)
(shorthand reject ^Reject)
(shorthand tentative-reject ^TentativeReject)
(shorthand remove ^Remove)
(shorthand undo ^Undo)
(shorthand update ^Update)
(shorthand experience ^Experience)
(shorthand view ^View)
(shorthand listen ^Listen)
(shorthand read ^Read)
(shorthand move ^Move)
(shorthand travel ^Travel)
(shorthand announce ^Announce)
(shorthand block ^Block)
(shorthand flag ^Flag)
(shorthand dislike ^Dislike)

(shorthand application ^Application)
(shorthand group ^Organization)
(shorthand person ^Person)
(shorthand process ^Process)
(shorthand service ^Service)

(shorthand relationship ^Relationship)
(shorthand content ^Content)
(shorthand article ^Article)
(shorthand album ^Album)
(shorthand folder ^Folder)
(shorthand story ^Story)
(shorthand document ^Document)
(shorthand audio ^Audio)
(shorthand image ^Image)
(shorthand video ^Video)
(shorthand note ^Note)
(shorthand page ^Page)
(shorthand question ^Question)
(shorthand event ^Event)
(shorthand place ^Place)
(shorthand mention ^Mention)
(shorthand profile ^Profile)
