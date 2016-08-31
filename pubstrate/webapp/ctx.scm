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

(define-module (pubstrate webapp ctx)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:export (%ctx ctx-ref
            with-extended-ctx ctx=> ctx=>*))

;;; Context system (aka ctx)
;;; ========================

;;; So this is a nice little parameters-based context system, kind of
;;; like flask's "application/request context".  Except, since we're using
;;; parameters, it's much cleaner.
;;; 
;;; This uses a vhashed parameter, %ctx.  However, you should probably never
;;; need to access %ctx directly.  Instead, use with-extended-ctx to "extend"
;;; the current context for the execution of some thunk.  Like so:
;;;
;;;   (with-extended-ctx
;;;    `((db . ,(setup-db))
;;;      (user . ,anonymous-user))
;;;    (lambda ()
;;;      (get-user-info (ctx-ref 'db) (ctx-ref 'user))))
;;;
;;; Please note that keys should only be symbols!  (Or things that can be
;;; compared with eq?)
;;;
;;; But, say that's your default application-level context, and as each request
;;; comes in, you need some additional context information.  Say you get
;;; some request, and you need to set up your users based on that request.
;;; No problem, we can nest things.
;;;
;;;   (with-extended-ctx             ; your applicattion context
;;;    `((db . ,(setup-db))
;;;      (user . ,anonymous-user))
;;;    (lambda ()
;;;      (with-extended-ctx          ; your request context
;;;       `((user . ,(get-user-from-request)))
;;;       (lambda ()
;;;         (get-user-info (ctx-ref 'db) (ctx-ref 'user))))))
;;;
;;; That's a very contrived (and unlikely) example for the sake of
;;; demonstration, but hopefully you get the idea.
;;;
;;; By default, ctx-ref returns #f if a variable is not defined.
;;; But that might not be what you want, so you can define your own default:
;;;
;;;   (ctx-ref 'language "en")
;;;
;;; But there's a real nasty issue: what if #f is a real possible return value?
;;; Coding around this is a pain.  Luckily, there's an easy solution with ctx=>
;;;
;;;   (ctx=> 'user
;;;          (lambda (user)
;;;            (format #f "User was defined as: ~s\n" user)))
;;;
;;; In the above case, if user was defined (even as #f), we'd get a string back
;;; with some information about its value.  But the expression itself would
;;; return #f if user was not defined as anything.
;;;
;;; We can also provide a thunk in case nothing was defined:
;;;
;;;   (ctx=> 'user
;;;          (lambda (user)
;;;            (format #f "User was defined as: ~s\n" user))
;;;          (lambda ()
;;;            "No user defined in %ctx!\n"))
;;;
;;; Lastly, ctx=>* is a macro provided for convenience, in case you want to
;;; reduce a level of nesting and want to provide just the body of the procedure
;;; in case the key is defined.
;;;
;;;   (ctx=>* 'user user
;;;          (format #f "User was defined as: ~s\n" user))
;;;
;;; That's it!  The implementation is pretty simple.  It's even considerably
;;; shorter than this documentation! :)

(define %ctx (make-parameter vlist-null))

(define* (ctx-ref key #:optional dflt)
  "Pull KEY out of %ctx, returning DFLT or #f if not found."
  (match (vhash-assq key (%ctx))
    ((key . val) val)
    (#f dflt)))

;;; Immutable and unique
(define %the-nothing (cons '*the* '*nothing*))

(define (with-extended-ctx alist thunk)
  "Run THUNK in extended %ctx with ALIST as its extended context."
  (define extended-ctx
    (fold (match-lambda*
            (((key . val) prev)
             (vhash-consq key val prev)))
          (%ctx) alist))

  (parameterize ((%ctx extended-ctx))
    (thunk)))

(define* (ctx=> key proc #:optional proc-if-not)
  "Call PROC with value of KEY in %ctx, but only if KEY is defined.

When KEY is not defined in %ctx, but proc-if-not is provided,
call thunk proc-if-not.  Otherwise return #f."
  (let ((val (ctx-ref key %the-nothing)))
    (if (eq? val %the-nothing)
        ;; No definition for this val.
        ;; If proc-if-not is provided, call that, otherwise return #f
        (and proc-if-not (proc-if-not))
        ;; looks like val was defined, so call proc
        (proc val))))

(define-syntax-rule (ctx=>* key varname body1 body2 ...)
  "Convenience macro to run body in case KEY is defined"
  (ctx=> key (lambda (varname)
               body1 body2 ...)))
