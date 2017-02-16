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

;;; Parts borrowed here from guile-sdl2

;;; Guile-sdl2 is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with guile-sdl2.  If not, see
;;; <http://www.gnu.org/licenses/>.


(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 popen)
             (ice-9 match)
             (ice-9 rdelim)
             (guix packages)
             (guix build-system gnu)
             (guix download)
             (guix git-download)
             (guix gexp)
             ((guix build utils) #:select (with-directory-excursion))
             (gnu packages)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages gnupg)
             (guix licenses))

(define %source-dir (dirname (current-filename)))

(define guile-without-select-bug
  (package
   (inherit guile-next)
   (version (package-version guile-next))
   (source (origin
              (method url-fetch)
              (uri (string-append "ftp://alpha.gnu.org/gnu/guile/guile-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0r9y4hw17dlxahik4zsccfb2f3p2a07wqndfm251bgmam9hln6gi"))
              (modules '((guix build utils)))

              ;; Remove the pre-built object files.  Instead, build everything
              ;; from source, at the expense of significantly longer build
              ;; times (almost 3 hours on a 4-core Intel i5).
              (snippet '(for-each delete-file
                                  (find-files "prebuilt" "\\.go$")))

              ;; Here's what we're adding
              (patches (list (string-append %source-dir
                                            "/build-aux/patch-guile-fix-live-repl.patch")))))))

(define guile2.2-lib
  ((@@ (gnu packages guile) package-for-guile-2.2)
   (package
     (inherit guile-lib)
     ;; @@: VERY hackily stubbing out the test suite....
     ;;   by not inheriting the arguments we potentially cause ourselves
     ;;   some trouble :)
     ;;   But, hopefully we have a fixed up guile2.2-lib soon.
     (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'patch-module-dir
                    (lambda _
                      (substitute* "src/Makefile.in"
                        (("^moddir = ([[:graph:]]+)")
                         "moddir = $(datadir)/guile/site/@GUILE_EFFECTIVE_VERSION@\n")
                        (("^godir = ([[:graph:]]+)")
                         "godir = \
$(libdir)/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache\n"))
                      #t))))))))

(define guile-sjson
  (package
    (name "guile-sjson")
    (version "0.1-pre")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/dustyweb/guile-sjson.git")
                    (commit "65f61e65cc994b284c37665ff662a9ffdcc64db5")))
              (sha256
               (base32
                "10jhi0hq0r5hmih0lwf9d0yajb467x9fbdmp865nkf4v37m288gv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _ (zero? (system* "sh" "bootstrap.sh")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("guile" ,guile-next)))
    (home-page "https://gitlab.com/dustyweb/guile-sjson")
    (synopsis "s-expression based json reader/writer for Guile")
    (description "guile-sjson is a json reader/writer for Guile.
It has a nice, simple s-expression based syntax.")
    (license lgpl3+)))

(define pubstrate
  (package
    (name "pubstrate")
    (version "0.1-pre")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _ (zero? (system* "sh" "bootstrap.sh")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("texinfo" ,texinfo)))
    (inputs
     `(("guile" ,guile-without-select-bug)
       ("guile-8sync" ,guile-8sync)
       ("libgcrypt" ,libgcrypt)))
    (propagated-inputs
     `(("guile-gdbm-ffi" ,guile2.2-gdbm-ffi)
       ("guile-irregex" ,guile2.2-irregex)
       ("guile-lib" ,guile2.2-lib)
       ("guile-sjson" ,guile-sjson)))
    (home-page #f)
    (synopsis "ActivityStreams and ActivityPub implementation in Guile.")
    (description "ActivityStreams and ActivityPub implementation in Guile.
Includes a full (currently demo) web server.")
    (license gpl3+)))

pubstrate
