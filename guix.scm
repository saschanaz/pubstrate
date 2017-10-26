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
             (guix utils)
             ((guix build utils) #:select (with-directory-excursion))
             (gnu packages)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages gettext)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages tls)
             (gnu packages gnunet)
             (gnu packages gnupg)
             (guix licenses))

(define %source-dir (dirname (current-filename)))

(define guile-fibers-git
  (package
    (inherit guile-fibers)
    (name "guile-fibers")
    (version "git")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wingo/fibers.git")
                    (commit "0fa1fd6adf9980229a46956503a6bf36e8154a78")))
              (sha256
               (base32
                "0a782aa0v2d115427h1h57jkxy04axklan60dzgnsry4axw9iq8r"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'bootstrap
                    (lambda _
                      (zero? (system* "./autogen.sh"))))
                  (add-before 'configure 'setenv
                    (lambda _
                      (setenv "GUILE_AUTO_COMPILE" "0"))))
       ;; We wouldn't want this in the upstream fibers package, but gosh
       ;; running tests takes forever and is painful
       #:tests? #f))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("texinfo" ,texinfo)
       ("gettext" ,gettext-minimal)
       ,@(package-native-inputs guile-2.2)))))

(define guile-8sync-latest
  (package
    (inherit guile-8sync)
    (version "git")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://git.savannah.gnu.org/8sync.git")
             (commit "80d96fb807e55e14cf0cd0738ae8ae5b14288dc4")))
       (sha256
        (base32
         "1i1050nm3w4i93dggqd3w4zpvdq644cbi2814jwmz4jwhqz5as9w"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("guile" ,guile-2.2)
                     ("pkg-config" ,pkg-config)
                     ("texinfo" ,texinfo)))
    (propagated-inputs `(("guile-fibers" ,guile-fibers-git)))
    (inputs `(("gnunet" ,gnunet)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'bootstrap
                    (lambda _
                      (zero? (system* "./bootstrap.sh"))))
                  (add-before 'configure 'setenv
                    (lambda _
                      (setenv "GUILE_AUTO_COMPILE" "0"))))))
    (home-page "https://gnu.org/s/8sync/")
    (synopsis "Asynchronous actor model library for Guile")
    (description
     "GNU 8sync (pronounced \"eight-sync\") is an asynchronous programming
library for GNU Guile based on the actor model.")
    (license lgpl3+)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'bootstrap
                    (lambda _
                      (zero? (system* "./bootstrap.sh")))))))))

(define guile-gcrypt
  (package
    (name "guile-gcrypt")
    (version "git")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://notabug.org/cwebber/guile-gcrypt.git")
             (commit "6bda028ad7f67a1b75a04b1e3f172ec3d975391c")))
       (sha256
        (base32
         "1xmlhkzd9b29rmipw7g71j5fvbzlj24wgpx1m2g49rm932f9pn8r"))))
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
     `(("guile" ,guile-2.2)
       ("libgcrypt" ,libgcrypt)))
    (home-page "https://notabug.org/cwebber/guile-gcrypt")
    (synopsis "Crypto library for Guile using libgcrypt")
    (description "guile-gcrypt uses Guile's foreign function interface to wrap
libgcrypt to provide a variety of encryption tooling.")
    (license gpl3+)))

(define guile-webutils
  (package
    (name "guile-webutils")
    (version "git")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://notabug.org/cwebber/guile-webutils.git")
             (commit "8541904f761066dc9c27b1153e9a838be9a55299")))
       (sha256
        (base32
         "1s9n3hbxd7lfpdi0x8wr0cfvlsf6g62ird9gbspxdrp5p05rbi64"))))
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
     `(("guile" ,guile-2.2)))
    (propagated-inputs
     ;; @@: We use guile-irregex for date.scm, but we could refactor
     ;; it and get rid of this dependency.
     `(("guile-irregex" ,guile2.2-irregex)
       ("guile-gcrypt" ,guile-gcrypt)))
    (home-page "https://notabug.org/cwebber/guile-webutils")
    (synopsis "Web application authoring utilities for Guile")
    (description "Tooling to write web applications in Guile.")
    (license gpl3+)))

(define guile-next
  (package
    (inherit guile-2.2)
    (name "guile")
    (version "git")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.sv.gnu.org/guile.git")
                    (commit "155ddcdc3bfc0d5e87397f18cd4cfb2f062fbb75")))
              (sha256
               (base32
                "0sjp5ws7ccig880wkvjxl89737af7d3z89yw8svz1lgp35f5hbsc"))))
    (arguments
     (substitute-keyword-arguments `(;; Tests aren't passing for now.
                                     ;; Obviously we should re-enable this!
                                     #:tests? #f
                                     ,@(package-arguments guile-2.2))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'autogen
             (lambda _
               (zero? (system* "sh" "autogen.sh"))))
           (add-before 'autogen 'patch-/bin/sh
             (lambda _
               (substitute* "build-aux/git-version-gen"
                 (("#!/bin/sh") (string-append "#!" (which "sh"))))
               #t))))))
    ;; (native-inputs
    ;;  `(("autoconf" ,autoconf)
    ;;    ("automake" ,automake)
    ;;    ("libtool" ,libtool)
    ;;    ("flex" ,flex)
    ;;    ("texinfo" ,texinfo)
    ;;    ("gettext" ,gettext-minimal)
    ;;    ,@(package-native-inputs guile-2.2)))
    ;; lazy avoidance of having to import flex, etc
    (native-inputs
     (package-native-inputs guile-for-guile-emacs))))

(define pubstrate
  (package
    (name "pubstrate")
    (version "0.1-pre")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("texinfo" ,texinfo)))
    (inputs
     `(("guile" ,guile-next)
       ("libgcrypt" ,libgcrypt)))
    (propagated-inputs
     `(("gnutls" ,gnutls)
       ("guile-gdbm-ffi" ,guile2.2-gdbm-ffi)
       ("guile-irregex" ,guile2.2-irregex)
       ("guile-lib" ,guile2.2-lib)
       ("guile-8sync" ,guile-8sync-latest)
       ("guile-sjson" ,guile-sjson)
       ("guile-gcrypt" ,guile-gcrypt)
       ("guile-webutils" ,guile-webutils)))
    (home-page #f)
    (synopsis "ActivityStreams and ActivityPub implementation in Guile.")
    (description "ActivityStreams and ActivityPub implementation in Guile.
Includes a full (currently demo) web server.")
    (license gpl3+)))

pubstrate
