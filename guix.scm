;;; Pubstrate --- ActivityStreams based social networking for Guile
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
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
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages tls)
             (gnu packages gnunet)
             (gnu packages gnupg)
             (guix licenses))

(define %source-dir (dirname (current-filename)))

(define guile-8sync-latest
  (package
    (name "guile-8sync")
    (version "git")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://git.savannah.gnu.org/8sync.git")
             (commit "7972787723d08a491379b63e6e5dc1cc6a3fac87")))
       (sha256
        (base32
         "0m3k3cizi89frnw58dws3g4jcssck6jf1ahpadxxg3ncclqzad8r"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("guile" ,guile-3.0)
                     ("pkg-config" ,pkg-config)
                     ("texinfo" ,texinfo)))
    (propagated-inputs `(("guile-fibers" ,guile-fibers)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'bootstrap
                    (lambda _
                      (zero? (system* "./bootstrap.sh"))))
                  (add-before 'configure 'setenv
                    (lambda _
                      ;; quiet warnings
                      (setenv "GUILE_AUTO_COMPILE" "0")
                      #t)))))
    (home-page "https://gnu.org/s/8sync/")
    (synopsis "Asynchronous actor model library for Guile")
    (description
     "GNU 8sync (pronounced \"eight-sync\") is an asynchronous programming
library for GNU Guile based on the actor model.

Note that 8sync is only available for Guile 2.2.")
    (properties '((upstream-name . "8sync")))
    (license lgpl3+)))

(define guile-webutils
  (package
    (name "guile-webutils")
    (version "git")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://notabug.org/cwebber/guile-webutils.git")
             (commit "d309d65a85247e4f3cea63a17defd1e6d35d821f")))
       (sha256
        (base32
         "1a3bblk5zaldkkxn0a94s544drqm0w2i5fsjpghagd64m149blf0"))))
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
     `(("guile" ,guile-3.0)))
    (propagated-inputs
     ;; @@: We use guile-irregex for date.scm, but we could refactor
     ;; it and get rid of this dependency.
     `(("guile-irregex" ,guile-irregex)
       ("guile-gcrypt" ,guile-gcrypt)))
    (home-page "https://notabug.org/cwebber/guile-webutils")
    (synopsis "Web application authoring utilities for Guile")
    (description "Tooling to write web applications in Guile.")
    (license gpl3+)))


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
     `(("guile" ,guile-3.0)
       ("libgcrypt" ,libgcrypt)))
    (propagated-inputs
     `(("gnutls" ,gnutls)
       ("guile-gdbm-ffi" ,guile-gdbm-ffi)
       ("guile-irregex" ,guile-irregex)
       ("guile-lib" ,guile-lib)
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
