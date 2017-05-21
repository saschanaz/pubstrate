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
             (gnu packages tls)
             (gnu packages gnupg)
             (guix licenses))

(define %source-dir (dirname (current-filename)))

;; (define guile-8sync-latest
;;   (package
;;     (inherit guile-8sync)
;;     (version "git")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "git://git.savannah.gnu.org/8sync.git")
;;              (commit "dfde2119df2a0adb86ec4921f95ef2c15692a593")))
;;        (sha256
;;         (base32
;;          "086smlch92n6z5xng0la9l9g6m145klw1c8222cgj32qhyarbkpk"))))
;;     (arguments
;;      `(#:phases (modify-phases %standard-phases
;;                   (add-before 'configure 'bootstrap
;;                               (lambda _
;;                                 (zero? (system* "./bootstrap.sh")))))))))

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
             (commit "7ef45b5396373ff42e91a1ecad1d9be6389d6934")))
       (sha256
        (base32
         "03hdz70l4ymbdvfm1xc3aycafps4x6al16qd4kzxxmyvkqxb7agh"))))
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
     `(("guile" ,guile-2.2)
       ("libgcrypt" ,libgcrypt)))
    (propagated-inputs
     `(("gnutls" ,gnutls)
       ("guile-gdbm-ffi" ,guile2.2-gdbm-ffi)
       ("guile-irregex" ,guile2.2-irregex)
       ("guile-lib" ,guile2.2-lib)
       ("guile-8sync" ,guile-8sync)
       ("guile-sjson" ,guile-sjson)
       ("guile-gcrypt" ,guile-gcrypt)
       ("guile-webutils" ,guile-webutils)))
    (home-page #f)
    (synopsis "ActivityStreams and ActivityPub implementation in Guile.")
    (description "ActivityStreams and ActivityPub implementation in Guile.
Includes a full (currently demo) web server.")
    (license gpl3+)))

pubstrate
