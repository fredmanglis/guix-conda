;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages updated-version)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages accepted)
  #:use-module (gnu packages conda))

(define-public python-sphinx-1.5.2
  (package
   (inherit python-sphinx)
   (name "python-sphinx")
   (version "1.5.2")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "Sphinx" version))
     (sha256
      (base32
       "08n765wix63yfvxhnrhlahb2wqn877ypdpnldr571r0974wli704"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-requests" ,python-requests-2.10.0)
      ("python-imagesize" ,python-imagesize)
      ("python-sphinx-alabaster-theme" ,python-sphinx-alabaster-theme)
      ("python-babel" ,python-babel)
      ("python-snowballstemmer" ,python-snowballstemmer)
      ("python-six" ,python-six)))
   (arguments
    `(#:tests? #f))))

(define-public python-requests-2.10.0
  (package
   (inherit python-requests)
   (name "python-requests")
   (version "2.10.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "requests" version))
            (sha256
             (base32
              "0m2vaasjdhrsf9nk05q0bybqw0w4w4p3p4vaw7730w8mi1bq3wb3"))))
   (build-system python-build-system)
   (native-inputs
    `(("python-py" ,python-py)
      ("python-pytest" ,python-pytest-2.9.2)
      ("python-pytest-cov" ,python-pytest-cov)
      ("python-wheel" ,python-wheel)
      ("python-pytest-httpbin" ,python-pytest-httpbin)))
   (arguments
    `(#:tests? #f))))

(define-public python-sphinx-rtd-theme-0.1.9
  (package
   (inherit python-sphinx-rtd-theme)
   (name "python-sphinx-rtd-theme")
   (version "0.1.9")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "sphinx_rtd_theme" version))
     (sha256
      (base32
       "18d0r63w7jpdrk4q5qy26n08vdlmnj9sar93akwjphyambw4cf17"))))
   (propagated-inputs
    `(("python-sphinx" ,python-sphinx-1.5.2)
      ("python-snowballstemmer" ,python-snowballstemmer)))))
