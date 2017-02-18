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

(define-module (gnu packages accepted)
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
  #:use-module (gnu packages base))

(define-public python-rst2ansi
  (package
    (name "python-rst2ansi")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rst2ansi" version))
       (sha256
        (base32
         "0vzy6gd60l79ff750scl0sz48r1laalkl6md6dwzah4dcadgn5qv"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-docutils" ,python-docutils)))
    (home-page "https://github.com/Snaipe/python-rst-to-ansi")
    (synopsis "Convert RST to ANSI-decorated console output")
    (description
     "Python module dedicated to rendering RST (reStructuredText) documents
to ansi-escaped strings suitable for display in a terminal.")
    (license license:expat)))

(define-public python-ddt
  (package
    (name "python-ddt")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ddt" version))
       (sha256
        (base32
         "1c00ikkxr7lha97c81k938bzhgd4pbwamkjn0h4nkhr3xk00zp6n"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-pyyaml" ,python-pyyaml)))
    (home-page "https://github.com/txels/ddt")
    (synopsis "Data-Driven Tests")
    (description
     "DDT (Data-Driven Tests) allows you to multiply one test case by running
it with different test data, and make it appear as multiple test cases.")
    (license license:expat)))

(define-public python2-ddt
  (package-with-python2 python-ddt))

(define-public python-pycosat
  (package
    (name "python-pycosat")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycosat" version))
       (sha256
        (base32
         "1kl3wh1f47rc712n4bmwplbx3fqz3x9i1b587jrbpmvdva4c8f6l"))))
    ;; TODO: Unundle picosat. http://fmv.jku.at/picosat/
    (build-system python-build-system)
    (home-page "https://github.com/ContinuumIO/pycosat")
    (synopsis "Bindings to picosat (a SAT solver)")
    (description
     "This package provides efficient Python bindings to @code{picosat} on
the C level.  When importing pycosat, the @code{picosat} solver becomes part
of the Python process itself.  @code{picosat} is a @dfn{Boolean Satisfiability
Problem} (SAT) solver.")
    (license license:expat)))

(define-public python2-pycosat
  (package-with-python2 python-pycosat))

(define-public python2-ruamel.ordereddict
  (package
    (name "python2-ruamel.ordereddict")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruamel.ordereddict" version))
       (sha256
        (base32
         "1xmkl8v9l9inm2pyxgc1fm5005yxm7fkd5gv74q7lj1iy5qc8n3h"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (zero? (system* "python" "test/testordereddict.py")))))))
    (home-page "https://bitbucket.org/ruamel/ordereddict")
    (synopsis "Version of dict that keeps keys in insertion order")
    (description
     "This is an implementation of an ordered dictionary with @dfn{Key
Insertion Order} (KIO: updates of values do not affect the position of the
key), @dfn{Key Value Insertion Order} (KVIO, an existing key's position is
removed and put at the back).  The standard library module @code{OrderedDict},
implemented later, implements a subset of @code{ordereddict} functionality.
Sorted dictionaries are also provided.  Currently only with @dfn{Key Sorted
Order} (KSO, no sorting function can be specified, but a transform can be
specified to apply on the key before comparison (e.g. @code{string.lower})).")
    (license license:expat)))
