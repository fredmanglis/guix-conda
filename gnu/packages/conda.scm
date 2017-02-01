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

(define-module (gnu packages conda)
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
       (uri (string-append
             "https://pypi.python.org/packages/3c/19/b29bc04524e7d1dbde13272fbb67e45a8eb2"
             "4bb6d112cf10c46162b350d7/rst2ansi-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0vzy6gd60l79ff750scl0sz48r1laalkl6md6dwzah4dcadgn5qv"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-docutils" ,python-docutils)))
    (home-page
     "https://github.com/Snaipe/python-rst-to-ansi")
    (synopsis
     "Python rst converter to ansi-decorated console output")
    (description
     "Python module dedicated to rendering RST (reStructuredText) documents to
 ansi-escaped strings suitable for display in a terminal")
    (license license:expat)))

(define-public python-alabaster
  (package
   (name "python-alabaster")
   (version "0.7.9")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/71/c3/70da7d8ac18a4f4c502887bd2549e05745fa40"
           "3e2cd9d06a8a9910a762bc/alabaster-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "027anxzcb951gjlcc43y3rbn9qrw36d16vj9wd2smv5410xx9bs7"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-setuptools" ,python-setuptools)))
   (home-page "https://alabaster.readthedocs.io")
   (synopsis
    "A configurable sidebar-enabled Sphinx theme")
   (description
    "A configurable sidebar-enabled Sphinx theme")
   (license #f)))

(define-public python-snowballstemmer
  (package
   (name "python-snowballstemmer")
   (version "1.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/20/6b/d2a7cb176d4d664d94a6debf52cd8dbae1f720"
           "3c8e42426daa077051d59c/snowballstemmer-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0a0idq4y5frv7qsg2x62jd7rd272749xk4x99misf5rcifk2d7wi"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-setuptools" ,python-setuptools)))
   (arguments
    `(#:tests? #f))
   (home-page
    "https://github.com/shibukawa/snowball_py")
   (synopsis
    "This package provides 16 stemmer algorithms (15 + Poerter English stemmer) generated
from Snowball algorithms.")
   (description
    "This package provides 16 stemmer algorithms (15 + Poerter English stemmer) generated
from Snowball algorithms.")
   (license license:bsd-3)))

(define-public python-sphinx
  (package
   (name "python-sphinx")
   (version "1.5.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/e3/87/e271f7f0d498c7fdaec009c27955401d18ef35"
           "7c0d468e1eb2be36bdc68c/Sphinx-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "08n765wix63yfvxhnrhlahb2wqn877ypdpnldr571r0974wli704"))))
   (build-system python-build-system)
   (inputs
    `(("python-alabaster" ,python-alabaster)
      ("python-babel" ,python-babel)
      ("python-docutils" ,python-docutils)
      ("python-imagesize" ,python-imagesize)
      ("python-jinja2" ,python-jinja2)
      ("python-pygments" ,python-pygments)
      ("python-requests" ,python-requests)
      ("python-setuptools" ,python-setuptools)
      ("python-six" ,python-six)
      ("python-snowballstemmer"
       ,python-snowballstemmer)))
   (home-page "http://sphinx-doc.org/")
   (synopsis "Python documentation generator")
   (description "Python documentation generator")
   (license license:bsd-3)))

(define-public python-mando
  (package
    (name "python-mando")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/2b/52/684d9ab8c2ccfb611275f2e44d3ebc76a6a6"
             "c56f4afacd2e91237fa07ec3/mando-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1bicmnxzxi9bxz9bfgv2rk7297f5rbwc9v2hg2rqfqr6h27zjgw5"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-rst2ansi" ,python-rst2ansi)))
    (inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://mando.readthedocs.org/")
    (synopsis
     "Wrapper around argparse, allowing creation of complete CLI applications")
    (description
     "This package is a wrapper around argparse, allowing you to write complete CLI
 applications in seconds while maintaining all the flexibility")
    (license license:expat)))

(define-public python-flake8-polyfill
  (package
    (name "python-flake8-polyfill")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/71/6e/dd7e0f0ddf146213d0cc0b963b3d4c643482"
             "3ebe3992c29b523182bbf785/flake8-polyfill-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "02gn2wxvh9vnf7m7dld7ca4l60mg5c370hv3swwppkngwaqmcw67"))))
    (build-system python-build-system)
    (inputs
     `(("python-flake8" ,python-flake8)))
    (home-page "https://gitlab.com/pycqa/flake8")
    (synopsis "Polyfill package for Flake8 plugins")
    (description
     "This package that provides some compatibility helpers for Flake8 plugins that
 intend to support Flake8 2.x and 3.x simultaneously")
    (license license:expat)))

(define-public python-radon
  (package
    (name "python-radon")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/91/48/5853fa60811d6cec1043bd60b057aafc2270"
             "6e19ab90b33d8df8155e6b8d/radon-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "15xyzavfj1zwb5rn07fs2wfi6ccys9b5q0s8hmnpqz712mifl92g"))))
    (build-system python-build-system)
    (inputs
     `(("python-colorama" ,python-colorama)
       ("python-flake8-polyfill"
        ,python-flake8-polyfill)
       ("python-mando" ,python-mando)))
    (home-page "https://radon.readthedocs.org/")
    (synopsis "Code Metrics in Python")
    (description "Radon is a Python tool which computes various code metrics.  Supported
 metrics are:
 * raw metrics: SLOC, comment lines, blank lines, &c.
 * Cyclomatic Complexity (i.e.  McCabe’s Complexity)
 * Halstead metrics (all of them)
 * the Maintainability Index (a Visual Studio metric)")
    (license license:expat)))

(define-public python-xenon
  (package
    (name "python-xenon")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/0d/60/649ef1567dac76bf680d5d7498e37d9bf745"
             "f6a6214da7e5dba530c25481/xenon-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1c03nis488ls50cgcq7ghbj55nxsi6a9683lsvg6z6vaj4smc8g8"))))
    (build-system python-build-system)
    (inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-radon" ,python-radon)
       ("python-requests" ,python-requests)
       ("python-pyyaml" ,python-pyyaml)))
    (home-page "https://xenon.readthedocs.org/")
    (synopsis
     "Monitor code metrics for Python on your CI server")
    (description
     "Xenon is a monitoring tool based on Radon.  It monitors code’s complexity.  Ideally,
 Xenon is run every time code is committed.  Through command line options, various
 thresholds can be set for the complexity of code.  It will fail (i.e.  it will exit with
 a non-zero exit code) when any of these requirements is not met")
    (license license:expat)))

(define-public python-ddt
  (package
    (name "python-ddt")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/83/96/21a2cef2962a07768854d411a97366292669"
             "3173887560895e962cf952c9/ddt-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1c00ikkxr7lha97c81k938bzhgd4pbwamkjn0h4nkhr3xk00zp6n"))))
    (build-system python-build-system)
    (inputs
     `(("python-six" ,python-six)
       ("python-pyyaml" ,python-pyyaml)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/txels/ddt")
    (synopsis "Data-Driven/Decorated Tests")
    (description "DDT (Data-Driven Tests) allows you to multiply one test case by running
 it with different test data, and make it appear as multiple test cases")
    (license (license:non-copyleft
              "https://github.com/txels/ddt/blob/master/LICENSE.md"))))

(define-public python-auxlib
  (package
    (name "python-auxlib")
    (version "0.0.42")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/92/c7/304b651594ebc31fbe1aa201369ab7bd7e71"
             "8928543d8cc1063d46319c30/auxlib-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0pczs3a8ck3z6qhl2fldhm2dl2czxl4yj9kkhx47qlpwhy0726xj"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build
                     'set-source-date-epoch
                     (lambda* (set-source-date-epoch #:rest _)
                       ;; guix sets environment variable SOURCE_DATE_EPOCH=1 in
                       ;; guix/build/gnu-build-system.scm file
                       ;; The build for auxlib, however, calls the make_wheelfile_inner
                       ;; function which reads from SOURCE_DATE_EPOCH, giving a date in
                       ;; 1970, which is earlier than 1980, causing the error
                       ;; 'ZIP does not support timestamps before 1980'
                       ;; This phase fixes the issue, borrowing from phase
                       ;; ensure-no-mtimes-pre-1980 in guix/build/python-build-system.scm
                       (setenv "SOURCE_DATE_EPOCH" "315619200")
                       #t)))))
    (inputs
     `(("python-enum34" ,python-enum34)
       ("python-tox" ,python-tox)
       ("python-flake8" ,python-flake8)
       ("python-radon" ,python-radon)
       ("python-xenon" ,python-xenon)
       ("python-wheel" ,python-wheel)
       ("python-ddt" ,python-ddt)
       ("python-testtools" ,python-testtools)
       ("python-pycrypto" ,python-pycrypto)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://github.com/kalefranz/auxlib")
    (synopsis
     "Auxiliary library to the python standard library")
    (description
     "Auxlib is an auxiliary library to the python standard library.  The aim is to
 provide core generic features for app development in python.  Auxlib fills in some
 python stdlib gaps much like pytoolz has for functional programming, pyrsistent has for
 data structures, or boltons has generally")
    (license license:isc)))

(define-public python-pycosat
  (package
    (name "python-pycosat")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/76/0f/16edae7bc75b79376f2c260b7a459829785f"
             "08e463ecf74a8ccdef62dd4a/pycosat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1kl3wh1f47rc712n4bmwplbx3fqz3x9i1b587jrbpmvdva4c8f6l"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page
     "https://github.com/ContinuumIO/pycosat")
    (synopsis "Bindings to picosat (a SAT solver)")
    (description
     "This package provides efficient Python bindings to picosat on the C level, i.e.
 when importing pycosat, the picosat solver becomes part of the Python process itself")
    (license license:expat)))

(define-public python-typing
  (package
    (name "python-typing")
    (version "3.5.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/b6/0c/53c42edca789378b8c05a5496e689f44e5dd"
             "82bc6861d1ae5a926ee51b84/typing-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "08gz3grrh3vph5ib1w5x1ssnpzvj077x030lx63fxs4kwg3slbfa"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page
     "https://docs.python.org/3.5/library/typing.html")
    (synopsis "Type Hints for Python")
    (description "This module supports type hints as specified by PEP 484.  The most
 fundamental support consists of the types Any, Union, Tuple, Callable, TypeVar, and
 Generic.  For full specification please see PEP 484")
    (license license:psfl)))

(define-public python-ruamel.ordereddict
  (package
    (name "python-ruamel.ordereddict")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/b1/17/97868578071068fe7d115672b52624d421ff"
             "24e5e802f65d6bf3ea184e8f/ruamel.ordereddict-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1xmkl8v9l9inm2pyxgc1fm5005yxm7fkd5gv74q7lj1iy5qc8n3h"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-setuptools" ,python2-setuptools)))
    (arguments
     `(#:python ,python-2))
    (home-page
     "https://bitbucket.org/ruamel/ordereddict")
    (synopsis
     "Version of dict that keeps keys in insertion resp. sorted order")
    (description
     "This is an implementation of an ordered dictionary with Key Insertion Order (KIO:
 updates of values do not affect the position of the key), Key Value Insertion Order
 (KVIO, an existing key's position is removed and put at the back).  The standard library
 module OrderedDict, implemented later, implements a subset of ordereddict functionality.
Sorted dictionaries are also provided.  Currently only with Key Sorted Order (KSO, no
 sorting function can be specified, but a transform can be specified to apply on the key
 before comparison (e.g. string.lower))")
    (license license:expat)))

(define-public python-ruamel.yaml
  (package
    (name "python-ruamel.yaml")
    (version "0.13.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/0e/8c/769bc2e6eafeb65125edce30d06bc7ceb64f"
             "ec37d43cfb121458ed094aec/ruamel.yaml-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1nxh5h83kwphcr3ii8n1xlxp771dfc7iiw4iblrmgl6k88x3ah67"))))
    (build-system python-build-system)
    (inputs
     `(("python-ruamel.ordereddict"
        ,python-ruamel.ordereddict)
      ("python-typing" ,python-typing)))
    (home-page "https://bitbucket.org/ruamel/yaml")
  (synopsis
   "This package provide a YAML 1.2 loader/dumper for Python")
  (description
   "This package provides YAML parser/emitter that supports roundtrip preservation of
 comments, seq/map flow style, and map key order.  It is a derivative of Kirill Simonov’s
 PyYAML 3.11.  It supports YAML 1.2 and has round-trip loaders and dumpers that preserves
, among others:
 * comments
 * block style and key ordering are kept, so you can diff the round-tripped source
 * flow style sequences ( ‘a: b, c, d’) (based on request and test by Anthony Sottile)
 * anchors names that are hand-crafted (i.e. not of the form``idNNN``)
 * merges in dictionaries are preserved")
  (license license:expat)))

(define-public python-conda
  (package
    (name "python-conda")
    (version "4.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/conda/conda/archive/"
             version
             ".tar.gz"))
       (sha256
        (base32
         "18ykdrivqnshjmynafl91lhr0f2m4vckhf22ncl5x1d4j6fkhr6h"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build
                     'create-version-file
                     (lambda* (create-version-file #:rest _)
                       (let ((version-file (open-output-file "conda/.version")))
                         (display (version) version-file)
                         (close-output-port version-file)
                         #t))))))
    (inputs
     `(("python-requests" ,python-requests)
       ("python-auxlib" ,python-auxlib)
       ("git" ,git)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pycosat" ,python-pycosat)
       ("python-ruamel.yaml" ,python-ruamel.yaml)))
    (home-page "https://github.com/conda/conda")
    (synopsis
     "Cross-platform, OS-agnostic, system-level binary package manager")
    (description
     "Conda is a cross-platform, Python-agnostic binary package manager.  It is the
 package manager used by Anaconda installations, but it may be used for other systems as
 well.  Conda makes environments first-class citizens, making it easy to create
 independent environments even for C libraries.  Conda is written entirely in Python, and
 is BSD licensed open source.")
    (license license:bsd-3)))

(define-public python-conda-executable
  (package
    (inherit python-conda)
    (name "python-conda-executable")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build
                     'create-version-file
                     (lambda* (create-version-file #:rest _)
                       (let ((version-file (open-output-file "conda/.version")))
                         (display (version) version-file)
                         (close-output-port version-file)
                         #t)))
         (replace 'build
                  (lambda* (#:key use-setuptools? #:allow-other-keys)
                    (apply system* "python" "utils/setup-testing.py" "build" '())))
         (replace 'check
                  (lambda* (#:key tests? test-target use-setuptools? #:allow-other-keys)
                    (if tests?
                        (let ((before (find-files "build" "\\.egg-info$"
                                                  #:directories? #t)))
                          (apply system* "python" "utils/setup-testing.py"
                                 test-target '())
                          ;;(call-setuppy test-target '() use-setuptools?)
                          (let* ((after (find-files "build" "\\.egg-info$"
                                                    #:directories? #t))
                                 (inter (lset-difference eqv? after before)))
                            (for-each delete-file-recursively inter))))))
         (replace 'install
                  (lambda* (#:key outputs (configure-flags '())
                            use-setuptools? #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (params (append (list (string-append "--prefix=" out))
                                           (if use-setuptools?
                                               ;; distutils does not accept these flags
                                               (list "--single-version-externally-managed"
                                                     "--root=/")
                                               '())
                                           configure-flags)))
                      ;;(call-setuppy "install" params use-setuptools?)
                      (apply system* "python" "utils/setup-testing.py"
                             "install" '())))))
       #:tests? #f))))
