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
  #:use-module (gnu packages base)
  #:use-module (gnu packages accepted))

(define-public python-sphinx-new
  (package
   (name "python-sphinx-new")
   (version "1.5.2")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "Sphinx" version))
     (sha256
      (base32
       "08n765wix63yfvxhnrhlahb2wqn877ypdpnldr571r0974wli704"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-sphinx-alabaster-theme" ,python-sphinx-alabaster-theme)
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

(define-public python-pockets
  (package
   (name "python-pockets")
   (version "0.3.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "pockets" version))
     (sha256
      (base32
       "1l2v6522p1ybijawrr0rk5pr8rxciimgiw8bkjny4xxnqjjilzgc"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f))
   (propagated-inputs
    `(("python-setuptools" ,python-setuptools)
      ("python-six" ,python-six)))
   (home-page "http://pockets.readthedocs.org")
   (synopsis
    "A collection of helpful Python tools!")
   (description
    "A collection of helpful Python tools!")
   (license license:bsd-3)))

(define-public python-sphinxcontrib-napoleon
  (package
   (name "python-sphinxcontrib-napoleon")
   (version "0.6.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "sphinxcontrib-napoleon" version))
     (sha256
      (base32
       "0bqs1n4r00xdm18hpcvcb8ik5shcj7pdwny5fcz62hj3kdzdka3y"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f))
   (propagated-inputs
    `(("python-pockets" ,python-pockets)
      ("python-setuptools" ,python-setuptools)
      ("python-six" ,python-six)))
   (home-page
    "https://sphinxcontrib-napoleon.readthedocs.io")
   (synopsis "Sphinx \"napoleon\" extension.")
   (description "Sphinx \"napoleon\" extension.")
   (license license:bsd-3)))

(define-public python-mando-0.3.1
  (package
    (name "python-mando")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rubik/mando/archive/v"
                           version
                           ".tar.gz"))
       (sha256
        (base32
         "17jlkdpqw22z1nyml5ybslilqkzmnk0dxxjml8bfghav1l5hbwd2"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-rst2ansi" ,python-rst2ansi)))
    (native-inputs
     `(("python-sphinx-new" ,python-sphinx-new)))
    (arguments
     `(#:tests? #f))
    (home-page "https://mando.readthedocs.org/")
    (synopsis
     "Wrapper around argparse, allowing creation of complete CLI applications")
    (description
     "This package is a wrapper around argparse, allowing you to write complete CLI
 applications in seconds while maintaining all the flexibility.")
    (license license:expat)))

(define-public python-mando
  (package
    (name "python-mando")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mando" version))
       (sha256
        (base32
         "1bicmnxzxi9bxz9bfgv2rk7297f5rbwc9v2hg2rqfqr6h27zjgw5"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-rst2ansi" ,python-rst2ansi)))
    (native-inputs
     `(("python-sphinx-new" ,python-sphinx-new)))
    (arguments
     `(#:tests? #f))
    (home-page "https://mando.readthedocs.org/")
    (synopsis
     "Wrapper around argparse, allowing creation of complete CLI applications")
    (description
     "This package is a wrapper around argparse, allowing you to write complete CLI
 applications in seconds while maintaining all the flexibility.")
    (license license:expat)))

(define-public python-radon
  (package
    (name "python-radon")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "radon" version))
       (sha256
        (base32
         "15xyzavfj1zwb5rn07fs2wfi6ccys9b5q0s8hmnpqz712mifl92g"))))
    (build-system python-build-system)
    (inputs
     `(("python-colorama" ,python-colorama)
       ("python-flake8-polyfill"
        ,python-flake8-polyfill)
       ("python-mando" ,python-mando-0.3.1)))
    (arguments
     `(#:tests? #f))
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

(define-public python-auxlib
  (package
    (name "python-auxlib")
    (version "0.0.42")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "auxlib" version))
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

(define-public python-ruamel.yaml
  (package
    (name "python-ruamel.yaml")
    (version "0.13.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruamel.yaml" version))
       (sha256
        (base32
         "06x7vpjpnm17wrwkqras3a9xzivfvjs59qksrqssdm5190v6bzbw"))))
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
       ("python-pycosat" ,python-pycosat)))
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
         ;; (replace 'install
         ;;          (lambda* (#:key outputs (configure-flags '())
         ;;                    use-setuptools? #:allow-other-keys)
         ;;            (let* ((out (assoc-ref outputs "out"))
         ;;                   (params (append (list (string-append "--prefix=" out))
         ;;                                   (if use-setuptools?
         ;;                                       ;; distutils does not accept these flags
         ;;                                       (list "--single-version-externally-managed"
         ;;                                             "--root=/")
         ;;                                       '())
         ;;                                   configure-flags)))
         ;;              ;;(call-setuppy "install" params use-setuptools?)
         ;;              (apply system* "python" "utils/setup-testing.py"
         ;;                     "install" '()))))
         )
       #:tests? #f))
    (inputs
     `(("python-requests" ,python-requests)
       ("python-auxlib" ,python-auxlib)
       ("git" ,git)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pycosat" ,python-pycosat)
       ("python-ruamel.yaml" ,python-ruamel.yaml)))))
