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
  #:use-module (gnu packages accepted)
  #:use-module (gnu packages updated-version))

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
     `(("python-sphinx" ,python-sphinx-1.5.2)))
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
     `(("python-sphinx-1.5.2" ,python-sphinx-1.5.2)))
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

(define-public python-httpbin
  (package
  (name "python-httpbin")
  (version "0.5.0")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "httpbin" version))
      (sha256
        (base32
          "1dc92lnk846hpilslrqnr63x55cxll4qx88gif8fm521gv9cbyvr"))))
  (build-system python-build-system)
  (propagated-inputs
    `(("python-decorator" ,python-decorator)
      ("python-flask" ,python-flask)
      ("python-itsdangerous" ,python-itsdangerous)
      ("python-markupsafe" ,python-markupsafe)
      ("python-setuptools" ,python-setuptools)
      ("python-six" ,python-six)))
  (home-page "https://github.com/Runscope/httpbin")
  (synopsis "HTTP Request and Response Service")
  (description "HTTP Request and Response Service")
  (license license:expat)))

(define-public python-pytest-httpbin
  (package
   (name "python-pytest-httpbin")
   ;; (version "0.2.3")
   (version "0.0.7")
   ;; (source
   ;;  (origin
   ;;   (method url-fetch)
   ;;   (uri (pypi-uri "pytest-httpbin" version))
   ;;   (sha256
   ;;    (base32
   ;;     "1y0v2v7xpzpyd4djwp7ad8ifnlxp8r1y6dfbxg5ckzvllkgridn5"))))
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "pytest-httpbin" version))
     (sha256
      (base32
       "08ghq923dn33rllip3vap2p9fb680g0i96jdn5lcpfy8amq8mbq3"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-setuptools" ,python-setuptools)
      ("python-six" ,python-six)
      ("python-httpbin" ,python-httpbin)))
   (arguments
    `(#:tests? #f))
   (home-page
    "https://github.com/kevin1024/pytest-httpbin")
   (synopsis
    "Easily test your HTTP library against a local copy of httpbin")
   (description
    "Easily test your HTTP library against a local copy of httpbin")
   (license license:expat)))

(define-public python-httpretty
  (package
   (name "python-httpretty")
   (version "0.8.14")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "httpretty" version))
     (sha256
      (base32
       "0vlp5qkyw3pxwwsg7xmdcfh1csvypvaz4m6abida8s4xmjxpdhc3"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-setuptools" ,python-setuptools)))
   (inputs
    `(("python-sphinx-rtd-theme" ,python-sphinx-rtd-theme-0.1.9)))
   (arguments
    `(#:tests? #f))
   (home-page
    "http://github.com/gabrielfalcao/httpretty")
   (synopsis "HTTP client mock for Python")
   (description "HTTP client mock for Python")
   (license license:expat)))

(define-public python-xenon
  (package
    (name "python-xenon")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xenon" version))
       (sha256
        (base32
         "1c03nis488ls50cgcq7ghbj55nxsi6a9683lsvg6z6vaj4smc8g8"))))
    (build-system python-build-system)
    (inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-radon" ,python-radon)
       ("python-requests" ,python-requests-2.10.0)
       ("python-pyyaml" ,python-pyyaml)
       ("python-httpretty" ,python-httpretty)))
    (arguments
     `(#:tests? #f))
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
                       #t)))
       #:tests? #f))
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

(define-public python-typing
  (package
   (name "python-typing")
   (version "3.5.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "typing" version))
     (sha256
      (base32
       "08gz3grrh3vph5ib1w5x1ssnpzvj077x030lx63fxs4kwg3slbfa"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-setuptools" ,python-setuptools)))
   (arguments
    `(#:tests? #f))
   (home-page "https://docs.python.org/3.5/library/typing.html")
   (synopsis "Type Hints for Python")
   (description "Type Hints for Python")
   (license license:psfl)))

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
   (propagated-inputs
    `(("python-typing" ,python-typing)))
   (home-page "https://bitbucket.org/ruamel/yaml")
   (synopsis
    "YAML 1.2 parser/emitter")
   (description
    "This package provides YAML parser/emitter that supports roundtrip
preservation of comments, seq/map flow style, and map key order.  It
is a derivative of Kirill Simonov’s PyYAML 3.11.  It supports YAML 1.2
and has round-trip loaders and dumpers. It supports comments. Block
style and key ordering are kept, so you can diff the source.")
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
     "Conda is a cross-platform, Python-agnostic binary package
manager.  It is the package manager used by Anaconda installations,
but it may be used for other systems as well.  Conda makes
environments first-class citizens, making it easy to create
independent environments even for C libraries.  Conda is written
entirely in Python, and is BSD licensed open source.")
    (license license:bsd-3)))

(define-public conda
  (package
    (inherit python-conda)
    (name "conda")
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
                    (apply system* "python" "utils/setup-testing.py" "bdist" '())))
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
                                           configure-flags))
                           (main-dir (getcwd))
                           (dist-dir (string-append main-dir "/dist"))
                           (build-dir (string-append main-dir "/build"))
                           (dir-stream (opendir dist-dir))
                           (tar-file (string-append dist-dir "/" (readdir dir-stream))))
                      (apply system* "python" "utils/setup-testing.py"
                             "install" params)
                      (chdir build-dir)
                      (system* "tar" "-xvzf" tar-file "--strip-components=4")
                      (chdir main-dir)
                      (closedir dir-stream)
                      (system* "cp"
                               "-fvR"
                               (string-append build-dir "/bin")
                               out)
                      (system* "cp"
                               "-fvR"
                               (string-append build-dir "/lib")
                               out)))))
       #:tests? #f))
    (propagated-inputs
     `(("python-ruamel.yaml" ,python-ruamel.yaml)))
    (inputs
     `(("python-requests" ,python-requests)
       ("python-auxlib" ,python-auxlib)
       ("git" ,git)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pycosat" ,python-pycosat)))))
