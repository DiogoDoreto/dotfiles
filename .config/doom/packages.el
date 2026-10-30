;;; $DOOMDIR/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! jsts :recipe (:host github :repo "DiogoDoreto/emacs-jsts"))

(package! tts :recipe (:host github :repo "DiogoDoreto/emacs-tts"))

(package! modus-themes :pin "b1647b2980f4f27c3c81ac1531d992a14fefd06a")

(package! ef-themes :pin "48920f6ca277a0f55b3e21eefa04bdb57bfd8509")

(package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh")
  :pin "c8707e86d36ea4ec38e177ca8fa0313a5374a0a9")

(package! consult-omni :recipe (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el"))
  :pin "3a126ee54479755408faed10da945dbc2366303b")

(package! dts-mode :pin "cd1847d0e7162c1ecab772c6ca7ef4f3c5d2214d")

(package! groovy-mode :pin "7b8520b2e2d3ab1d62b35c426e17ac25ed0120bb")

(package! magit-delta :pin "5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616")

(package! git-link :pin "ca01d013bd575710e2cd47001ee1ef6ee41667cf")

(package! fancy-compilation :pin "502d36e0fb4c4daedc16ea5d732dcbc8285d6fb1")

(package! info-rename-buffer :pin "87fb263b18717538fd04878e3358e1e720415db8")

(package! ct :pin "0183c1120a5b405c9886fa3ec3fabef4dc4f9ec6") ;; Color Tools

(package! devdocs-browser :pin "a49adc1f20b745338db34b729a7c94394c1c1689")

(package! daemons :pin "4900fe1ec64ab339da29082e8fd4545fc6e48ec4")

(package! hnreader :pin "3a1ad5f3711b89e3e2f8506cc485ce9f8e8ca216")

(package! writegood-mode :pin "d54eadeedb8bf3aa0e0a584c0a7373c69644f4b8")

(package! elfeed-protocol :pin "58936590459ccc2dfd6132f69983011d15d9404a")
(package! elfeed-protocol-freshrss :recipe (:host codeberg :repo "lou/elfeed-protocol-freshrss")
  :pin "5b282ae09f994d9d63ac2f7aa0d07414d9802464")

(package! evil-textobj-tree-sitter :pin "fecc0e11615df31a6651ce11b012388e53cad4e9")

(package! alert :pin "31fc56855289d0846e73d7ca9b84b628aeac16a0")

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)
