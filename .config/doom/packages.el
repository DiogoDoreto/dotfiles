;;; $DOOMDIR/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! jsts :recipe (:host github :repo "DiogoDoreto/emacs-jsts"))

(package! tts :recipe (:host github :repo "DiogoDoreto/emacs-tts"))

(package! modus-themes :pin "e81c3c6ce3cdeb02266487a6fc1461cb9194c70e")

(package! ef-themes :pin "db36d53c090d7820643157f2e81ee37937fe4a0d")

(package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh")
  :pin "7042619362cd47314d3d0f706fa9cac5bf07950c")

(package! consult-omni :recipe (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el"))
  :pin "3a126ee54479755408faed10da945dbc2366303b")

(package! dts-mode :pin "cd1847d0e7162c1ecab772c6ca7ef4f3c5d2214d")

(package! groovy-mode :pin "7b8520b2e2d3ab1d62b35c426e17ac25ed0120bb")

(package! magit-delta :pin "5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616")

(package! git-link :pin "3870ae57408dc72ae2215b0056d6661e2c198e75")

(package! fancy-compilation :pin "502d36e0fb4c4daedc16ea5d732dcbc8285d6fb1")

(package! info-rename-buffer :pin "87fb263b18717538fd04878e3358e1e720415db8")

(package! ct :pin "d47271cc1b5ef55cf74a3e25ac45a187b36910d5") ;; Color Tools

(package! devdocs-browser :pin "f6c3b96748cb4e6d3022a2cece15d0d0fc437cd6")

(package! daemons :pin "4900fe1ec64ab339da29082e8fd4545fc6e48ec4")

(package! hnreader :pin "a56f67a99a855ca656da1c1985e09f44509e4bbb")

(package! writegood-mode :pin "d54eadeedb8bf3aa0e0a584c0a7373c69644f4b8")

(package! elfeed-protocol :pin "58936590459ccc2dfd6132f69983011d15d9404a")
(package! elfeed-protocol-freshrss :recipe (:host codeberg :repo "lou/elfeed-protocol-freshrss")
  :pin "7f9eca125ffa1e82fbcd2d63b734f13203feb5a4")

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
