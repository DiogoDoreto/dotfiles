;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! jsts
  :recipe (:host github :repo "DiogoDoreto/emacs-jsts"))

(package! tts
  :recipe (:host github :repo "DiogoDoreto/emacs-tts"))

(package! modus-themes :pin "b2e0aff5edcdcb5f79a072dcba3f4c0c26ec1000")

(package! ef-themes :pin "e9af6a569f2ad68710471a48ddbd30f4ee4f353c")

(package! dts-mode :pin "cd1847d0e7162c1ecab772c6ca7ef4f3c5d2214d")

(package! groovy-mode :pin "7b8520b2e2d3ab1d62b35c426e17ac25ed0120bb")

(package! magit-delta :pin "5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616")

(package! git-link :pin "d9b375f79e6071a9926bf73bba64111adfc93bf5")

(package! fancy-compilation :pin "502d36e0fb4c4daedc16ea5d732dcbc8285d6fb1")

(package! info-rename-buffer :pin "87fb263b18717538fd04878e3358e1e720415db8")

(package! ct :pin "66fb78baf83525ca068c3ddd156ef0989a65bf9d") ;; Color Tools

(package! devdocs-browser :pin "f6c3b96748cb4e6d3022a2cece15d0d0fc437cd6")

(package! daemons :pin "4900fe1ec64ab339da29082e8fd4545fc6e48ec4")

(package! org-block-capf
  :recipe (:host github :repo "xenodium/org-block-capf")
  :pin "080cfd2ed630a6739633b07a8ab6b896a1b5ef4a")

(package! hnreader :pin "a56f67a99a855ca656da1c1985e09f44509e4bbb")

(package! writegood-mode :pin "d54eadeedb8bf3aa0e0a584c0a7373c69644f4b8")

(package! jinx :pin "75e8e4805fe6f4ab256bd59bec71464edbc23887")

(package! elfeed-protocol :pin "4f5e77a28c501db686ac06a2ea250a7b37d5420c")

(package! combobulate :pin "38773810b5e532f25d11c6d1af02c3a8dffeacd7")

;; remove when fixed: https://github.com/doomemacs/doomemacs/issues/8585
;; (package! nixos-options :pin "053a2d5110ce05b7f99bcc2ac4804b70cbe87916")

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
