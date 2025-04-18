;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
(package! solaire-mode :disable t)
(package! buffer-move)
(package! multiple-cursors)
(package! citeproc-org)
;; (package! tree-sitter)
;; (package! tree-sitter-langs)
(package! meson-mode)
(package! olivetti)
;; (package! matlab-mode)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
;; (package! leetcode)
(package! gptel)
;; (package! arxiv-mode)
;; (package! pulsar)
(package! lazy-ruff)
(package! flymake-ruff)
(package! breadcrumb)
(package! engrave-faces)
(package! org-ref)
(package! lsp-proxy :recipe (:host github :repo "jadestrong/lsp-proxy"
                :files ("lsp-proxy.el" "lsp-proxy")
                :pre-build (("cargo" "build" "--release") ("cp" "./target/release/lsp-proxy" "./"))))

(package! xclip)
(package! eglot-booster
        :recipe (:host github
                 :repo "jdtsmith/eglot-booster"
                 :branch "main"
                 :build (compile)))
(package! treesit-auto)
(package! dape)
;; (when (package! lsp-bridge
;;         :recipe (:host github
;;                  :repo "manateelazycat/lsp-bridge"
;;                  :branch "master"
;;                  :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;                  ;; do not perform byte compilation or native compilation for lsp-bridge
;;                  :build (:not compile)))
;;   (package! markdown-mode)
;;   (package! yasnippet))
;; (unpin! consult)
;; (package! jupyter)
;; (package! jupyter :pin "16cbda79167b4e2f2c6b61b218658f0f660d97f9")

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
