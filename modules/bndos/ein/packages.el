;; -*- no-byte-compile: t; -*-
;;; bndos/ein/packages.el

(package! ein :recipe (:host github :repo "bndos/emacs-ipython-notebook"
                       :files ("lisp/*.el")))
