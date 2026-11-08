;; -*- no-byte-compile: t; -*-
;;; bndos/langs/packages.el

(package! cmake-mode)
(package! csv-mode)
(package! meson-mode)
(package! metal-mode :recipe (:host github :repo "masfj/metal-mode"
                              :files ("metal-mode.el")))
;; mlir-mode is vendored in ./lisp.
