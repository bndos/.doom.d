;; -*- no-byte-compile: t; -*-
;;; bndos/lsp/packages.el

(package! lsp-proxy :recipe (:host github :repo "jadestrong/lsp-proxy"
                             :files ("*.el")))
(package! eldoc-box)
(package! breadcrumb)
