;; -*- no-byte-compile: t; -*-
;;; bndos/windows/packages.el

(package! buffer-move)
;; `:pin nil' overrides the upstream pin from :ui workspaces; the separate
;; `:local-repo' stops straight from reusing an upstream clone.
(package! persp-mode
  :recipe (:host github :repo "bndos/persp-mode.el" :local-repo "persp-mode-bndos")
  :pin nil)
