;; -*- no-byte-compile: t; -*-
;;; bndos/windows/packages.el

(package! buffer-move)
;; Fork of persp-mode (used by :ui workspaces).  `:pin nil' drops the pin
;; :ui workspaces sets on upstream, and the separate `:local-repo' makes straight
;; clone the fork instead of reusing an existing upstream checkout.
(package! persp-mode
  :recipe (:host github :repo "bndos/persp-mode.el" :local-repo "persp-mode-bndos")
  :pin nil)
