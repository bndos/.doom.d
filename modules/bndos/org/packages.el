;; -*- no-byte-compile: t; -*-
;;; bndos/org/packages.el

(package! engrave-faces)
(package! org-ref)
(package! ob-mermaid)
(package! org-draw :recipe (:host github :repo "larrasket/org-draw"
                            :files ("org-draw.el" "web")))
(package! olivetti)
