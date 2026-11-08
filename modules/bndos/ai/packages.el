;; -*- no-byte-compile: t; -*-
;;; bndos/ai/packages.el

(package! copilot)
(package! gptel)
(package! acp :recipe (:host github :repo "xenodium/acp.el" :files ("*.el")))
(package! agent-shell :recipe (:host github :repo "xenodium/agent-shell" :branch "main"
                               :files ("*.el")))
