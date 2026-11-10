;; -*- no-byte-compile: t; -*-
;;; bndos/ai/packages.el

(package! copilot)
(package! gptel)
(package! acp :recipe (:host github :repo "xenodium/acp.el" :files ("*.el"))
  :pin "2959da5fa8657da17a78ac6ceef303a50e3e69de")
(package! shell-maker :pin "b28ee61a2533e408ed180662f83bb84fc8f84ba6")
(package! agent-shell :recipe (:host github :repo "xenodium/agent-shell" :branch "main"
                               :files ("*.el"))
  :pin "6c162d0b860855e500011b83fccf4ba3a9b3308d")
