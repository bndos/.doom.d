;;; bndos/langs/config.el -*- lexical-binding: t; -*-

(add-load-path! "lisp")

(add-to-list 'auto-mode-alist '("\\.mm?\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.cuh?\\'" . c++-mode))


;;; Tree-sitter

(dolist (remap '((bash-mode . bash-ts-mode)
                 (js2-mode . js-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (json-mode . json-ts-mode)))
  (add-to-list 'major-mode-remap-alist remap))

(after! treesit
  (dolist (source '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
                    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                    (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
                    (agda "https://github.com/tree-sitter/tree-sitter-agda")
                    (go "https://github.com/tree-sitter/tree-sitter-go")))
    (add-to-list 'treesit-language-source-alist source)))


;;; Formatting with apheleia

(add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook
             rjsx-mode-hook typescript-mode-hook
             nix-mode-hook nix-ts-mode-hook)
           #'apheleia-mode)


;;; Languages

(use-package! typescript-ts-mode
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.\\(?:ts\\|jsx\\)\\'" . typescript-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 4))

(use-package! mlir-mode
  :mode "\\.mlir\\'"
  :config
  (setq mlir-opt "mlir-opt"))

(use-package! metal-mode
  :mode "\\.metal\\'")

(after! go-mode
  (setq gofmt-command "gofumpt"))
(add-hook! 'go-mode-hook
  (add-hook 'before-save-hook #'gofmt-before-save nil t))

(map! :after dart-mode
      :map dart-mode-map
      "C-M-x" #'flutter-run-or-hot-reload)

(after! eglot
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nixd"))))

(after! nix-mode
  ;; Doom defaults to nixfmt; use the formatter installed in the Nix profile.
  (setq nix-nixfmt-bin "alejandra")
  (set-formatter! 'alejandra '("alejandra" "-q") :modes '(nix-mode nix-ts-mode)))

(after! treemacs
  (setq treemacs-is-never-other-window nil
        treemacs-width-is-initially-locked nil)
  (treemacs-project-follow-mode 1)
  (treemacs-follow-mode 1))
