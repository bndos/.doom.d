;;; init.el -*- lexical-binding: t; -*-

;; Run `doom sync' after changing this file.  `SPC h d m' lists every module
;; and its flags.  My own modules live in modules/bndos/ and load last.

;; Must be set before evil loads.
(setq evil-respect-visual-line-mode t)

(doom! :completion
       corfu
       vertico

       :ui
       doom
       dashboard
       hl-todo
       modeline
       ophints
       treemacs
       vc-gutter
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       format
       multiple-cursors
       snippets

       :emacs
       (dired +icons)
       electric
       undo
       vc

       :term
       vterm

       :checkers
       (syntax +childframe)
       grammar

       :tools
       direnv
       docker
       (eval +overlay)
       lookup
       (lsp +eglot)
       (magit +forge)
       pass
       terraform

       :os
       (:if (featurep :system 'macos) macos)

       :lang
       (dart +flutter)
       emacs-lisp
       go
       (haskell +dante)
       javascript
       typescript
       kotlin
       latex
       ledger
       markdown
       (nix +lsp)
       (org +dragndrop)
       python
       rest
       rust
       sh
       yaml

       :config
       (default +bindings +smartparens)

       :bndos
       ui         ; theme, faces, frame, modeline, tab-bar
       windows    ; display rules, side windows, window navigation
       editor     ; evil, completion, clipboard, editing keys
       tools      ; dired, external openers, projectile, kubed
       langs      ; major modes, tree-sitter, per-language settings
       lsp        ; lsp-proxy/eglot dispatch, eldoc-box, formatting
       debug      ; dape
       git        ; magit, pr-review, consult-gh, code-review
       term       ; vterm, external terminal, compilation helpers
       markdown   ; inline markdown/LaTeX rendering, xwidget preview
       ein        ; managed Jupyter server and notebook UX (needs markdown)
       org        ; org, babel, mermaid, export
       ai)        ; copilot, gptel, agent-shell
