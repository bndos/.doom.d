;;; init.el -*- lexical-binding: t; -*-

;; Must be set before evil loads.
(setq evil-respect-visual-line-mode t)

;; On macOS libgccjit can't find libemutls_w on its own, so native-comp
;; trampolines (built when a primitive is advised) fail to link.  Ask the
;; newest real gcc on PATH (`gcc' itself is clang there) where it is.
(when (featurep :system 'macos)
  (with-eval-after-load 'comp
    (when-let* ((gcc (seq-some (lambda (n) (executable-find (format "gcc-%d" n)))
                               (number-sequence 30 10 -1)))
                (lib (car (ignore-errors (process-lines gcc "-print-file-name=libemutls_w.a"))))
                ((file-exists-p lib)))
      (add-to-list 'native-comp-driver-options
                   (concat "-L" (file-name-directory lib)) t))))

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
       ai         ; copilot, gptel, agent-shell
       tasks)     ; worktrunk tasks, Hydra agent sessions, Hutch review (needs git, ai)
