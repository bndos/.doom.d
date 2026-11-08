;;; bndos/lsp/config.el -*- lexical-binding: t; -*-

;; Commands here dispatch to whichever backend owns the buffer: lsp-proxy for
;; the modes below, Eglot (Doom's :tools lsp +eglot) everywhere else.

(map! :n "g d" #'bndos/lsp-find-definition
      :n "g D" #'bndos/lsp-find-declaration
      :n "g r" #'bndos/lsp-find-references
      :n "g i" #'bndos/lsp-find-implementations
      (:leader
       "c a" #'bndos/lsp-code-actions
       "c d" #'bndos/lsp-describe-thing-at-point
       "c f" #'bndos/format-buffer
       "c r" #'bndos/lsp-rename
       "c x" #'bndos/lsp-project-diagnostics
       "["   #'flycheck-previous-error
       "]"   #'flycheck-next-error)
      (:after python
       :map python-mode-map
       "C-c f" #'bndos/format-buffer
       "C-c a" #'bndos/lsp-code-actions))


;;; lsp-proxy

(defvar bndos-lsp-proxy-modes
  '(c-mode c++-mode objc-mode metal-mode mlir-mode
    go-mode python-mode rust-mode terraform-mode
    js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode rjsx-mode
    web-mode less-css-mode)
  "Major modes that use lsp-proxy instead of Eglot.")

(defvar bndos-lsp-proxy-languages-config (file-name-concat (dir!) "languages.toml"))

(dolist (mode bndos-lsp-proxy-modes)
  (add-hook (intern (format "%s-hook" mode)) #'bndos-lsp-proxy-maybe-enable-h))

(after! lsp-proxy
  (setq lsp-proxy-user-languages-config bndos-lsp-proxy-languages-config
        lsp-proxy-copilot-server-name "copilot"
        lsp-proxy-diagnostics-provider :flycheck)

  (setq-hook! 'lsp-proxy-mode-hook flycheck-checker 'lsp-proxy)
  (add-hook 'lsp-proxy-mode-hook #'bndos-lsp-proxy-format-on-save-h)

  ;; lsp-proxy reports imenu positions as zero-based (LINE . CHARACTER) pairs;
  ;; imenu and breadcrumb expect markers.
  (advice-add #'lsp-proxy--convert-imenu-format
              :filter-return #'bndos-lsp-proxy-imenu-markers-a))


;;; Eldoc

;; Documentation goes to an on-demand child frame, never the echo area.
(setq eldoc-echo-area-use-multiline-p nil)
(setq-default eldoc-display-functions
              (remq #'eldoc-display-in-echo-area
                    (default-value 'eldoc-display-functions)))
(add-hook 'eldoc-mode-hook #'bndos-eldoc-disable-echo-area-h)

(use-package! eldoc-box
  :commands (eldoc-box-scroll-up eldoc-box-scroll-down eldoc-box-quit-frame)
  :init
  (map! :leader
        :desc "Show Eldoc popup"        "c h" #'bndos/eldoc-box-help-at-point
        :desc "Scroll Eldoc popup down" "c J" #'eldoc-box-scroll-up
        :desc "Scroll Eldoc popup up"   "c K" #'eldoc-box-scroll-down
        :desc "Quit Eldoc popup"        "c Q" #'eldoc-box-quit-frame)
  :config
  (setq eldoc-box-clear-with-C-g t
        eldoc-box-only-multi-line nil
        eldoc-box-max-pixel-width 900
        eldoc-box-max-pixel-height 500)
  (custom-set-faces!
    '(eldoc-box-body :background "#050505" :foreground "#f5f5f5")
    '(eldoc-box-border :background "#3a3a3a")))

(use-package! breadcrumb
  :hook (doom-first-buffer . breadcrumb-mode))
