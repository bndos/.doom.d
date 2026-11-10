;;; bndos/ui/config.el -*- lexical-binding: t; -*-

(defvar bndos-cursor-color "#ffed00")

(setq doom-theme 'doom-ephemeral
      doom-symbol-font (font-spec :family "MesloLGS NF" :weight 'bold)
      display-line-numbers-type 'relative
      recentf-menu-filter 'recentf-show-basenames
      compilation-max-output-line-length nil)

(add-hook 'doom-first-buffer-hook #'global-visual-line-mode)


;;; Dashboard

(setq +dashboard-functions '(+dashboard-widget-shortmenu
                             +dashboard-widget-footer))


;;; Frame

(dolist (param `((height . 80)
                 (width . 350)
                 (internal-border-width . 20)
                 (alpha-background . 98)
                 (cursor-color . ,bndos-cursor-color)))
  (add-to-list 'default-frame-alist param))

;; The initial frame exists before this file runs.
(set-frame-parameter nil 'alpha-background 98)

(setq window-divider-default-right-width 4
      window-divider-default-bottom-width 4
      window-divider-default-places t)
(window-divider-mode 1)
(blink-cursor-mode 1)


;;; Cursor color (GUI and terminal)

(add-hook! '(doom-load-theme-hook window-setup-hook server-after-make-frame-hook)
           #'bndos-ui-apply-cursor-color-h
           #'bndos-ui-apply-tty-separators-h)
(add-hook 'after-make-frame-functions #'bndos-ui-apply-cursor-color-h)
(add-hook 'kill-emacs-hook #'bndos-ui-reset-terminal-cursor-color-h)

(after! evil
  (setq evil-normal-state-cursor  `(box ,bndos-cursor-color)
        evil-insert-state-cursor  `(bar ,bndos-cursor-color)
        evil-visual-state-cursor  `(hollow ,bndos-cursor-color)
        evil-emacs-state-cursor   `(box ,bndos-cursor-color)
        evil-motion-state-cursor  `(box ,bndos-cursor-color)
        evil-replace-state-cursor `(hbar ,bndos-cursor-color)))


;;; Tab bar shows persp-mode workspaces

(after! persp-mode
  (setq tab-bar-show t
        tab-bar-new-button-show nil
        tab-bar-close-button-show nil
        tab-bar-format '(bndos-ui-tab-bar-persp-tabs))
  (tab-bar-mode 1))


;;; Modeline

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-height 15
        doom-modeline-persp-name t
        doom-modeline-workspace-name t)
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus
      github debug repl lsp minor-modes input-method indent-info buffer-encoding
      major-mode process vcs check time "     ")))


;;; Faces

(defface bndos-ui-flash-face '((t :background "#151515"))
  "Face briefly applied by `bndos/flash-buffer'.")

(map! "s-N" #'bndos/flash-buffer)

(custom-set-faces!
  '(default :background "#000000" :foreground "#ffffff")
  '(cursor :background "#ffed00" :foreground "#000000")
  '(fringe :background "#000000")
  '(header-line :background "#000000")
  '(hl-line :background "#272737")
  '(region :background "#3d1212")
  '(highlight :background "#331212" :foreground "#ffffff")
  '(match :background "#000000")
  '(font-lock-comment-face :foreground "#545577")
  '(help-key-binding :background "#161616" :foreground "#ffffff" :box nil)
  '(whitespace-tab :background "#000000")
  '(whitespace-space :background "#000000")

  ;; Windows, mode-line, tab-bar
  '(mode-line :background "#101010")
  '(mode-line-active :background "#050505")
  '(mode-line-inactive :background "#000000" :box nil)
  '(window-divider :foreground "#3d263e")
  '(window-divider-first-pixel :foreground "#3d263e")
  '(window-divider-last-pixel :foreground "#3d263e")
  '(vertical-border :foreground "#3d263e")
  '(tab-bar :height 1.1 :background "#000000")
  '(tab-bar-tab :weight bold :foreground "#ff79c6" :background "#000000")
  '(tab-bar-tab-inactive :weight normal :foreground "#ffffff" :background "#000000")
  '(+dashboard-banner :foreground "#fafaf3")

  ;; Completion, search, symbol highlights
  '(vertico-current :background "#573495")
  '(xref-match :foreground "#ffffff" :background "#2b458d")
  '(mc/cursor-bar-face :foreground "#ff79c6")
  '(eglot-highlight-symbol-face :background "#331212" :inherit bold)
  '(lsp-proxy-highlight-symbol-face :background "#331212" :inherit bold)
  '(pulse-highlight-face :background "#331212" :inherit bold)
  '(pulse-highlight-start-face :background "#331212" :inherit bold)

  ;; Markdown, org, shr, EIN
  '(markdown-code-face :inherit fixed-pitch :height 1.0 :background "#161616" :foreground "#ffffff")
  '(markdown-inline-code-face :inherit fixed-pitch :height 1.0 :background "#161616" :foreground "#ffffff")
  '(markdown-pre-face :inherit fixed-pitch :height 1.0 :background "#161616" :foreground "#ffffff")
  '(markdown-language-keyword-face :inherit fixed-pitch :height 1.0 :background "#161616" :foreground "#ffffff")
  '(markdown-highlight-face :inherit highlight :background "#331212" :foreground "#ffffff")
  '(shr-code :inherit fixed-pitch :background "#161616" :foreground "#ffffff")
  '(org-block-begin-line :background "#0f0f0f")
  '(org-block :background "#0f0f0f")
  '(org-block-end-line :background "#0f0f0f")
  '(ein:basecell-input-area-face :background "#0f0f0f")

  ;; Neutral review threads: this theme's `highlight' reads as a removal.
  '(diff-review-comment :inherit font-lock-comment-face :background "#141420" :extend t)
  '(diff-review-comment-bar :foreground "#e0af68")

  ;; Emacs 31/Doom theme workaround: the default doom-themes Gnus face chain can
  ;; form an inheritance cycle when child frames are created by Corfu/Flycheck.
  '(gnus-group-news-low :inherit nil :foreground "#545577" :weight normal)
  '(gnus-group-news-low-empty :inherit nil :foreground "#444444" :weight normal)

  ;; High-contrast Magit/diff colors, readable in terminal frames too.
  '(magit-header-line :background "#000000" :box nil)
  '(magit-diff-context :foreground "#c7c7d8" :background "#000000")
  '(magit-diff-context-highlight :foreground "#ffffff" :background "#111116")
  '(magit-diff-added :foreground "#7ee787" :background "#061a0b")
  '(magit-diff-added-highlight :foreground "#9effa8" :background "#0b2a12" :weight bold)
  '(magit-diff-removed :foreground "#ff7b72" :background "#230809")
  '(magit-diff-removed-highlight :foreground "#ff9b93" :background "#351011" :weight bold)
  '(magit-diff-base :foreground "#ffd866" :background "#221a05")
  '(magit-diff-base-highlight :foreground "#ffe88a" :background "#342608" :weight bold)
  '(magit-diff-file-heading :foreground "#ff79c6" :background "#000000" :weight bold)
  '(magit-diff-file-heading-highlight :foreground "#ff79c6" :background "#111116" :weight bold)
  '(magit-diff-hunk-heading :foreground "#d6c7ff" :background "#241836")
  '(magit-diff-hunk-heading-highlight :foreground "#ffffff" :background "#3a2457" :weight bold)
  '(magit-section-highlight :foreground unspecified :background "#17111f" :extend t :inverse-video nil)
  '(magit-diff-hunk-region :foreground "#ffffff" :background "#17111f" :weight bold :underline (:color "#ff79c6" :style line) :inverse-video nil :box nil :extend t)
  '(magit-diff-lines-heading :foreground "#000000" :background "#ff79c6")
  '(magit-diff-hunk-heading-selection :foreground "#ff79c6" :background "#17111f" :weight bold :inverse-video nil :extend t)
  '(magit-diff-file-heading-selection :foreground "#ff79c6" :background "#17111f" :weight bold :inverse-video nil :extend t)
  '(magit-diff-lines-boundary :foreground "#ff79c6" :background "#17111f" :inverse-video nil :extend t)
  '(magit-diff-revision-summary :foreground "#8be9fd" :background "#000000" :weight bold)
  '(magit-diff-revision-summary-highlight :foreground "#8be9fd" :background "#111116" :weight bold)
  '(diff-added :foreground "#7ee787" :background "#061a0b")
  '(diff-removed :foreground "#ff7b72" :background "#230809")
  '(diff-refine-added :foreground "#caffd0" :background "#14451f" :weight bold :inverse-video nil :box nil :extend t)
  '(diff-refine-removed :foreground "#ffd0cc" :background "#55161a" :weight bold :inverse-video nil :box nil :extend t)
  '(diff-refine-changed :foreground "#fff0a6" :background "#4a3a10" :weight bold :inverse-video nil :box nil :extend t)
  '(ediff-current-diff-A :background "#773333")
  '(ediff-current-diff-B :background "#337733")
  '(ediff-current-diff-C :background "#333377"))
