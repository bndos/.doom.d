;;; bndos/markdown/config.el -*- lexical-binding: t; -*-

;; Display-only rendering for markdown buffers (also used by the ein module):
;; heading/code styling, LaTeX formulas as SVG, and a full pandoc+shr render.
;; Buffer text is never modified; everything is an overlay.

(defvar bndos-markdown-latex-scale 1.35
  "Scale factor for SVG formula previews.")

(defvar bndos-markdown-latex-cache-dir
  (file-name-concat temporary-file-directory "markdown-latex-preview/"))

(defvar-local bndos-markdown-latex-overlays nil)
(defvar-local bndos-markdown-style-overlays nil)
(defvar-local bndos-markdown-html-overlays nil)

(after! shr
  (set-face-attribute 'shr-h1 nil :height 2.05 :weight 'bold)
  (set-face-attribute 'shr-h2 nil :height 1.7 :weight 'bold)
  (set-face-attribute 'shr-h3 nil :height 1.4 :weight 'bold))


;;; xwidget preview

(use-package! markdown-xwidget
  :commands markdown-xwidget-preview-mode
  :init
  (map! :after markdown-mode
        :map markdown-mode-command-map
        "x" #'markdown-xwidget-preview-mode)
  :config
  (setq markdown-xwidget-command (or (executable-find "pandoc") markdown-command)
        markdown-xwidget-github-theme "dark"
        markdown-xwidget-mermaid-theme "dark"
        markdown-xwidget-code-block-theme "github-dark")
  (advice-add #'markdown-xwidget-header-html :around #'bndos-markdown-xwidget-mermaid-a))

(map! :after xwidget
      :map xwidget-webkit-mode-map
      "C-s" #'bndos/xwidget-search-forward
      "C-r" #'bndos/xwidget-search-backward
      :nm "n" #'bndos/xwidget-search-next
      :nm "N" #'bndos/xwidget-search-previous)
