;;; bndos/org/config.el -*- lexical-binding: t; -*-

(defvar bndos-org-image-max-height 0.8
  "Maximum inline image height, as a fraction of the window's body height.")

(defvar bndos-org-mermaid-config (file-name-concat (dir!) "mermaid-config.json"))

(setq org-agenda-files '("~/Dropbox/agenda.org")
      org-roam-directory "~/Dropbox/org-roam"
      org-journal-date-format "%a, %Y %b %d"
      ;; A fraction is more reliable for SVGs than a fixed pixel width; a list
      ;; still lets #+ATTR_ORG: :width override it.
      org-image-actual-width '(0.9)
      org-image-max-width 'window)

(map! :after org
      :map org-mode-map
      :n "g k" #'org-up-element
      :n "g j" #'org-down-element)

(after! org
  (setq org-startup-with-latex-preview t
        org-src-fontify-natively t)
  ;; Native SVG rendering, capped at `bndos-org-image-max-height'.
  (advice-add #'org--create-inline-image :around #'bndos-org-inline-svg-a))

;; ImageMagick drops text from Mermaid's SVG labels; keep SVGs on librsvg.
(after! image-mode
  (add-to-list 'imagemagick-types-inhibit 'SVG)
  (add-to-list 'imagemagick-types-inhibit 'SVGZ))

(use-package! org-ref
  :after org)

(use-package! olivetti
  :defer t
  :config
  (setq olivetti-body-width 170))

(use-package! org-draw
  :commands (org-draw org-draw-edit org-draw-setup)
  :init
  (map! :after org
        :map org-mode-map
        "C-c d d" #'org-draw
        "C-c d e" #'org-draw-edit
        "C-c d s" #'org-draw-setup)
  :config
  (setq org-draw-directory "figures"
        org-draw-require-pairing t
        org-draw-open-browser t))


;;; LaTeX export

(after! ox-latex
  (setq org-latex-src-block-backend 'engraved
        org-latex-hyperref-template nil
        org-latex-toc-command "\\tableofcontents \\clearpage")
  (add-to-list 'org-latex-classes
               '("extarticle" "\\documentclass{extarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package! engrave-faces-latex
  :after ox-latex
  :config
  (add-to-list 'org-latex-engraved-options '("linenos" "true")))


;;; Mermaid

;; Doom loads babel languages lazily, so ob-mermaid loads on the first mermaid
;; block.
(after! ob-mermaid
  (setq ob-mermaid-default-config-file bndos-org-mermaid-config)
  (add-to-list 'org-babel-default-header-args:mermaid '(:theme . "dark"))
  (add-to-list 'org-babel-default-header-args:mermaid '(:background-color . "transparent"))
  (advice-add #'org-babel-execute:mermaid :after #'bndos-org-mermaid-fix-svg-spacing-a)
  ;; Point mermaid-cli at the system browser rather than Puppeteer's own copy.
  (when-let* ((chrome (seq-some #'executable-find
                                '("google-chrome" "chromium" "chromium-browser"))))
    (setenv "PUPPETEER_EXECUTABLE_PATH" chrome)))
