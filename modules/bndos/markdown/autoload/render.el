;;; bndos/markdown/autoload/render.el -*- lexical-binding: t; -*-

(defun bndos-markdown--skip-buffer-p ()
  "Non-nil in EIN Python cell buffers, where # comments aren't headings."
  (and (fboundp 'bndos-ein-python-cell-buffer-p)
       (bndos-ein-python-cell-buffer-p)))

(defun bndos-markdown--overlay-at-point-p (overlay)
  (and (overlay-buffer overlay)
       (<= (overlay-start overlay) (point) (overlay-end overlay))))

(defun bndos-markdown--delete-overlays (var category &optional beg end)
  "Delete overlays tracked in VAR or tagged CATEGORY between BEG and END."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
    (dolist (overlay (symbol-value var))
      (when (and (overlay-buffer overlay)
                 (< (overlay-start overlay) end)
                 (> (overlay-end overlay) beg))
        (delete-overlay overlay)))
    (dolist (overlay (overlays-in beg end))
      (when (eq (overlay-get overlay 'category) category)
        (delete-overlay overlay)))
    (set var (seq-filter #'overlay-buffer (symbol-value var)))))

(defun bndos-markdown--delete-overlays-at-point (var)
  "Delete overlays in VAR covering point.  Return non-nil if any were deleted."
  (let (deleted)
    (dolist (overlay (symbol-value var))
      (when (bndos-markdown--overlay-at-point-p overlay)
        (delete-overlay overlay)
        (setq deleted t)))
    (set var (seq-filter #'overlay-buffer (symbol-value var)))
    deleted))

;;;###autoload
(defun bndos-markdown-clear-previews (&optional beg end)
  "Remove style and formula overlays between BEG and END."
  (bndos-markdown--delete-overlays 'bndos-markdown-style-overlays 'bndos-markdown-style beg end)
  (bndos-markdown--delete-overlays 'bndos-markdown-latex-overlays 'bndos-markdown-latex beg end))

;;;###autoload
(defun bndos-markdown-clear-html ()
  "Remove the full pandoc render, revealing the source."
  (bndos-markdown--delete-overlays 'bndos-markdown-html-overlays 'bndos-markdown-html))

;;;###autoload
(defun bndos-markdown-clear (&optional beg end)
  "Remove every markdown render overlay between BEG and END."
  (bndos-markdown--delete-overlays 'bndos-markdown-html-overlays 'bndos-markdown-html beg end)
  (bndos-markdown-clear-previews beg end))


;;; Style overlays (headings, inline code)

(defun bndos-markdown--push-style (overlay priority &optional face)
  (overlay-put overlay 'category 'bndos-markdown-style)
  (overlay-put overlay 'evaporate t)
  (overlay-put overlay 'priority priority)
  (when face (overlay-put overlay 'face face))
  (push overlay bndos-markdown-style-overlays))

(defun bndos-markdown--dim (beg end)
  "Dim markup between BEG and END without hiding it."
  (when (< beg end)
    (bndos-markdown--push-style (make-overlay beg end nil t nil) 8 'shadow)))

(defun bndos-markdown--heading-face (level)
  `(:height ,(pcase level (1 1.75) (2 1.45) (3 1.25) (4 1.12) (_ 1.05))
    :weight bold
    :foreground ,(pcase level
                   (1 "#7aa2f7") (2 "#9ece6a") (3 "#e0af68") (4 "#bb9af7")
                   (_ "#c0caf5"))))

;;;###autoload
(defun bndos-markdown-render-style (&optional beg end)
  "Style headings and inline code between BEG and END."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
    (bndos-markdown--delete-overlays 'bndos-markdown-style-overlays 'bndos-markdown-style beg end)
    (unless (bndos-markdown--skip-buffer-p)
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "^[ \t]*\\(#\\{1,6\\}\\)[ \t]+\\(.+?\\)[ \t]*#*[ \t]*$" end t)
          (let ((level (length (match-string 1)))
                (text-beg (match-beginning 2))
                (text-end (match-end 2)))
            (bndos-markdown--dim (match-beginning 1) text-beg)
            (bndos-markdown--dim text-end (line-end-position))
            (bndos-markdown--push-style (make-overlay text-beg text-end nil t nil)
                                        9 (bndos-markdown--heading-face level))))
        (goto-char beg)
        (while (re-search-forward "\\(?:^\\|[^\\\\]\\)\\(`\\)\\([^`\n]+\\)\\(`\\)" end t)
          (bndos-markdown--dim (match-beginning 1) (match-end 1))
          (bndos-markdown--dim (match-beginning 3) (match-end 3))
          (bndos-markdown--push-style (make-overlay (match-beginning 2) (match-end 2) nil t nil)
                                      9 (if (facep 'markdown-code-face)
                                            'markdown-code-face
                                          'font-lock-constant-face)))))))


;;; LaTeX formula previews

(defun bndos-markdown--foreground-hex ()
  (if-let* ((rgb (color-values (face-attribute 'default :foreground nil 'default))))
      (apply #'format "%02x%02x%02x" (mapcar (lambda (v) (/ v 256)) rgb))
    "000000"))

;;;###autoload
(defun bndos-markdown-latex-svg (formula display)
  "Compile FORMULA (DISPLAY math if non-nil) to a cached SVG and return its path."
  (unless (and (executable-find "latex") (executable-find "dvisvgm"))
    (user-error "latex and dvisvgm are required for formula previews"))
  (let* ((color (bndos-markdown--foreground-hex))
         (dir (file-name-concat bndos-markdown-latex-cache-dir
                                (secure-hash 'sha256 (format "%s\n%s\n%s" color display formula))))
         (tex (file-name-concat dir "formula.tex"))
         (dvi (file-name-concat dir "formula.dvi"))
         (svg (file-name-concat dir "formula.svg")))
    (unless (file-exists-p svg)
      (make-directory dir t)
      (with-temp-file tex
        (insert "\\documentclass{article}\n"
                "\\usepackage[active,tightpage]{preview}\n"
                "\\usepackage{amsmath,amssymb,xcolor}\n"
                "\\pagestyle{empty}\n"
                "\\begin{document}\n\\begin{preview}\n"
                (format "\\color[HTML]{%s}\n" color)
                (format (if display "\\[%s\\]\n" "$%s$\n") formula)
                "\\end{preview}\n\\end{document}\n"))
      (let ((default-directory dir))
        (unless (zerop (call-process "latex" nil nil nil
                                     "-interaction=nonstopmode" "-halt-on-error" tex))
          (user-error "latex failed for formula preview; see %s" dir))
        (unless (and (file-exists-p dvi)
                     (zerop (call-process "dvisvgm" nil nil nil
                                          "--no-fonts" "--exact-bbox" dvi "-o" svg)))
          (user-error "dvisvgm failed for formula preview; see %s" dir))))
    svg))

;;;###autoload
(defun bndos-markdown-latex-ranges (beg end)
  "Return (START END FORMULA DISPLAY-P) for each TeX formula between BEG and END.
Recognizes $$...$$, \\[...\\] and $...$."
  (let (ranges)
    (cl-labels ((overlaps-p (start finish)
                  (seq-some (pcase-lambda (`(,s ,e . ,_)) (and (< start e) (> finish s)))
                            ranges))
                (collect (regexp group display)
                  (save-excursion
                    (goto-char beg)
                    (while (re-search-forward regexp end t)
                      (let ((start (match-beginning group))
                            (finish (match-end 0)))
                        (unless (overlaps-p start finish)
                          (push (list start finish
                                      (string-trim (match-string-no-properties (1+ group)))
                                      display)
                                ranges)))))))
      (collect "\\(\\$\\$\\)\\(\\(?:.\\|\n\\)*?\\)\\$\\$" 1 t)
      (collect "\\(\\\\\\[\\)\\(\\(?:.\\|\n\\)*?\\)\\\\\\]" 1 t)
      (collect "\\(?:^\\|[^\\\\]\\)\\(\\$\\)\\([^$\n]+?\\)\\$" 1 nil))
    (sort ranges (lambda (a b) (< (car a) (car b))))))

;;;###autoload
(defun bndos-markdown-render-latex (beg end)
  "Render TeX formulas between BEG and END as SVG overlays.  Return the count."
  (interactive "r")
  (bndos-markdown--delete-overlays 'bndos-markdown-latex-overlays 'bndos-markdown-latex beg end)
  (if (bndos-markdown--skip-buffer-p)
      0
    (let ((count 0))
      (pcase-dolist (`(,start ,finish ,formula ,display) (bndos-markdown-latex-ranges beg end))
        (unless (string-empty-p formula)
          (condition-case err
              (let ((overlay (make-overlay start finish nil t nil)))
                (overlay-put overlay 'display
                             (create-image (bndos-markdown-latex-svg formula display) 'svg nil
                                           :ascent 'center :scale bndos-markdown-latex-scale))
                (overlay-put overlay 'help-echo formula)
                (overlay-put overlay 'evaporate t)
                (overlay-put overlay 'category 'bndos-markdown-latex)
                (push overlay bndos-markdown-latex-overlays)
                (cl-incf count))
            (error (message "Skipping formula preview: %s" (error-message-string err))))))
      count)))


;;; Full render via pandoc + shr

(defun bndos-markdown--pandoc-html (markdown)
  "Convert MARKDOWN to HTML (with MathML) using pandoc."
  (unless (executable-find "pandoc")
    (user-error "pandoc is required for markdown rendering"))
  (with-temp-buffer
    (insert markdown)
    (unless (zerop (call-process-region (point-min) (point-max) "pandoc" t '(t nil) nil
                                        "--quiet"
                                        "-f" "markdown+tex_math_dollars+pipe_tables"
                                        "-t" "html" "--mathml"))
      (user-error "pandoc failed while rendering markdown"))
    (buffer-string)))

(defun bndos-markdown--html-to-string (html)
  "Render HTML to a propertized string with shr."
  (require 'shr)
  (let ((dom (with-temp-buffer
               (insert html)
               (libxml-parse-html-region (point-min) (point-max)))))
    (with-temp-buffer
      (let ((shr-width (max 80 (- (window-width) 4)))
            (shr-use-fonts t)
            (shr-use-colors nil))
        (shr-insert-document dom))
      (goto-char (point-min))
      (while (re-search-forward "\n\\{3,\\}" nil t)
        (replace-match "\n\n"))
      (buffer-string))))

;;;###autoload
(defun bndos/markdown-render-pandoc ()
  "Render the whole buffer with pandoc and shr, over the unchanged source."
  (interactive)
  (bndos-markdown-clear)
  (let* ((source (replace-regexp-in-string
                  "\\`[ \t\n]*markdown:[ \t]*" ""
                  (buffer-substring-no-properties (point-min) (point-max))))
         (overlay (make-overlay (point-min) (point-max) nil t nil)))
    (overlay-put overlay 'before-string
                 (bndos-markdown--html-to-string (bndos-markdown--pandoc-html source)))
    (overlay-put overlay 'display "")
    (overlay-put overlay 'priority 100)
    (overlay-put overlay 'category 'bndos-markdown-html)
    (push overlay bndos-markdown-html-overlays)
    (message "Rendered markdown with pandoc/shr")))


;;; Commands

;;;###autoload
(defun bndos/markdown-render ()
  "Refresh inline styling and formula previews for the whole buffer."
  (interactive)
  (bndos-markdown-clear-html)
  (bndos-markdown-render-style)
  (message "Rendered markdown: %s formula previews"
           (bndos-markdown-render-latex (point-min) (point-max))))

;;;###autoload
(defun bndos-markdown-clear-at-point ()
  "Reveal rendered markdown at point.  Return non-nil if anything was revealed."
  (seq-some #'bndos-markdown--delete-overlays-at-point
            '(bndos-markdown-html-overlays
              bndos-markdown-latex-overlays
              bndos-markdown-style-overlays)))

;;;###autoload
(defun bndos/markdown-edit-at-point ()
  "Reveal rendered markdown at point for editing, or insert a newline."
  (interactive)
  (unless (bndos-markdown-clear-at-point)
    (newline)))
