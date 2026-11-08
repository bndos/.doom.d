;;; bndos/ein/autoload/notebook.el -*- lexical-binding: t; -*-

;;;###autoload
(defun bndos-ein-notebook-evil-h ()
  "Start notebooks in normal state."
  (when (bound-and-true-p evil-mode)
    (evil-local-mode 1)
    (evil-normal-state)))


;;; Python cell buffers

;;;###autoload
(defun bndos-ein-python-cell-buffer-p ()
  "Non-nil in EIN's per-cell Python edit buffers."
  (and (derived-mode-p 'python-mode)
       (string-match-p "\\` \\*ein: .*\\[python\\]\\'" (buffer-name))))

;;;###autoload
(define-minor-mode bndos-ein-python-cell-mode
  "Keys for EIN Python cell buffers."
  :keymap (define-keymap
            "M-RET" #'bndos/ein-run-cell-and-next
            "C-c C-l" (cmd! (user-error "C-c C-l is only for markdown cells")))
  (when bndos-ein-python-cell-mode
    (bndos-markdown-clear)))

;;;###autoload
(defun bndos-ein-python-cell-setup-h ()
  (when (bndos-ein-python-cell-buffer-p)
    (bndos-ein-python-cell-mode 1)
    (bndos-ein-python-lsp-setup)))

(defun bndos-ein--delete-backing-file-h ()
  (when buffer-file-name
    (ignore-errors (delete-file buffer-file-name))))

;;;###autoload
(defun bndos-ein-kill-cell-without-saving-a (buffer)
  "Let EIN kill Python cell BUFFER without offering to save its backing file.
The file only exists for language servers; the content belongs to the notebook."
  (with-current-buffer buffer
    (bndos-ein-python-cell-buffer-p)))

(defun bndos-ein--copilot-reopen ()
  "Make Copilot reopen the cell under its new backing-file URI."
  (when (and buffer-file-name
             (bound-and-true-p copilot-mode)
             (fboundp 'copilot--ensure-doc-open))
    (when (boundp 'copilot--opened-buffers)
      (setq copilot--opened-buffers (delq (current-buffer) copilot--opened-buffers)))
    (copilot--ensure-doc-open)))

;;;###autoload
(defun bndos-ein-python-lsp-setup ()
  "Give this Python cell buffer a backing file and attach lsp-proxy to it."
  (when (and (bndos-ein-python-cell-buffer-p)
             (require 'lsp-proxy nil t))
    (let ((new-file (not buffer-file-name)))
      (when new-file
        (make-directory bndos-ein-lsp-temp-dir t)
        (setq-local buffer-file-name
                    (file-name-concat bndos-ein-lsp-temp-dir
                                      (concat (secure-hash 'sha1 (buffer-name)) ".py"))
                    buffer-offer-save nil
                    auto-save-default nil
                    create-lockfiles nil)
        (add-hook 'kill-buffer-hook #'bndos-ein--delete-backing-file-h nil t))
      (let ((write-region-inhibit-fsync t))
        (write-region (point-min) (point-max) buffer-file-name nil 'silent))
      (cond ((not (bound-and-true-p lsp-proxy-mode)) (lsp-proxy-mode 1))
            (new-file (lsp-proxy-mode -1) (lsp-proxy-mode 1)))
      (bndos-ein--copilot-reopen))))


;;; Locating cells and notebook buffers

(defun bndos-ein--current-cell ()
  (ignore-errors
    (with-timeout (1 nil)
      (ein:worksheet-get-current-cell :noerror t))))

(defun bndos-ein--current-markdown-cell ()
  (let ((cell (bndos-ein--current-cell)))
    (and cell (ein:markdowncell-p cell) cell)))

;;;###autoload
(defun bndos-ein-markdown-context-p ()
  "Non-nil in an EIN markdown edit buffer or on a markdown cell."
  (cond ((bndos-ein-python-cell-buffer-p) nil)
        ((eq major-mode 'ein:markdown-mode) t)
        ((derived-mode-p 'python-mode) nil)
        ((bndos-ein--current-markdown-cell) t)))

(defun bndos-ein--notebook-name (&optional buffer)
  "Return the notebook buffer name that BUFFER (an edit or notebook buffer) belongs to."
  (let ((name (string-trim-left (buffer-name buffer))))
    (when (and (string-prefix-p "*ein: " name)
               (string-match "\\.ipynb\\*" name))
      (substring name 0 (match-end 0)))))

(defun bndos-ein--notebook-buffer ()
  "Return the notebook buffer owning the current edit or notebook buffer."
  (or (buffer-base-buffer)
      (when-let* ((name (bndos-ein--notebook-name)))
        (get-buffer name))))

(defun bndos-ein--clear-sibling-cell-buffers ()
  "Clear stale render overlays from Python cell buffers of this notebook."
  (when-let* ((notebook (bndos-ein--notebook-name)))
    (dolist (buffer (buffer-list))
      (when (equal notebook (bndos-ein--notebook-name buffer))
        (with-current-buffer buffer
          (when (bndos-ein-python-cell-buffer-p)
            (bndos-markdown-clear)))))))


;;; Rendering

(defun bndos-ein--edit-block-bounds ()
  "Return (BEG . END) of the markdown block at point in an edit buffer."
  (let ((beg (point-min))
        (end (point-max)))
    (save-excursion
      (when (re-search-backward "^markdown:[ \t]*$" nil t)
        (setq beg (min (point-max) (1+ (line-end-position))))))
    (save-excursion
      (when (re-search-forward "^\\(?:markdown:[ \t]*$\\|In \\[.*\\]:\\)" nil t)
        (setq end (line-beginning-position))))
    (cons beg end)))

(defun bndos-ein--render-edit-block ()
  "Render only the markdown block at point.
Some edit buffers hold the whole notebook; rendering all of it would turn Python
comments into headings."
  (pcase-let ((`(,beg . ,end) (bndos-ein--edit-block-bounds)))
    (when (< beg end)
      (bndos-markdown-clear-previews beg end)
      (bndos-markdown-render-style beg end)
      (bndos-markdown-render-latex beg end))))

(defun bndos-ein--render-cell (cell)
  "Render CELL's input area in the current notebook buffer."
  (let ((beg (ein:cell-input-pos-min cell))
        (end (ein:cell-input-pos-max cell)))
    (bndos-markdown-render-style beg end)
    (message "Rendered %s formula previews" (bndos-markdown-render-latex beg end))))

(defun bndos-ein--current-markdown-cell-or-error ()
  (let ((cell (with-timeout (2 nil)
                (ein:worksheet-get-current-cell :noerror t))))
    (unless (and cell (ein:markdowncell-p cell))
      (user-error "Not in a markdown cell"))
    cell))

;;;###autoload
(defun bndos/ein-render-markdown ()
  "Render the current markdown cell (styling and formulas).  Return the cell."
  (interactive)
  (bndos-markdown-clear-html)
  (cond
   ((bndos-ein-python-cell-buffer-p)
    (bndos-markdown-clear)
    (user-error "Not in a markdown cell"))
   ;; Indirect markdown edit buffer: render here and in the notebook buffer.
   ((and (eq major-mode 'ein:markdown-mode) (buffer-base-buffer))
    (let ((cell (bndos-ein--current-markdown-cell-or-error))
          (notebook (buffer-base-buffer)))
      (bndos-ein--clear-sibling-cell-buffers)
      (bndos-ein--render-edit-block)
      (with-current-buffer notebook
        (bndos-ein--render-cell cell))
      cell))
   ((derived-mode-p 'ein:markdown-mode 'markdown-mode)
    (bndos/markdown-render))
   (t
    (let ((cell (bndos-ein--current-markdown-cell-or-error)))
      (bndos-ein--render-cell cell)
      cell))))


;;; Navigation

(defun bndos-ein--close-duplicate-edit-windows (edit-buffer notebook-buffer keep-window)
  "Replace windows showing EDIT-BUFFER (other than KEEP-WINDOW) with NOTEBOOK-BUFFER."
  (dolist (window (window-list nil 'no-minibuf))
    (when (and (not (eq window keep-window))
               (eq (window-buffer window) edit-buffer))
      (if (one-window-p)
          (set-window-buffer window notebook-buffer)
        (delete-window window)))))

;;;###autoload
(defun bndos-ein-goto-next-input (&optional anchor-cell)
  "Move to the next input cell, from a notebook or a cell edit buffer.
With ANCHOR-CELL, move relative to that cell rather than the notebook's current
ewoc node, which can drift while an indirect edit buffer is rendering."
  (when-let* ((origin (current-buffer))
              (notebook (bndos-ein--notebook-buffer)))
    (ignore-errors
      (with-timeout (1 nil)
        (unless (eq origin notebook)
          (set-window-buffer (selected-window) notebook)
          (bndos-ein--close-duplicate-edit-windows origin notebook (selected-window)))
        (when-let* ((window (get-buffer-window notebook)))
          (select-window window))
        (with-current-buffer notebook
          (if anchor-cell
              (if-let* ((next (ein:cell-next anchor-cell)))
                  (ein:cell-goto next)
                (message "No next input"))
            (when-let* ((node (ein:worksheet-get-current-ewoc-node)))
              (ein:worksheet-goto-next-input node))))))))

;;;###autoload
(defun bndos/ein-run-cell-and-next ()
  "Render a markdown cell or execute a code cell, then go to the next input."
  (interactive)
  (cond ((bndos-ein-markdown-context-p)
         ;; Capture the cell before rendering; point can move while rendering.
         (let ((cell (bndos-ein--current-markdown-cell)))
           (bndos/ein-render-markdown)
           (bndos-ein-goto-next-input cell)))
        ((bndos-ein-python-cell-buffer-p)
         (call-interactively #'ein:worksheet-execute-cell)
         (bndos-ein-goto-next-input))
        ((bndos-ein-goto-next-input))))

;;;###autoload
(defun bndos/ein-execute-or-render-cell (ws cell &optional insert)
  "Execute code CELL and go to the next one; render markdown CELL in place."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)))
  (if (ein:markdowncell-p cell)
      (bndos/ein-render-markdown)
    (ein:worksheet-execute-cell-and-goto-next ws cell insert)))

;;;###autoload
(defun bndos/ein-notebook-ret ()
  "Reveal rendered markdown at point, or go to the next input."
  (interactive)
  (unless (bndos-markdown-clear-at-point)
    (call-interactively #'ein:worksheet-goto-next-input)))


;;; Side preview

(defun bndos-ein--current-markdown-text ()
  (if-let* ((cell (bndos-ein--current-markdown-cell)))
      (ein:cell-get-text cell)
    (when (derived-mode-p 'ein:markdown-mode)
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun bndos/ein-toggle-markdown-preview ()
  "Toggle a side window previewing the current markdown cell with rendered formulas."
  (interactive)
  (if-let* ((window (get-buffer-window "*ein-md-preview*")))
      (delete-window window)
    (let ((text (bndos-ein--current-markdown-text)))
      (if (string-empty-p (or text ""))
          (message "Not in a markdown cell")
        (with-current-buffer (get-buffer-create "*ein-md-preview*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert text)
            (goto-char (point-min))
            (markdown-mode)
            (bndos-markdown-render-latex (point-min) (point-max))
            (view-mode 1)))
        (display-buffer "*ein-md-preview*"
                        `((display-buffer-in-side-window)
                          (side . right)
                          (window-width . ,bndos-ein-preview-window-width)))))))
