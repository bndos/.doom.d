;;; bndos/lsp/autoload/lsp-proxy.el -*- lexical-binding: t; -*-

;;;###autoload
(defun bndos-lsp-proxy-maybe-enable-h ()
  "Enable lsp-proxy, loading the project's .envrc first so servers see it."
  (when (and (fboundp 'envrc--find-env-dir)
             (envrc--find-env-dir)
             (not (eq (bound-and-true-p envrc--status) 'on)))
    (envrc-mode 1))
  (unless (eq (bound-and-true-p envrc--status) 'error)
    (lsp-proxy-mode 1)))

;;;###autoload
(defun bndos-lsp-proxy-imenu-markers-a (alist)
  "Convert lsp-proxy's (LINE . CHARACTER) imenu positions in ALIST to markers."
  (mapcar
   (pcase-lambda (`(,name . ,pos))
     (cons name
           (pcase pos
             (`(,(and line (pred numberp)) . ,(and char (pred numberp)))
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-min))
                  (forward-line line)
                  (forward-char char)
                  (point-marker))))
             ((pred consp) (bndos-lsp-proxy-imenu-markers-a pos))
             (_ pos))))
   alist))


;;; Format on save

(defvar bndos-lsp-proxy--formatting nil
  "Non-nil while `bndos-lsp-proxy-format-before-save-h' runs.")

(defun bndos-lsp-proxy--ruff-organize-imports-p (action)
  (and (equal (plist-get action :language_server_name) "ruff")
       (member (plist-get (plist-get action :lsp_item) :kind)
               '("source.organizeImports" "source.organizeImports.ruff"))))

(defun bndos-lsp-proxy--apply-action-edit (action)
  "Apply ACTION's workspace edit, if the server returned one."
  (when-let* ((edit (plist-get (plist-get action :lsp_item) :edit)))
    (lsp-proxy--apply-workspace-edit edit this-command)
    t))

(defun bndos-lsp-proxy--organize-imports ()
  "Organize imports in Python buffers via Ruff's code action."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (with-demoted-errors "lsp-proxy organize-imports failed: %s"
      (let ((actions
             (lsp-proxy--request
              'textDocument/codeAction
              (lsp-proxy--build-params
               (list :textDocument (lsp-proxy--TextDocumentIdentifier)
                     :range (lsp-proxy--region-range (point-min) (point-max))
                     :context (list :diagnostics (vector)
                                    :only (vector "source.organizeImports"))))
              :timeout 2)))
        (seq-some #'bndos-lsp-proxy--apply-action-edit
                  (seq-filter #'bndos-lsp-proxy--ruff-organize-imports-p actions))))))

(defun bndos-lsp-proxy-format-before-save-h ()
  "Organize imports and format the buffer synchronously before saving."
  (when (and (bound-and-true-p lsp-proxy-mode)
             (not bndos-lsp-proxy--formatting))
    (let ((bndos-lsp-proxy--formatting t))
      (with-demoted-errors "lsp-proxy format-on-save failed: %s"
        (bndos-lsp-proxy--organize-imports)
        (let ((edits
               (lsp-proxy--request
                'textDocument/formatting
                (lsp-proxy--build-params
                 (list :options
                       (list :tabSize (symbol-value (lsp-proxy--get-indent-width major-mode))
                             :insertSpaces (not indent-tabs-mode)
                             :trimTrailingWhitespace lsp-proxy-trim-trailing-whitespace
                             :insertFinalNewline lsp-proxy-insert-final-newline
                             :trimFinalNewlines lsp-proxy-trim-final-newlines)
                       :textDocument (lsp-proxy--TextDocumentIdentifier)))
                :timeout 2)))
          (unless (seq-empty-p edits)
            (eglot--apply-text-edits edits)))))))

;;;###autoload
(defun bndos-lsp-proxy-format-on-save-h ()
  (add-hook 'before-save-hook #'bndos-lsp-proxy-format-before-save-h nil t))
