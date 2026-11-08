;;; bndos/lsp/autoload/dispatch.el -*- lexical-binding: t; -*-

(defun bndos-lsp--call (proxy-cmd eglot-cmd &optional fallback-cmd)
  "Call PROXY-CMD, EGLOT-CMD or FALLBACK-CMD depending on the active backend."
  (call-interactively
   (cond ((bound-and-true-p lsp-proxy-mode) proxy-cmd)
         ((bound-and-true-p eglot--managed-mode) eglot-cmd)
         (fallback-cmd)
         ((user-error "No LSP backend active in this buffer")))))

;;;###autoload
(defun bndos/lsp-find-definition ()
  "Jump to the definition at point."
  (interactive)
  (when (and (fboundp 'bndos-ein-python-cell-buffer-p)
             (bndos-ein-python-cell-buffer-p))
    (bndos-ein-python-lsp-setup))
  (better-jumper-set-jump)
  (bndos-lsp--call #'lsp-proxy-find-definition #'xref-find-definitions
                   #'+lookup/definition))

;;;###autoload
(defun bndos/lsp-find-declaration ()
  "Jump to the declaration at point."
  (interactive)
  (better-jumper-set-jump)
  (bndos-lsp--call #'lsp-proxy-find-declaration #'eglot-find-declaration))

;;;###autoload
(defun bndos/lsp-find-references ()
  "List references to the symbol at point."
  (interactive)
  (better-jumper-set-jump)
  (bndos-lsp--call #'lsp-proxy-find-references #'xref-find-references
                   #'+lookup/references))

;;;###autoload
(defun bndos/lsp-find-implementations ()
  "List implementations of the symbol at point."
  (interactive)
  (better-jumper-set-jump)
  (bndos-lsp--call #'lsp-proxy-find-implementations #'eglot-find-implementation))

;;;###autoload
(defun bndos/lsp-rename ()
  "Rename the symbol at point."
  (interactive)
  (bndos-lsp--call #'lsp-proxy-rename #'eglot-rename))

;;;###autoload
(defun bndos/lsp-code-actions ()
  "Run code actions for the region or point."
  (interactive)
  (bndos-lsp--call #'lsp-proxy-execute-code-action #'eglot-code-actions))

;;;###autoload
(defun bndos/lsp-describe-thing-at-point ()
  "Show full documentation for the symbol at point."
  (interactive)
  (bndos-lsp--call #'lsp-proxy-describe-thing-at-point #'eldoc-doc-buffer))

;;;###autoload
(defun bndos/lsp-project-diagnostics ()
  "List diagnostics for the whole project."
  (interactive)
  (bndos-lsp--call #'lsp-proxy-show-project-diagnostics #'+default/diagnostics
                   #'+default/diagnostics))

;;;###autoload
(defun bndos/format-buffer ()
  "Format the buffer with the active LSP backend, falling back to Doom's formatter."
  (interactive)
  (call-interactively
   (cond ((and (bound-and-true-p lsp-proxy-mode)
               (bound-and-true-p lsp-proxy--has-any-servers))
          #'lsp-proxy-format-buffer)
         ((bound-and-true-p eglot--managed-mode) #'eglot-format-buffer)
         ((bound-and-true-p apheleia-mode) #'apheleia-format-buffer)
         (#'+format/buffer))))
