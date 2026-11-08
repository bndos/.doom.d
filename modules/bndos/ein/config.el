;;; bndos/ein/config.el -*- lexical-binding: t; -*-

;; EIN with a Jupyter server owned by this Emacs (random port + token), plus
;; notebook ergonomics: M-RET renders markdown cells or runs code cells and
;; advances, and Python cells get file-backed lsp-proxy.  Rendering comes from
;; the markdown module.

(defvar bndos-ein-managed-url nil
  "URL of the Jupyter server started by `bndos/ein-start-jupyter'.")

(defvar bndos-ein-managed-token nil
  "Auth token of the server at `bndos-ein-managed-url'.")

(defvar bndos-ein-lsp-temp-dir (file-name-concat temporary-file-directory "ein-lsp/")
  "Where Python cell buffers get backing files so language servers can attach.")

(defvar bndos-ein-preview-window-width 0.8
  "Frame-width fraction of the `bndos/ein-toggle-markdown-preview' window.")

(map! :leader
      :prefix ("j" . "jupyter")
      "r" #'bndos/ein-start-jupyter
      "R" #'bndos/ein-restart-jupyter
      "s" #'bndos/ein-stop-jupyter
      "o" #'bndos/ein-open-server)

(setq ein:jupyter-server-use-subcommand "notebook"
      ein:jupyter-server-args '("--no-browser" "--ip=127.0.0.1")
      ein:notebooklist-login-timeout 20000
      ein:output-area-inlined-images t
      ein:output-area-inlined-image-use-file-cache t
      ein:worksheet-warn-obsolesced-keybinding nil)

(after! ein-jupyter
  (setq ein:jupyter-server-command (or (executable-find "jupyter") "jupyter")))

;; Some EIN files are compiled without the `ein:log' macro expanded, so they call
;; it as a function.
(after! ein-log
  (when (macrop 'ein:log)
    (fset 'ein:log (lambda (level string &rest args)
                     (ein:log-wrapper level (lambda () (apply #'format string args)))))))

;; Authenticate requests to our server with a header instead of the /login
;; POST flow, which breaks with Jupyter Server 7.
(after! ein-query
  (advice-add #'ein:query-prepare-header :around #'bndos-ein-add-auth-header-a))

(after! ein-notebook
  (add-hook 'ein:notebook-mode-hook #'bndos-ein-notebook-evil-h)
  (add-hook 'python-mode-hook #'bndos-ein-python-cell-setup-h)
  (advice-add #'kill-buffer--possibly-save :before-until #'bndos-ein-kill-cell-without-saving-a)

  (map! :map ein:notebook-mode-map
        "C-c C-v" #'bndos/ein-toggle-markdown-preview
        "C-c C-l" #'bndos/ein-render-markdown
        "M-RET"   #'bndos/ein-run-cell-and-next
        "RET"     #'bndos/ein-notebook-ret
        :nvi "C-c C-v" #'bndos/ein-toggle-markdown-preview
        :nvi "C-c C-l" #'bndos/ein-render-markdown
        :nvi "M-RET"   #'bndos/ein-run-cell-and-next
        :n "RET"        #'bndos/ein-notebook-ret
        :n "SPC"        #'bndos/ein-execute-or-render-cell
        :n "S-<return>" #'ein:worksheet-execute-cell
        :n "]c"         #'ein:worksheet-goto-next-input
        :n "[c"         #'ein:worksheet-goto-prev-input))

(after! ein-markdown-mode
  (map! :map ein:markdown-mode-map
        "C-c C-v" #'bndos/ein-toggle-markdown-preview
        "C-c C-l" #'bndos/ein-render-markdown
        "M-RET"   #'bndos/ein-run-cell-and-next
        "RET"     nil
        :nvi "C-c C-v" #'bndos/ein-toggle-markdown-preview
        :nvi "C-c C-l" #'bndos/ein-render-markdown
        :nvi "M-RET"   #'bndos/ein-run-cell-and-next
        :n "RET" #'bndos/markdown-edit-at-point))
