;;; bndos/debug/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun bndos-dape-python-command ()
  "Return the project's .venv Python, or \"python\"."
  (let ((venv-python (expand-file-name ".venv/bin/python"
                                       (or (ignore-errors (projectile-project-root))
                                           default-directory))))
    (if (file-executable-p venv-python) venv-python "python")))

;;;###autoload
(defun bndos-dape-go-package-dir ()
  "Return the current file's directory relative to dape's cwd, for Delve."
  (concat "./" (file-relative-name default-directory (dape-cwd))))

;;;###autoload
(defun bndos-dape-save-buffers-h ()
  "Save modified buffers before a session starts (for interpreted languages)."
  (save-some-buffers t t))

(defun bndos-dape--env-plist ()
  "Return the project's direnv environment (or `process-environment') as a plist."
  (let ((env (or (and (fboundp 'envrc--export)
                      (cdr (ignore-errors (envrc--export (projectile-project-root)))))
                 (mapcar (lambda (entry)
                           (let ((i (string-search "=" entry)))
                             (cons (substring entry 0 i)
                                   (if i (substring entry (1+ i)) ""))))
                         process-environment))))
    (cl-loop for (key . val) in env
             when (and (stringp key) (stringp val))
             append (list (intern (concat ":" key)) val))))

;;;###autoload
(defun bndos-dape-inject-env-a (fn config &rest args)
  "Merge the project's environment into CONFIG's :env before starting dape."
  (apply fn
         (plist-put config :env (append (bndos-dape--env-plist)
                                        (plist-get config :env)))
         args))
