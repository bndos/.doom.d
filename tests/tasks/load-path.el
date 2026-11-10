;;; load-path.el --- Batch load-path for the tasks tests -*- lexical-binding: t; -*-
;; Puts Doom's built packages and this config's vendored libraries on the
;; load-path so the libraries can be compiled and tested with `emacs -Q --batch'.
(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (doomdir (expand-file-name "../../" here))
       (build (expand-file-name (format "~/.config/emacs/.local/straight/build-%s" emacs-version))))
  (dolist (dir (directory-files build t "\\`[^.]"))
    (when (file-directory-p dir) (add-to-list 'load-path dir)))
  (dolist (dir '("modules/bndos/git/lisp" "modules/bndos/git/lisp/diff-review"
                 "modules/bndos/tasks/lisp"))
    (add-to-list 'load-path (expand-file-name dir doomdir))))
(setq byte-compile-warnings '(not obsolete docstrings))
