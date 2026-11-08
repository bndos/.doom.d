;;; bndos/lsp/autoload/eldoc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun bndos-eldoc-disable-echo-area-h ()
  "Keep Eldoc from writing documentation into the echo area."
  (setq-local eldoc-display-functions
              (remq #'eldoc-display-in-echo-area eldoc-display-functions)))

;;;###autoload
(defun bndos/eldoc-box-help-at-point ()
  "Show documentation at point in an Eldoc child frame without focusing it."
  (interactive)
  (require 'eldoc-box)
  (bndos-eldoc-disable-echo-area-h)
  (when (get-buffer " *eldoc-box*")
    (ignore-errors (eldoc-box-quit-frame)))
  (add-hook 'eldoc-display-functions #'eldoc-box--help-at-point-async-update 0 t)
  (eldoc-box-help-at-point)
  (eldoc-print-current-symbol-info t))
