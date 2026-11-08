;;; bndos/editor/autoload.el -*- lexical-binding: t; -*-

;;; Motion

;;;###autoload
(defun bndos/evilem-previous-line ()
  "`evilem-motion-previous-line', recording a jump point first."
  (interactive)
  (better-jumper-set-jump)
  (evilem-motion-previous-line))

;;;###autoload
(defun bndos/evilem-next-line ()
  "`evilem-motion-next-line', recording a jump point first."
  (interactive)
  (better-jumper-set-jump)
  (evilem-motion-next-line))

;;;###autoload
(defun bndos/scroll-down-lines ()
  "Scroll the view and point down 4 lines."
  (interactive)
  (scroll-up 4)
  (setq this-command 'next-line)
  (forward-line 4))

;;;###autoload
(defun bndos/scroll-up-lines ()
  "Scroll the view and point up 4 lines."
  (interactive)
  (scroll-down 4)
  (setq this-command 'previous-line)
  (forward-line -4))


;;; Clipboard

;;;###autoload
(defun bndos/insert-primary-selection ()
  "Insert the PRIMARY selection, like a middle click."
  (interactive)
  (insert (gui-get-primary-selection)))

(defun bndos-clipboard--backend ()
  (cond ((and (getenv "WAYLAND_DISPLAY")
              (executable-find "wl-copy")
              (executable-find "wl-paste"))
         'wayland)
        ((and (getenv "DISPLAY") (executable-find "xclip"))
         'x11)))

(defun bndos-clipboard-copy (text)
  (pcase (bndos-clipboard--backend)
    ('wayland (with-temp-buffer
                (insert text)
                (call-process-region (point-min) (point-max) "wl-copy")))
    ('x11 (with-temp-buffer
            (insert text)
            (call-process-region (point-min) (point-max) "xclip"
                                 nil nil nil "-selection" "clipboard"))))
  text)

(defun bndos-clipboard-paste ()
  (with-temp-buffer
    (when (zerop (pcase (bndos-clipboard--backend)
                   ('wayland (call-process "wl-paste" nil t nil "--no-newline"))
                   ('x11 (call-process "xclip" nil t nil "-selection" "clipboard" "-o"))
                   (_ 1)))
      (buffer-string))))

;;;###autoload
(defun bndos-clipboard-enable-h ()
  "Route kills and yanks through wl-copy/xclip in terminal Emacs."
  (when (and (not (equal (getenv "BNDOS_TERMINAL_CLIPBOARD") "0"))
             (not (display-graphic-p))
             (bndos-clipboard--backend))
    (setq select-enable-clipboard t
          interprogram-cut-function #'bndos-clipboard-copy
          interprogram-paste-function #'bndos-clipboard-paste)))
