;;; bndos/windows/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun bndos/window-move-to-side (side slot size)
  "Show the current buffer in a dedicated SIDE window at SLOT with SIZE."
  (interactive
   (list (intern (completing-read "Side: " '(top bottom left right) nil t))
         0 0.25))
  (let ((buffer (current-buffer))
        (window (selected-window)))
    ;; A buffer already shown in a side window (e.g. vterm via
    ;; `display-buffer-alist') would just be reused, so remove that window first.
    (set-window-dedicated-p window nil)
    (when (window-parameter window 'window-side)
      (delete-window window))
    (select-window
     (display-buffer-in-side-window
      buffer
      `((side . ,side)
        (slot . ,slot)
        (,(if (memq side '(left right)) 'window-width 'window-height) . ,size)
        (dedicated . side)
        (window-parameters . ((no-delete-other-windows . t))))))))

;;;###autoload
(defun bndos/workspace-other ()
  "Switch to the last workspace, or any other one if there's no history."
  (interactive)
  (if (bound-and-true-p +workspace--last)
      (+workspace/switch-to +workspace--last)
    (if-let* ((other (car (remove (+workspace-current-name)
                                  (+workspace-list-names)))))
        (+workspace-switch other)
      (+workspace-message "No other workspace" 'warn))))

;;;###autoload
(defun bndos-ediff-new-frame-h ()
  (select-frame (make-frame)))
