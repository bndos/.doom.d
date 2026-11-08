;;; bndos/ui/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun bndos-ui-apply-cursor-color-h (&optional frame)
  "Apply `bndos-cursor-color' to FRAME, including terminal frames."
  (let ((frame (or frame (selected-frame))))
    (modify-frame-parameters frame `((cursor-color . ,bndos-cursor-color)))
    (set-face-background 'cursor bndos-cursor-color frame)
    (with-selected-frame frame
      (ignore-errors (set-cursor-color bndos-cursor-color))
      (unless (display-graphic-p)
        (send-string-to-terminal (format "\e]12;%s\a" bndos-cursor-color))))))

;;;###autoload
(defun bndos-ui-reset-terminal-cursor-color-h ()
  "Restore the terminal's own cursor color."
  (unless (display-graphic-p)
    (send-string-to-terminal "\e]112\a")))

;;;###autoload
(defun bndos-ui-apply-tty-separators-h (&optional frame)
  "Add a quiet horizontal cue between stacked windows in TTY frames.
TTY frames don't render bottom window dividers, so use a muted overline on the
mode-line instead."
  (dolist (frame (if frame (list frame) (frame-list)))
    (unless (display-graphic-p frame)
      (with-selected-frame frame
        (pcase-dolist (`(,face ,bg ,overline)
                       '((mode-line          "#060606" "#1b111c")
                         (mode-line-active   "#060606" "#241725")
                         (mode-line-inactive "#040404" "#1b111c")))
          (set-face-attribute face nil
                              :background bg
                              :foreground 'unspecified
                              :box nil
                              :overline overline))))))

;;;###autoload
(defun bndos-ui-tab-bar-persp-tabs ()
  "Return `tab-bar-format' items for the current frame's persp-mode workspaces."
  (when (bound-and-true-p persp-mode)
    (let ((current (safe-persp-name (get-current-persp))))
      (mapcan
       (lambda (name)
         (let ((active (string= name current)))
           `((,(intern (concat "persp-" name))
              menu-item
              ,(propertize (format (if active " [%s] " " %s ") name)
                           'face (if active 'tab-bar-tab 'tab-bar-tab-inactive))
              ,(lambda () (interactive) (persp-switch name))))))
       (persp-names-current-frame-fast-ordered)))))

;;;###autoload
(defun bndos/flash-buffer ()
  "Briefly highlight the selected window's buffer."
  (interactive)
  (let ((cookie (face-remap-add-relative 'default 'bndos-ui-flash-face)))
    (run-at-time 0.1 nil #'face-remap-remove-relative cookie)))
