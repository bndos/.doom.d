;;; bndos/ai/autoload.el -*- lexical-binding: t; -*-

;;; Copilot

;;;###autoload
(defun bndos-copilot-buffer-too-large-p ()
  "Non-nil when the buffer exceeds `copilot-max-char'."
  (and (natnump (bound-and-true-p copilot-max-char))
       (> (buffer-size) copilot-max-char)))

;;;###autoload
(defun bndos-copilot-maybe-enable-h ()
  "Enable Copilot here, unless toggled off globally or the buffer is too large."
  (when (and bndos-copilot-enabled
             (not (bndos-copilot-buffer-too-large-p)))
    (copilot-mode 1)
    (copilot-nes-mode 1)))

;;;###autoload
(defun bndos-copilot-disable-if-too-large-h ()
  (when (and copilot-mode (bndos-copilot-buffer-too-large-p))
    (copilot-mode -1)))

;;;###autoload
(defun bndos-copilot-add-doc-version-a (params)
  "Include the document version in Copilot inline completion PARAMS."
  (when-let* ((doc (plist-get params :textDocument)))
    (plist-put params :textDocument (plist-put doc :version copilot--doc-version)))
  params)

;;;###autoload
(defun bndos/copilot-toggle (&optional arg)
  "Toggle Copilot in all buffers.
With prefix ARG, enable if ARG is positive, otherwise disable."
  (interactive "P")
  (setq bndos-copilot-enabled (if arg
                                  (> (prefix-numeric-value arg) 0)
                                (not bndos-copilot-enabled)))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond ((not bndos-copilot-enabled)
             (when (bound-and-true-p copilot-mode) (copilot-mode -1)))
            ((derived-mode-p 'prog-mode 'org-mode 'markdown-mode 'gfm-mode)
             (bndos-copilot-maybe-enable-h)))))
  (message "Copilot %s globally" (if bndos-copilot-enabled "enabled" "disabled")))


;;; agent-shell

;;;###autoload
(defun bndos/agent-shell-ret-dwim ()
  "Run the RET binding of the button at point, or insert a newline."
  (interactive)
  (let* ((map (or (get-text-property (point) 'keymap)
                  (and (> (point) (point-min))
                       (get-text-property (1- (point)) 'keymap))))
         (cmd (and map (or (lookup-key map (kbd "RET"))
                           (lookup-key map (kbd "<return>"))))))
    (if (commandp cmd)
        (call-interactively cmd)
      (newline))))
