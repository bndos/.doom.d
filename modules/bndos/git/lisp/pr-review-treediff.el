;;; pr-review-treediff.el --- File tree sidebar for pr-review  -*- lexical-binding:t -*-

;;; Commentary:

;; Adds a left-side file tree panel to pr-review buffers using magit-treediff's
;; tree backend (builtin or treemacs, respecting `magit-treediff-tree-backend').
;; Selecting a file in the tree scrolls the pr-review buffer to that file's
;; diff section.

;;; Code:

(require 'pr-review)
(require 'magit-treediff)

;;; Buffer-local state

(defvar-local pr-review-treediff--pr-buffer nil
  "The pr-review buffer associated with this magit-treediff controller.
When non-nil on a controller buffer, file selection navigates the
pr-review buffer instead of rendering diff panes.")

;;; Controller setup

(defun pr-review-treediff--controller-name (pr-buf)
  (format " *pr-review-treediff: %s*" (buffer-name pr-buf)))

(defun pr-review-treediff--make-controller (pr-buf)
  "Create or refresh a magit-treediff controller buffer for PR-BUF."
  (let* ((name  (pr-review-treediff--controller-name pr-buf))
         (ctrl  (or (get-buffer name) (get-buffer-create name)))
         (pr-path (buffer-local-value 'pr-review--pr-path pr-buf)))
    (with-current-buffer ctrl
      (unless (derived-mode-p 'magit-treediff-mode)
        (magit-treediff-mode))
      (setq-local pr-review-treediff--pr-buffer pr-buf)
      (setq-local magit-treediff-source-kind 'range)
      (setq-local magit-treediff-range
                  (format "%s/%s  PR #%s"
                          (nth 0 pr-path) (nth 1 pr-path) (nth 2 pr-path)))
      (setq-local magit-treediff--model
                  (mapcar (lambda (f) (list :file f))
                          (with-current-buffer pr-buf
                            (pr-review--find-all-file-names)))))
    ctrl))

;;; Advice: intercept file selection on pr-review-treediff controllers

(defun pr-review-treediff--around-select-file (orig-fn file)
  "When called on a pr-review-treediff controller, navigate PR buffer instead."
  (if pr-review-treediff--pr-buffer
      (let ((pr-buf pr-review-treediff--pr-buffer))
        (setq magit-treediff-selected-file file)
        (magit-treediff--sync-tree)
        (when (buffer-live-p pr-buf)
          (when-let ((win (get-buffer-window pr-buf)))
            (with-selected-window win
              (pr-review--goto-section-with-value file))
            ;; Focus the diff window after navigation
            (select-window win))))
    (funcall orig-fn file)))

(defun pr-review-treediff--around-quit (orig-fn)
  "For pr-review-treediff controllers, just close the tree window and clean up."
  (if pr-review-treediff--pr-buffer
      (let ((pr-buf pr-review-treediff--pr-buffer))
        (when (buffer-live-p magit-treediff--tree-buffer)
          (when-let ((win (get-buffer-window magit-treediff--tree-buffer t)))
            (set-window-dedicated-p win nil)
            (delete-window win))
          (kill-buffer magit-treediff--tree-buffer))
        ;; Remove the sync hook from the pr-review buffer
        (when (buffer-live-p pr-buf)
          (with-current-buffer pr-buf
            (remove-hook 'post-command-hook
                         #'pr-review-treediff--sync-tree-to-point t)))
        (kill-buffer (current-buffer)))
    (funcall orig-fn)))

(advice-add 'magit-treediff--select-file :around #'pr-review-treediff--around-select-file)
(advice-add 'magit-treediff-quit         :around #'pr-review-treediff--around-quit)

;;; Advice: re-enable hl-line after treemacs setup disables it

(defun pr-review-treediff--after-tree-common-setup (controller)
  "Re-enable hl-line when tree buffer belongs to a pr-review-treediff controller."
  (when (buffer-local-value 'pr-review-treediff--pr-buffer controller)
    (hl-line-mode 1)))

(advice-add 'magit-treediff--tree-buffer-common-setup :after
            #'pr-review-treediff--after-tree-common-setup)

;;; Current-file tracking: sync tree highlight as user scrolls pr-review buffer

(defun pr-review-treediff--file-at-point ()
  "Return the diff file path at or above point in a pr-review buffer."
  (let ((section (magit-current-section)))
    (while (and section (not (magit-file-section-p section)))
      (setq section (oref section parent)))
    (when (magit-file-section-p section)
      (oref section value))))

(defun pr-review-treediff--sync-tree-to-point ()
  "Update tree selection to match the file visible at point."
  (when (eq major-mode 'pr-review-mode)
    (let* ((ctrl (get-buffer (pr-review-treediff--controller-name (current-buffer)))))
      (when (and ctrl
                 (buffer-live-p (buffer-local-value 'magit-treediff--tree-buffer ctrl))
                 (get-buffer-window
                  (buffer-local-value 'magit-treediff--tree-buffer ctrl)))
        (when-let ((file (pr-review-treediff--file-at-point)))
          (unless (equal file (buffer-local-value 'magit-treediff-selected-file ctrl))
            (with-current-buffer ctrl
              (setq magit-treediff-selected-file file)
              (magit-treediff--sync-tree))))))))

;;; Entry point

;;;###autoload
(defun pr-review-treediff-toggle ()
  "Toggle the file tree sidebar for the current pr-review buffer.
Respects `magit-treediff-tree-backend' (builtin or treemacs)."
  (interactive)
  (unless (eq major-mode 'pr-review-mode)
    (user-error "Not in a pr-review buffer"))
  (let* ((pr-buf  (current-buffer))
         (ctrl    (get-buffer (pr-review-treediff--controller-name pr-buf)))
         (tree-win (and ctrl
                        (buffer-live-p
                         (buffer-local-value 'magit-treediff--tree-buffer ctrl))
                        (get-buffer-window
                         (buffer-local-value 'magit-treediff--tree-buffer ctrl)))))
    (if tree-win
        ;; Already visible — close
        (with-current-buffer ctrl
          (magit-treediff-quit))
      ;; Open / refresh
      (let ((ctrl (pr-review-treediff--make-controller pr-buf)))
        (with-current-buffer ctrl
          (magit-treediff--display-tree
           (magit-treediff--ensure-tree-buffer)))
        ;; Track current file as user scrolls
        (with-current-buffer pr-buf
          (add-hook 'post-command-hook
                    #'pr-review-treediff--sync-tree-to-point nil t))))))

(provide 'pr-review-treediff)
;;; pr-review-treediff.el ends here
