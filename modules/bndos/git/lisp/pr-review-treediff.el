;;; pr-review-treediff.el --- File tree sidebar for pr-review  -*- lexical-binding:t -*-

;;; Commentary:

;; Adds a left-side file tree panel to pr-review buffers using magit-treediff's
;; tree backend (builtin or treemacs, respecting `magit-treediff-tree-backend').
;; Selecting a file in the tree scrolls the pr-review buffer to that file's
;; diff section, and moving through the review keeps the tree selection in sync.
;; The mechanics live in `magit-treediff-follow'.

;;; Code:

(require 'pr-review)
(require 'magit-treediff)

(defun pr-review-treediff--file-at-point ()
  "Return the diff file path at or above point in a pr-review buffer."
  (let ((section (magit-current-section)))
    (while (and section (not (magit-file-section-p section)))
      (setq section (oref section parent)))
    (when (magit-file-section-p section)
      (oref section value))))

(defun pr-review-treediff--follow (pr-buf)
  "Register PR-BUF's file list with `magit-treediff-follow'."
  (let ((pr-path (buffer-local-value 'pr-review--pr-path pr-buf)))
    (magit-treediff-follow
     pr-buf
     :title (format "%s/%s  PR #%s" (nth 0 pr-path) (nth 1 pr-path) (nth 2 pr-path))
     :files (mapcar (lambda (file) (list :file file))
                    (with-current-buffer pr-buf
                      (pr-review--find-all-file-names)))
     :goto-file #'pr-review--goto-section-with-value
     :file-at-point #'pr-review-treediff--file-at-point)))

;;;###autoload
(defun pr-review-treediff-toggle ()
  "Toggle the file tree sidebar for the current pr-review buffer.
Respects `magit-treediff-tree-backend' (builtin or treemacs)."
  (interactive)
  (unless (eq major-mode 'pr-review-mode)
    (user-error "Not in a pr-review buffer"))
  (if (magit-treediff-follow-visible-p (current-buffer))
      (magit-treediff-follow-close (current-buffer))
    (pr-review-treediff--follow (current-buffer))
    (magit-treediff-follow-show (current-buffer))))

(provide 'pr-review-treediff)
;;; pr-review-treediff.el ends here
