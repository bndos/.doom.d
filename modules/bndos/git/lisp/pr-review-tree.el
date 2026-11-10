;;; pr-review-tree.el --- File tree beside pr-review buffers  -*- lexical-binding:t -*-

;;; Commentary:

;; diff-review's sidebar beside a pr-review buffer, listing its files.

;;; Code:

(require 'pr-review)
(require 'diff-review)

(defun pr-review-tree--file-at-point ()
  "Return the diff file path at or above point in a pr-review buffer."
  (let ((section (magit-current-section)))
    (while (and section (not (magit-file-section-p section)))
      (setq section (oref section parent)))
    (when (magit-file-section-p section)
      (oref section value))))

(defun pr-review-tree--files ()
  "The pull request's files, with what its data says of each."
  (let ((known (make-hash-table :test #'equal)))
    (let-alist pr-review--pr-info
      (seq-doseq (node .files.nodes)
        (let-alist node
          (when .path (puthash .path node known)))))
    (mapcar (lambda (path)
              (let-alist (gethash path known)
                (list :path path
                      :status (pcase .changeType ("ADDED" "A") ("DELETED" "D") ("RENAMED" "R") ("MODIFIED" "M"))
                      :added .additions :removed .deletions)))
            (pr-review--find-all-file-names))))

(defun pr-review-tree--refresh-a (&rest _)
  "Reread the files of a pr-review buffer showing its tree, after a refresh."
  (when diff-review-host (diff-review--host-refresh)))

;;;###autoload
(defun pr-review-tree-toggle ()
  "Show or hide the file tree beside this pr-review buffer."
  (interactive)
  (unless (derived-mode-p 'pr-review-mode)
    (user-error "Not in a pr-review buffer"))
  (advice-add 'pr-review-refresh :after #'pr-review-tree--refresh-a)
  (let ((path pr-review--pr-path))
    (diff-review-host-sidebar
     :title (format "%s/%s #%s" (nth 0 path) (nth 1 path) (nth 2 path))
     :files #'pr-review-tree--files
     :open #'pr-review--goto-section-with-value
     :current #'pr-review-tree--file-at-point)))

(provide 'pr-review-tree)
;;; pr-review-tree.el ends here
