;;; bndos/git/autoload.el -*- lexical-binding: t; -*-

;;; pr-review comment navigation

(defun bndos-pr-review--comment-positions ()
  "Return sorted positions of inline threads, pending comments and review sections."
  (let (positions)
    (save-excursion
      (dolist (prop '(pr-review-eldoc-content pr-review-pending-review-thread))
        (goto-char (point-min))
        (while-let ((match (text-property-search-forward prop nil (lambda (_ v) v))))
          (push (prop-match-beginning match) positions)))
      (goto-char (point-min))
      (while-let ((match (text-property-search-forward
                          'magit-section nil
                          (lambda (_ v) (pr-review--review-thread-section-p v)))))
        (push (prop-match-beginning match) positions)
        ;; Don't re-match the section's interior.
        (goto-char (prop-match-end match))))
    (sort (delete-dups positions) #'<)))

(defun bndos-pr-review--skip-comment (pos direction)
  "Return a position past the comment at POS in DIRECTION (1 or -1)."
  (let ((section (get-text-property pos 'magit-section))
        (forward (> direction 0)))
    (cl-flet ((skip-prop (prop)
                (if forward
                    (or (next-single-property-change pos prop) pos)
                  (1- (or (previous-single-property-change pos prop) (1+ pos))))))
      (cond ((pr-review--review-thread-section-p section)
             (if forward (oref section end) (1- (oref section start))))
            ((get-text-property pos 'pr-review-pending-review-thread)
             (skip-prop 'pr-review-pending-review-thread))
            ((get-text-property pos 'pr-review-eldoc-content)
             (skip-prop 'pr-review-eldoc-content))
            (pos)))))

;;;###autoload
(defun bndos/pr-review-next-comment ()
  "Move to the next inline or review-section comment."
  (interactive)
  (let ((start (bndos-pr-review--skip-comment (point) 1)))
    (goto-char (or (seq-find (lambda (pos) (> pos start))
                             (bndos-pr-review--comment-positions))
                   (user-error "No next comment")))))

;;;###autoload
(defun bndos/pr-review-previous-comment ()
  "Move to the previous inline or review-section comment."
  (interactive)
  (let ((start (bndos-pr-review--skip-comment (point) -1)))
    (goto-char (or (car (last (seq-filter (lambda (pos) (< pos start))
                                          (bndos-pr-review--comment-positions))))
                   (user-error "No previous comment")))))
