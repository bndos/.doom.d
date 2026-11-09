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


;;; GitHub identity

;; gh resolves the account from the environment, which envrc sets per buffer,
;; so the token is looked up with the relevant buffer's environment.

(defun bndos-gh--env-buffer ()
  "Return the buffer whose environment decides the GitHub identity here.
That is the current buffer if it has a project environment, otherwise the
most recent buffer in the current workspace that has one."
  (if (local-variable-p 'process-environment)
      (current-buffer)
    (when (and (bound-and-true-p persp-mode) (fboundp '+workspace-buffer-list))
      (let ((ws-buffers (+workspace-buffer-list)))
        (cl-loop for buf in (buffer-list)
                 when (and (memq buf ws-buffers)
                           (local-variable-p 'process-environment buf))
                 return buf)))))

;;;###autoload
(defun bndos-gh-token (&optional host)
  "Return the token `gh' resolves for HOST (default github.com) in this context."
  (let ((env (buffer-local-value 'process-environment
                                 (or (bndos-gh--env-buffer) (current-buffer)))))
    (with-temp-buffer
      ;; Bound here, where it isn't buffer-local, so the subprocess sees it.
      (let ((process-environment env))
        (when (zerop (call-process "gh" nil '(t nil) nil
                                   "auth" "token" "--hostname" (or host "github.com")))
          (let ((token (string-trim (buffer-string))))
            (unless (string-empty-p token) token)))))))

;;;###autoload
(defun bndos/gh-whoami ()
  "Show which GitHub account the current context resolves to."
  (interactive)
  (let ((env-buf (bndos-gh--env-buffer))
        (token (or (bndos-gh-token) (user-error "gh has no token for this context"))))
    (with-temp-buffer
      (let ((process-environment (cons (concat "GH_TOKEN=" token) process-environment)))
        (message "GitHub: %s (environment of %s)"
                 (if (zerop (call-process "gh" nil '(t nil) nil "api" "user" "--jq" ".login"))
                     (string-trim (buffer-string))
                   "token rejected")
                 (if env-buf
                     (abbreviate-file-name (buffer-local-value 'default-directory env-buf))
                   "Emacs, i.e. gh's active account"))))))

;;;###autoload
(defun bndos-pr-review-gh-auth-a (args)
  "Authenticate pr-review's GitHub requests with the token gh resolves here."
  (if-let* (((memq (plist-get args :forge) '(nil github)))
            (token (bndos-gh-token (bound-and-true-p pr-review--host))))
      (plist-put (copy-sequence args) :auth token)
    args))
