;;; pr-review-worktree.el --- Worktree integration for pr-review  -*- lexical-binding:t -*-

;;; Commentary:

;; Adds git worktree support to pr-review: open a PR's head branch in a
;; dedicated worktree and switch to it as a new projectile/persp workspace.

;;; Code:

(require 'pr-review)

;;; Worktree integration

(defcustom pr-review-worktree-base-directory nil
  "Base directory for creating PR worktrees.
When nil, worktrees are created as siblings of the project root,
named <repo-name>-pr-<pr-id>.  When set to a directory path,
worktrees are created inside that directory instead."
  :type '(choice (const :tag "Sibling of project root" nil)
                 (directory :tag "Custom base directory"))
  :group 'pr-review)

(defun pr-review--find-local-project (repo-owner repo-name)
  "Find the local project root for REPO-OWNER/REPO-NAME.
Searches projectile's known projects for a match by directory name.
Falls back to prompting the user if no match or multiple matches."
  (unless (fboundp 'projectile-known-projects)
    (user-error "Projectile is required for worktree integration"))
  (let* ((matching (seq-filter
                    (lambda (p)
                      (and (file-directory-p p)
                           (string= (file-name-nondirectory
                                     (directory-file-name p))
                                    repo-name)))
                    projectile-known-projects)))
    (cond
     ((= (length matching) 1)
      (car matching))
     ((> (length matching) 1)
      (completing-read
       (format "Select project for %s/%s: " repo-owner repo-name)
       matching nil t))
     (t
      (read-directory-name
       (format "No known project for '%s/%s'. Select local clone: "
               repo-owner repo-name))))))

(defun pr-review--worktree-path (project-root repo-name pr-id)
  "Return the path where the PR worktree should live.
Uses `pr-review-worktree-base-directory' if set, otherwise
creates a sibling directory of PROJECT-ROOT named <REPO-NAME>-pr-<PR-ID>."
  (let ((name (format "%s-pr-%s" repo-name pr-id)))
    (if pr-review-worktree-base-directory
        (expand-file-name name pr-review-worktree-base-directory)
      (expand-file-name (concat "../" name) project-root))))

(defun pr-review--git-worktree-add (project-root worktree-path branch pr-id)
  "Create a git worktree at WORKTREE-PATH for BRANCH from PROJECT-ROOT.
PR-ID is used to name the local tracking branch (pr-<PR-ID>).
Returns t on success, signals an error on failure."
  (let ((default-directory project-root)
        (wt-dir (directory-file-name worktree-path)))
    (call-process "git" nil nil nil "fetch" "origin" branch)
    (let* ((local-branch (format "pr-%s" pr-id))
           (exit (call-process "git" nil nil nil
                               "worktree" "add"
                               "--track" "-b" local-branch
                               wt-dir
                               (format "origin/%s" branch))))
      (when (not (= exit 0))
        (let ((exit2 (call-process "git" nil nil nil
                                   "worktree" "add" wt-dir branch)))
          (unless (= exit2 0)
            (user-error "Failed to create worktree for branch '%s'" branch)))))
    t))

;;;###autoload
(defun pr-review-open-worktree ()
  "Open a git worktree for the current PR's branch.

Creates a worktree (if none exists) for the PR's head branch inside a
local clone of the repository, then switches to it as a new projectile
project.  In Doom Emacs this automatically opens a new persp workspace.

The PR review buffer's `default-directory' is updated to the worktree so
that magit commands operate on the right repository.

See `pr-review-worktree-base-directory' to configure where worktrees live."
  (interactive)
  (unless (eq major-mode 'pr-review-mode)
    (user-error "Not in a pr-review buffer"))
  (unless pr-review--pr-info
    (user-error "PR info not loaded yet — try refreshing first"))
  (let* ((repo-owner (nth 0 pr-review--pr-path))
         (repo-name  (nth 1 pr-review--pr-path))
         (pr-id      (nth 2 pr-review--pr-path))
         (branch     (let-alist pr-review--pr-info .headRefName))
         (pr-buf     (current-buffer)))
    (unless branch
      (user-error "Could not determine the head branch name for this PR"))
    (let* ((project-root  (file-name-as-directory
                           (expand-file-name
                            (pr-review--find-local-project repo-owner repo-name))))
           (worktree-path (file-name-as-directory
                           (expand-file-name
                            (pr-review--worktree-path project-root repo-name pr-id)))))
      (if (file-directory-p worktree-path)
          (message "Reusing existing worktree at %s" worktree-path)
        (message "Creating worktree for %s (PR #%s)..." branch pr-id)
        (pr-review--git-worktree-add project-root worktree-path branch pr-id)
        (message "Worktree created at %s" worktree-path))
      (projectile-add-known-project worktree-path)
      (projectile-switch-project-by-name worktree-path)
      (when (buffer-live-p pr-buf)
        (with-current-buffer pr-buf
          (setq-local default-directory worktree-path))))))

(provide 'pr-review-worktree)
;;; pr-review-worktree.el ends here
