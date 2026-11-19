;;; bndos-review-workspace.el --- One review per workspace -*- lexical-binding: t; -*-

;;; Commentary:

;; A workspace holds at most one live review.  A review of commits or of a
;; branch, whose files come from a worktree of their own, opens in a
;; workspace of its own; so does any review opening where another one is
;; live.  Such a workspace is the review's: ending the review closes it,
;; and closing it ends the review.  A review of the worktree stays in the
;; workspace it is opened from.

;;; Code:

(require 'seq)

(declare-function +workspace-buffer-list "autoload/workspaces")
(declare-function +workspace-current-name "autoload/workspaces")
(declare-function +workspace-exists-p "autoload/workspaces")
(declare-function +workspace-kill "autoload/workspaces")
(declare-function +workspace-list-names "autoload/workspaces")
(declare-function +workspace-switch "autoload/workspaces")
(declare-function persp-name "persp-mode")
(declare-function +workspace-list "autoload/workspaces")
(defvar +workspace--last)
(defvar +workspaces-main)
(defvar persp-before-kill-functions)

(defvar-local bndos-review-workspace nil
  "In a review buffer, the workspace that is the review's own, or nil.")
(put 'bndos-review-workspace 'permanent-local t)

(defvar bndos-review-workspace--closing nil
  "Non-nil while a review and its workspace are being closed together.")

(defun bndos-review-workspace--available-p ()
  (and (bound-and-true-p persp-mode) (fboundp '+workspace-switch)))

(defun bndos-review-workspace-name (file)
  "The name of the workspace of review FILE: its path under review/, if any."
  (let* ((file (expand-file-name file))
         (under (and (string-match "/review/\\(.+\\)\\'" file) (match-string 1 file))))
    (concat "review " (file-name-sans-extension (or under (file-name-nondirectory file))))))

(defun bndos-review-workspace--reviews (&optional except)
  "The live reviews of the current workspace, but for the one visiting EXCEPT."
  (let ((except (and except (file-truename except))))
    (seq-filter (lambda (b)
                  (and (buffer-live-p b)
                       (provided-mode-derived-p (buffer-local-value 'major-mode b) 'diff-review-mode)
                       (not (and except (buffer-file-name b)
                                 (equal (file-truename (buffer-file-name b)) except)))))
                (+workspace-buffer-list))))

(defun bndos-review-workspace-prepare (file own)
  "Switch to where review FILE belongs, before it is shown; return the
workspace that is its own, or nil.  With OWN, or when the current workspace
has another review live, that is a workspace of its own."
  (when (bndos-review-workspace--available-p)
    (let* ((buffer (find-buffer-visiting file))
           (home (and buffer (buffer-local-value 'bndos-review-workspace buffer))))
      (cond
       ((and home (+workspace-exists-p home))
        (+workspace-switch home)
        home)
       ;; Open in some workspace already: there, not in a second.
       ((and buffer (not (memq buffer (+workspace-buffer-list))))
        (when-let* ((there (seq-find (lambda (persp) (memq buffer (+workspace-buffer-list persp)))
                                     (+workspace-list))))
          (+workspace-switch (persp-name there)))
        nil)
       ((or own (bndos-review-workspace--reviews file))
        (let ((name (bndos-review-workspace-name file)))
          (+workspace-switch name t)
          name))))))

(defun bndos-review-workspace-claim (buffer workspace)
  "Make WORKSPACE review BUFFER's own: each closes with the other."
  (when (and workspace (buffer-live-p buffer))
    (with-current-buffer buffer
      (setq bndos-review-workspace workspace)
      ;; Last: the review ends, its worktree removed, before its workspace goes.
      (add-hook 'kill-buffer-hook #'bndos-review-workspace--review-killed t t))))

(defun bndos-review-workspace--review-killed ()
  "Close the workspace of the review being killed."
  (when-let* ((name bndos-review-workspace)
              ((not bndos-review-workspace--closing))
              ((bndos-review-workspace--available-p))
              ((+workspace-exists-p name)))
    (let ((bndos-review-workspace--closing t))
      (when (equal (+workspace-current-name) name)
        (+workspace-switch (seq-find (lambda (w) (not (equal w name)))
                                     (cons +workspace--last (+workspace-list-names))
                                     +workspaces-main)
                           t))
      ;; Its other buffers are the user's to keep; the review's own go with it.
      (+workspace-kill name t))))

(defun bndos-review-workspace--workspace-killed (persp)
  "End the review whose workspace PERSP is being closed."
  (unless bndos-review-workspace--closing
    (let ((name (and persp (persp-name persp)))
          (bndos-review-workspace--closing t))
      (dolist (buffer (buffer-list))
        (when (and name (equal (buffer-local-value 'bndos-review-workspace buffer) name))
          (kill-buffer buffer))))))

(defun bndos-review-workspace-find-file-a (fn file &optional root base head)
  "Open review FILE where it belongs (around `diff-review-find-file')."
  (let ((workspace (bndos-review-workspace-prepare file head)))
    (prog1 (funcall fn file root base head)
      (bndos-review-workspace-claim (find-buffer-visiting file) workspace))))

(advice-add 'diff-review-find-file :around #'bndos-review-workspace-find-file-a)
(add-hook 'persp-before-kill-functions #'bndos-review-workspace--workspace-killed)

(provide 'bndos-review-workspace)
;;; bndos-review-workspace.el ends here
