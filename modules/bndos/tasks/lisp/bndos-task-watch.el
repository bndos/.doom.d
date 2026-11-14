;;; bndos-task-watch.el --- Telling of agents waiting unseen -*- lexical-binding: t; -*-

;;; Commentary:

;; A task's agent asking for a permission waits until a client answers, and
;; only an attached agent-shell shows the request.  This polls `bndos-task
;; status' in every known project and tells you, once per wait, of a waiting
;; task no shell here is attached to.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bndos-task-store)
(require 'bndos-task-agent)
(require 'bndos-task-dashboard)

(defcustom bndos-task-watch-interval 30
  "Seconds between looks at the tasks' agents."
  :type 'number
  :group 'bndos-task)

(defvar bndos-task-watch--timer nil)

(defvar bndos-task-watch--told (make-hash-table :test #'equal)
  "Worktrees told of in their current wait.")

(defun bndos-task-watch--notify (text)
  "Say TEXT here and as a macOS notification."
  (message "%s" text)
  (when (fboundp 'ns-do-applescript)
    (ignore-errors
      (ns-do-applescript
       (format "display notification \"%s\" with title \"Tasks\""
               (replace-regexp-in-string "[\"\\]" "\\\\\\&" text))))))

(defun bndos-task-watch--consider (common statuses)
  "Tell of each waiting task in STATUSES (of repository COMMON) without a shell."
  (dolist (status statuses)
    (let ((worktree (file-name-as-directory (plist-get status :worktree))))
      (if (not (equal (plist-get status :state) "waiting"))
          (remhash worktree bndos-task-watch--told)
        (unless (or (gethash worktree bndos-task-watch--told)
                    (bndos-task-agent-buffer
                     (list :key (bndos-task-key common worktree) :worktree (list :path worktree)
                           :agent (list :session-id (plist-get status :session_id)))))
          (puthash worktree t bndos-task-watch--told)
          (bndos-task-watch--notify
           (format "%s waits for a permission no shell here shows: SPC g w w, then v on it"
                   (or (plist-get status :branch) (abbreviate-file-name worktree)))))))))

(defun bndos-task-watch-check ()
  "Look at every known project's agents once."
  (dolist (project (bndos-task-known-projects))
    (when-let* ((common (bndos-task--common-dir-sync project)))
      (bndos-task-cli-run
       '("status")
       (lambda (out)
         (unless (and (plistp out) (equal (plist-get out :status) "failed"))
           (bndos-task-watch--consider common out)))
       :directory project))))

;;;###autoload
(define-minor-mode bndos-task-watch-mode
  "Tell, once per wait, of a task's agent waiting for a permission no shell here shows."
  :global t
  :group 'bndos-task
  (when bndos-task-watch--timer
    (cancel-timer bndos-task-watch--timer)
    (setq bndos-task-watch--timer nil))
  (when bndos-task-watch-mode
    (setq bndos-task-watch--timer
          (run-at-time bndos-task-watch-interval bndos-task-watch-interval #'bndos-task-watch-check))))

(provide 'bndos-task-watch)
;;; bndos-task-watch.el ends here
