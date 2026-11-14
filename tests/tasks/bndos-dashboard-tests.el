;;; bndos-dashboard-tests.el --- Dashboard: gone tasks, forget and restore, agent here, the watch -*- lexical-binding: t; -*-

(require 'test-helper)
(require 'bndos-review-tests)
(require 'bndos-task-watch)

(defun bndos-test-dash-row (key branch base &rest props)
  "Save a task record KEY on BRANCH in project /r/ and return its dashboard row.
PROPS are :session (its session id), :threads and the row's own keys."
  (bndos-task-store-save
   (list :key key :branch branch :repo (list :main-path "/r/" :project "proj")
         :worktree (list :path (concat "/r." branch "/") :handle branch)
         :threads (plist-get props :threads)
         :agent (and (plist-get props :session) (list :provider "claude-code" :session-id (plist-get props :session)))))
  (append (list :key key :record (bndos-task-store-load key) :base base :worktree nil)
          (bndos-task-store--without props '(:session :threads))))

(defmacro bndos-test-with-dashboard (rows &rest body)
  "Run BODY in a dashboard buffer showing ROWS."
  (declare (indent 1))
  `(let ((bndos-task-agent--hydra (make-hash-table :test #'equal))
         (bndos-task-agent--shells (make-hash-table :test #'equal)))
     (let ((bndos-task--rows ,rows)
           (bndos-task--problems nil))
       (with-temp-buffer
         (bndos-task-dashboard-mode)
         (bndos-task--print)
         ,@body))))

(defun bndos-test-dash-lines ()
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))

(defun bndos-test-dash-gone-section ()
  (car (bndos-task--sections 'bndos-gone)))

(defun bndos-test-remove-worktree (repo wt)
  (bndos-test-git repo "worktree" "remove" "--force" (directory-file-name wt)))


;;; The tree and the gone section

(ert-deftest bndos-dash-tree-hides-gone-tasks-and-marks-gone-parents ()
  (bndos-test-with-store
    (bndos-test-with-dashboard
        (list (bndos-test-dash-row "kb" "layer-b" "layer-a")
              (bndos-test-dash-row "ka" "layer-a" "main" :missing t :branch-state "kept" :session "hydra_session_a")
              (bndos-test-dash-row "kz" "bare" "main" :missing t :branch-state "deleted"))
      (let ((lines (bndos-test-dash-lines)))
        (should (equal (nth 0 lines) "proj  /r/"))
        (should (equal (nth 1 lines) "  layer-a (gone)"))
        (should (string-match-p "^  └─ layer-b +· no agent" (nth 2 lines)))
        (should (seq-find (lambda (l) (equal l "  gone (1)")) lines))
        ;; Neither threads nor a session: not shown at all.
        (should-not (seq-find (lambda (l) (string-match-p "bare" l)) lines)))
      ;; The gone task is only in the gone section, folded.
      (should (oref (bndos-test-dash-gone-section) hidden))
      (should (eq (oref (bndos-task--section-for "ka") parent) (bndos-test-dash-gone-section)))
      (should (string-match-p "1 tasks" (format "%s" header-line-format)))
      (should (string-match-p "x forget" (format "%s" header-line-format))))))

(ert-deftest bndos-dash-gone-section-says-whether-the-branch-was-kept ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((kept (bndos-test-task repo "kept" "main"))
             (dropped (bndos-test-task repo "dropped" "main"))
             (bare (bndos-test-task repo "bare" "main")))
        (bndos-test-add-thread (car kept) "keep me")
        (bndos-test-add-thread (car dropped) "me too")
        (dolist (task (list kept dropped bare)) (bndos-test-remove-worktree repo (cdr task)))
        (bndos-test-git repo "branch" "-D" "dropped")
        (let ((rows (bndos-task--rows nil (make-hash-table :test #'equal))))
          (bndos-test-await (lambda (cb) (bndos-task--check-gone-branches rows cb)))
          (should-not (seq-find (lambda (r) (equal (plist-get r :key) (car bare))) rows))
          (bndos-test-with-dashboard rows
            (magit-section-show (bndos-test-dash-gone-section))
            (let ((lines (bndos-test-dash-lines)))
              (should (member "  gone (2)" lines))
              (should (seq-find (lambda (l) (string-match-p "^    dropped +branch deleted +1 open" l)) lines))
              (should (seq-find (lambda (l) (string-match-p "^    kept +branch kept +1 open" l)) lines)))))))))

(ert-deftest bndos-dash-an-open-gone-section-stays-open ()
  (bndos-test-with-store
    (bndos-test-with-dashboard
        (list (bndos-test-dash-row "kb" "fix" "main")
              (bndos-test-dash-row "ka" "old" "main" :missing t :branch-state "kept" :session "hydra_session_a"))
      (should (oref (bndos-test-dash-gone-section) hidden))
      (bndos-task--print)
      (should (oref (bndos-test-dash-gone-section) hidden))
      (magit-section-show (bndos-test-dash-gone-section))
      (bndos-task--print)
      (should-not (oref (bndos-test-dash-gone-section) hidden))
      (goto-char (bndos-task--heading-position "ka"))
      (should-not (invisible-p (point))))))


;;; Forget and restore

(ert-deftest bndos-dash-forget-moves-the-record-aside ()
  (bndos-test-with-store
    (bndos-test-with-dashboard
        (list (bndos-test-dash-row "ka" "old" "main" :missing t :branch-state "kept" :session "hydra_session_a"))
      (goto-char (bndos-task--heading-position "ka"))
      (let (asked)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) t))
                  ((symbol-function 'bndos-task-dashboard-refresh) #'ignore))
          (bndos/task-dashboard-forget))
        (should (equal asked '("Forget old? "))))
      (should-not (file-exists-p (bndos-task-store-file "ka")))
      (should (equal (plist-get (bndos-task-store--read-json-file
                                 (file-name-concat bndos-task-store-directory "removed" "ka.json"))
                                :branch)
                     "old"))
      (should-not (bndos-task-store-all)))
    ;; A task whose worktree is there isn't gone.
    (bndos-task-store-save (list :key "kl" :branch "live" :worktree (list :path temporary-file-directory)))
    (should-error (bndos-task-forget "kl") :type 'user-error)
    (should (file-exists-p (bndos-task-store-file "kl")))))

(ert-deftest bndos-dash-restore-brings-the-worktree-back ()
  (skip-unless (bndos-test-wt-available-p))
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((task (bndos-test-task repo "back" "main"))
             (live (bndos-test-task repo "live" "main")))
        (bndos-test-add-thread (car task) "still here")
        (bndos-test-remove-worktree repo (cdr task))
        (should-error (bndos-task-restore (car live) #'ignore) :type 'user-error)
        (let* ((key (bndos-test-await (lambda (cb) (bndos-task-restore (car task) cb)) 60))
               (record (bndos-task-store-load key)))
          (should (stringp key))
          (should (file-directory-p (bndos-task-get record :worktree :path)))
          (should (equal (bndos-test-git (bndos-task-get record :worktree :path) "symbolic-ref" "--short" "HEAD")
                         "back"))
          (should (equal (mapcar (lambda (th) (plist-get th :body)) (plist-get record :threads)) '("still here")))
          (should (equal (bndos-task-store-load key)
                         (seq-find (lambda (r) (equal (plist-get r :branch) "back")) (bndos-task-store-all))))
          (should (= (seq-count (lambda (r) (equal (plist-get r :branch) "back")) (bndos-task-store-all)) 1)))))))


;;; The agent here

(ert-deftest bndos-dash-agent-here-offers-to-start-an-agent ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((task (bndos-test-task repo "ag" "main"))
             (default-directory (cdr task))
             (bndos-task-agent--shells (make-hash-table :test #'equal))
             (bndos-task-agent--hydra (make-hash-table :test #'equal)))
        (cl-flet ((run (status answer)
                    (let (asked opened done)
                      (cl-letf (((symbol-function 'bndos-task-cli-run)
                                 (lambda (_args callback &rest _) (funcall callback status) (setq done t)))
                                ((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) answer))
                                ((symbol-function 'bndos-task-agent-open) (lambda (key) (push key opened))))
                        (bndos/task-agent-here)
                        (bndos-proc-wait (lambda () done) 20))
                      (list asked opened))))
          (should (equal (run '(:branch "ag" :state "none") t)
                         (list '("No agent on ag: start one?") (list (car task)))))
          (should (equal (run '(:branch "ag" :state "none") nil)
                         (list '("No agent on ag: start one?") nil)))
          (should (equal (run '(:branch "ag" :session_id "hydra_session_x" :state "idle") t)
                         (list nil (list (car task))))))))))


;;; The watch

(ert-deftest bndos-dash-watch-tells-once-per-wait ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((wt (bndos-test-worktree repo "w1" "main"))
             (key (bndos-task-key (bndos-task--common-dir-sync repo) wt))
             (bndos-task-projects (list repo))
             (bndos-task-watch--told (make-hash-table :test #'equal))
             (bndos-task-agent--shells (make-hash-table :test #'equal))
             (state "waiting")
             told)
        (cl-letf (((symbol-function 'bndos-task-cli-run)
                   (lambda (args callback &rest _)
                     (should (equal args '("status")))
                     (funcall callback (list (list :branch "w1" :worktree (directory-file-name wt)
                                                   :session_id "hydra_session_w" :state state)))))
                  ((symbol-function 'bndos-task-watch--notify) (lambda (text) (push text told))))
          (bndos-task-watch-check)
          (should (equal told '("w1 waits for a permission no shell here shows: SPC g w w, then v on it")))
          (bndos-task-watch-check)
          (should (= (length told) 1))
          (setq state "working")
          (bndos-task-watch-check)
          (setq state "waiting")
          (bndos-task-watch-check)
          (should (= (length told) 2))
          ;; A shell here shows the request itself.
          (setq state "idle")
          (bndos-task-watch-check)
          (bndos-test-with-fake-shells ((key shell 'blocked "hydra_session_w"))
            (setq state "waiting")
            (bndos-task-watch-check)
            (should (= (length told) 2))))
        (unwind-protect
            (progn (bndos-task-watch-mode 1)
                   (should (timerp bndos-task-watch--timer)))
          (bndos-task-watch-mode -1))
        (should-not bndos-task-watch--timer)))))

;; diff-review checks a commit out under <git dir>/review/worktrees to show
;; its files; that worktree is the review's, never a task.
(ert-deftest bndos-dash-review-worktrees-are-not-tasks ()
  (skip-unless (bndos-test-wt-available-p))
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((wt (bndos-test-worktree repo "layer-a" "main"))
             (common (bndos-test-git repo "rev-parse" "--path-format=absolute" "--git-common-dir"))
             (own (expand-file-name "review/worktrees/abc123" common))
             (bndos-task-projects (list repo)))
        (make-directory (file-name-directory own) t)
        (bndos-test-git repo "worktree" "add" "--quiet" "--detach" own "HEAD")
        (should (bndos-task--review-worktree-p common own))
        (should-not (bndos-task--review-worktree-p common wt))
        (pcase-let ((`(,rows ,_) (bndos-test-await #'bndos-task--inventory)))
          (should (seq-find (lambda (r) (equal (bndos-task-get r :record :branch) "layer-a")) rows))
          (should-not (seq-find (lambda (r) (string-match-p "review/worktrees" (or (bndos-task-get r :worktree :path) "")))
                                rows)))
        (should-not (seq-find (lambda (r) (string-match-p "review/worktrees" (or (bndos-task-get r :worktree :path) "")))
                              (bndos-task-store-all)))
        (let ((all (bndos-test-cli repo "status")))
          (should (eql (car all) 0))
          (should (equal (mapcar (lambda (s) (plist-get s :branch)) (cdr all)) '("layer-a"))))))))

(ert-deftest bndos-dash-dwim-takes-its-base-from-the-hook ()
  (bndos-test-with-repo repo
    (bndos-test-git repo "config" "branch.main.bndos-base" "origin/dev")
    (should (equal (bndos-task-git-saved-base-sync repo "main") "origin/dev"))
    (should-not (bndos-task-git-saved-base-sync repo "other"))
    (should-not (bndos-task-git-saved-base-sync repo nil))))

(provide 'bndos-dashboard-tests)
;;; bndos-dashboard-tests.el ends here
