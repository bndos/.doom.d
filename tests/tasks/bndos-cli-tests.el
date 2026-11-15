;;; bndos-cli-tests.el --- bin/bndos-task: start on an existing task, adopt guards -*- lexical-binding: t; -*-

(require 'test-helper)
(require 'bndos-review-tests)

(defmacro bndos-test-with-stub-hydra (&rest body)
  "Run BODY with a private store against the test daemon, as the integration tests do."
  (declare (indent 0))
  `(progn
     (skip-unless (and (getenv "BNDOS_TEST_HYDRA") (getenv "HYDRA_ACP_HOME") (bndos-test-wt-available-p)))
     (bndos-test-with-store ,@body)))

(defun bndos-test-task-config (dir)
  (bndos-test-git dir "config" "--get-regexp" "^branch\\..*\\.bndos-"))

(ert-deftest bndos-cli-adopt-refuses-a-hydra-session-id ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((wt (bndos-test-worktree repo "old" "main"))
             (before (bndos-test-task-config repo))
             (out (bndos-test-cli wt "adopt" wt "--agent" "bndos-stub" "--session" "hydra_session_abc")))
        (should (eql (car out) 1))
        (should (string-match-p "Hydra session" (plist-get (cdr out) :error)))
        (should (equal (bndos-test-task-config repo) before))))))

(ert-deftest bndos-cli-start-needs-a-base-for-a-new-branch ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let ((out (bndos-test-cli repo "start" "--agent" "bndos-stub" "fresh")))
        (should (eql (car out) 1))
        (should (string-match-p "--base" (plist-get (cdr out) :error)))
        (should (equal (bndos-test-git repo "branch" "--list" "fresh") ""))))))

(ert-deftest bndos-cli-start-refuses-a-task-that-has-an-agent ()
  (bndos-test-with-stub-hydra
    (bndos-test-with-repo repo
      (should (eql (car (bndos-test-cli repo "start" "--base" "main" "--agent" "bndos-stub" "busy")) 0))
      (let ((sid (bndos-test-git repo "config" "branch.busy.bndos-session"))
            (out (bndos-test-cli repo "start" "--agent" "bndos-stub" "busy")))
        (should (eql (car out) 1))
        (should (string-match-p "already has an agent" (plist-get (cdr out) :error)))
        (should (equal (bndos-test-git repo "config" "branch.busy.bndos-session") sid))))))

(ert-deftest bndos-cli-start-on-a-task-without-an-agent-starts-it-there ()
  (bndos-test-with-stub-hydra
    (bndos-test-with-repo repo
      (bndos-test-worktree repo "lower" "main")
      (let* ((wt (bndos-test-worktree repo "upper" "lower"))
             (trees (bndos-test-git repo "worktree" "list"))
             (branches (bndos-test-git repo "branch" "--list")))
        ;; Its recorded session is gone from Hydra.
        (bndos-test-git repo "config" "branch.upper.bndos-session" "hydra_session_gone")
        (let ((out (cdr (bndos-test-cli repo "start" "--agent" "bndos-stub" (directory-file-name wt)))))
          (should (equal (plist-get out :status) "started"))
          (should (eq (plist-get out :existing) t))
          (should (equal (plist-get out :branch) "upper"))
          (should (equal (file-name-as-directory (plist-get out :worktree)) wt))
          (should (equal (plist-get out :parent) "lower"))
          (should (equal (bndos-test-git repo "worktree" "list") trees))
          (should (equal (bndos-test-git repo "branch" "--list") branches))
          (should (equal (bndos-test-git repo "config" "branch.upper.bndos-base") "lower"))
          (let ((sid (plist-get out :session_id)))
            (should (string-prefix-p "hydra_session_" sid))
            (should (equal (bndos-test-git repo "config" "branch.upper.bndos-session") sid))
            (should (member (plist-get (cdr (bndos-test-cli repo "status" "upper")) :state) '("idle" "working")))))))))

;;; Project folders: the repo inside a folder that has .tasks

(defun bndos-test-project-folder (repo)
  "Make REPO's parent its project folder; return it."
  (let ((folder (file-name-directory (directory-file-name repo))))
    (make-directory (expand-file-name ".tasks" folder) t)
    folder))

(ert-deftest bndos-cli-context-names-the-project-folder ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let ((out (cdr (bndos-test-cli repo "context"))))
        (should-not (plist-get out :container))
        (should-not (plist-get out :context)))
      (should-not (bndos-task-git-container repo))
      (let* ((folder (bndos-test-project-folder repo))
             (out (cdr (bndos-test-cli repo "context"))))
        (should (equal (file-name-as-directory (plist-get out :container)) folder))
        (should (equal (plist-get out :context) (expand-file-name ".tasks" (directory-file-name folder))))
        (should (equal (bndos-task-git-container repo) folder))
        ;; From a worktree too.
        (let ((wt (bndos-test-worktree repo "side" "main")))
          (should (equal (bndos-task-git-container wt) folder)))))))

(ert-deftest bndos-wt-worktrees-go-in-the-project-folder ()
  (skip-unless (bndos-test-wt-available-p))
  (bndos-test-with-repo repo
    (let ((folder (file-name-directory (directory-file-name repo))))
      ;; Without .tasks: worktrunk's layout (the runner's: beside the repo).
      (let ((made (bndos-test-await (lambda (cb) (bndos-task-wt-create repo "plain" "main" cb)))))
        (should (equal (file-name-as-directory (file-truename (plist-get made :path)))
                       (expand-file-name "repo.plain/" folder))))
      (bndos-test-project-folder repo)
      (let ((made (bndos-test-await (lambda (cb) (bndos-task-wt-create repo "feat/x" "main" cb)))))
        (should (equal (file-name-as-directory (file-truename (plist-get made :path)))
                       (expand-file-name "feat-x/" folder))))
      ;; Restoring a gone task's worktree lands there too.
      (bndos-test-git repo "worktree" "remove" (expand-file-name "feat-x" folder))
      (let ((back (bndos-test-await (lambda (cb) (bndos-task-wt-restore repo "feat/x" cb)))))
        (should (equal (file-name-as-directory (file-truename (plist-get back :path)))
                       (expand-file-name "feat-x/" folder)))))))

(ert-deftest bndos-cli-start-in-a-project-folder ()
  (bndos-test-with-stub-hydra
    (bndos-test-with-repo repo
      (let* ((folder (bndos-test-project-folder repo))
             (out (cdr (bndos-test-cli repo "start" "--agent" "bndos-stub" "--base" "main"
                                       "--prompt" "Build the thing." "feat/y")))
             (wt (file-name-as-directory (plist-get out :worktree))))
        (should (equal (plist-get out :status) "started"))
        (should (equal wt (expand-file-name "feat-y/" folder)))
        (should (equal (plist-get out :context) (expand-file-name ".tasks" (directory-file-name folder))))
        ;; The brief among the project's briefs, and in the store as before.
        (should (equal (with-temp-buffer (insert-file-contents (expand-file-name ".tasks/briefs/feat-y.md" folder))
                                         (buffer-string))
                       "Build the thing."))
        (let ((key (bndos-task-key (bndos-test-git repo "rev-parse" "--path-format=absolute" "--git-common-dir") wt)))
          (should (file-exists-p (expand-file-name (concat "briefs/" key ".md") bndos-task-store-directory)))
          ;; The log by task key: branches named alike in two projects don't share one.
          (should (file-exists-p (expand-file-name (concat "logs/" key ".log") bndos-task-store-directory))))
        (should (equal (plist-get (cdr (bndos-test-cli repo "status" "feat/y")) :context)
                       (plist-get out :context)))))))

;;; migrate: a plain clone into a project folder of its own

(defun bndos-test-json-file (file)
  (json-parse-string (with-temp-buffer (insert-file-contents file) (buffer-string))
                     :object-type 'plist :array-type 'list :null-object nil :false-object nil))

(defun bndos-test-migratable (repo)
  "REPO with a task on branch feat: record, thread, brief, review file, recorded session.
Return (KEY . WORKTREE)."
  (pcase-let* ((`(,key . ,wt) (bndos-test-task repo "feat" "main"))
               (common (bndos-test-git repo "rev-parse" "--path-format=absolute" "--git-common-dir")))
    (bndos-task-store-update key (lambda (r) (bndos-task-add-thread
                                              r (bndos-task-make-thread :kind "comment" :author "me" :body "keep me"))))
    (bndos-test-write bndos-task-store-directory (concat "briefs/" key ".md") "Build feat.\n")
    ;; The review file the store keeps the thread in, rooted at the worktree.
    (let* ((file (expand-file-name "review/feat.json" common))
           (review (if (file-exists-p file)
                       (json-parse-string (with-temp-buffer (insert-file-contents file) (buffer-string)))
                     (make-hash-table :test #'equal))))
      (puthash "root" wt review)
      (bndos-test-write common "review/feat.json" (json-serialize review)))
    (bndos-test-git repo "config" "branch.feat.bndos-base" "main")
    (bndos-test-git repo "config" "branch.feat.bndos-session" "hydra_session_old")
    (bndos-test-write bndos-task-store-directory "projects.json" (json-serialize (vector repo)))
    (cons key wt)))

(ert-deftest bndos-cli-migrate-dry-run-changes-nothing ()
  (bndos-test-with-stub-hydra
    (bndos-test-with-repo repo
      (bndos-test-migratable repo)
      (bndos-test-write repo ".envrc" "export A=1\n")
      (let* ((trees (bndos-test-git repo "worktree" "list"))
             (out (cdr (bndos-test-cli repo "migrate" "--dry-run")))
             (folder (directory-file-name repo)))
        (should (equal (plist-get out :status) "planned"))
        (should (equal (plist-get out :folder) folder))
        (should (equal (plist-get out :main) (expand-file-name "repo" repo)))
        (should (equal (mapcar (lambda (m) (plist-get m :to)) (plist-get out :worktrees))
                       (list (expand-file-name "feat" repo))))
        (should (equal (plist-get out :sessions_dropped) '("feat")))
        (should (seq-find (lambda (f) (string-match-p "direnv allow" f)) (plist-get out :follow_up)))
        (should (equal (bndos-test-git repo "worktree" "list") trees))
        (should (file-exists-p (expand-file-name ".git" repo)))
        (should (equal (bndos-test-git repo "config" "branch.feat.bndos-session") "hydra_session_old"))))))

(ert-deftest bndos-cli-migrate-moves-the-repo-and-its-tasks ()
  (bndos-test-with-stub-hydra
    (bndos-test-with-repo repo
      (pcase-let* ((`(,old-key . ,old-wt) (bndos-test-migratable repo))
                   (folder repo)
                   (main (expand-file-name "repo/" folder))
                   (wt (expand-file-name "feat/" folder))
                   (out (cdr (bndos-test-cli repo "migrate"))))
        (should (equal (plist-get out :status) "migrated"))
        ;; The layout: the repo one level down, its task beside it, .tasks.
        (should (file-directory-p (expand-file-name ".tasks/briefs" folder)))
        (should-not (file-exists-p (expand-file-name ".git" folder)))
        (should-not (file-exists-p old-wt))
        (should (equal (bndos-test-git main "rev-parse" "--show-toplevel") (directory-file-name main)))
        (should (equal (bndos-test-git wt "rev-parse" "--abbrev-ref" "HEAD") "feat"))
        (should (string-match-p (regexp-quote (directory-file-name wt)) (bndos-test-git main "worktree" "list")))
        ;; Context from either checkout, in the CLI and in Emacs.
        (should (equal (plist-get (cdr (bndos-test-cli wt "context")) :context)
                       (expand-file-name ".tasks" (directory-file-name folder))))
        (should (equal (bndos-task-git-container main) folder))
        ;; The record under its new key: paths, thread kept, session gone.
        (let* ((common (bndos-test-git main "rev-parse" "--path-format=absolute" "--git-common-dir"))
               (key (bndos-task-key common wt))
               (record (bndos-task-store-load key)))
          (should-not (equal key old-key))
          (should-not (file-exists-p (bndos-task-store-file old-key)))
          (should (equal (bndos-task-get record :worktree :path) wt))
          (should (equal (bndos-task-get record :repo :main-path) main))
          (should (equal (bndos-task-get record :repo :common-dir) (directory-file-name common)))
          (should (equal (bndos-task-get record :worktree :handle) "feat"))
          (should (= (length (plist-get record :threads)) 1))
          (should-not (bndos-task-get record :agent :session-id))
          (should (file-exists-p (expand-file-name (concat "briefs/" key ".md") bndos-task-store-directory)))
          (should (equal (with-temp-buffer (insert-file-contents (expand-file-name ".tasks/briefs/feat.md" folder))
                                           (buffer-string))
                         "Build feat.\n"))
          ;; The review file follows its worktree.
          (should (equal (plist-get (bndos-test-json-file (expand-file-name "review/feat.json" common)) :root) wt)))
        (should-not (ignore-errors (bndos-test-git main "config" "branch.feat.bndos-session")))
        (should (equal (bndos-test-git main "config" "branch.feat.bndos-base") "main"))
        (should (equal (bndos-test-json-file (expand-file-name "projects.json" bndos-task-store-directory))
                       (list main)))
        ;; Done once.
        (should (eql (car (bndos-test-cli main "migrate")) 1))))))

(ert-deftest bndos-cli-migrate-refuses-what-it-can-t-move-safely ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      ;; A task whose folder name would clash with the repo's.
      (bndos-test-worktree repo "repo" "main")
      (let ((out (bndos-test-cli repo "migrate" "--dry-run")))
        (should (eql (car out) 1))
        (should (string-match-p "taken" (plist-get (cdr out) :error))))))
  ;; Two branches that would land in one folder.
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (bndos-test-worktree repo "a/b" "main")
      (bndos-test-worktree repo "a-b" "main")
      (let ((out (bndos-test-cli repo "migrate" "--dry-run")))
        (should (eql (car out) 1))
        (should (string-match-p "taken" (plist-get (cdr out) :error)))))))

(ert-deftest bndos-cli-migrate-waits-for-busy-agents ()
  (bndos-test-with-stub-hydra
    (bndos-test-with-repo repo
      (let ((started (cdr (bndos-test-cli repo "start" "--agent" "bndos-stub" "--base" "main"
                                          "--prompt" "PERMIT write it" "busy"))))
        (should (bndos-test-await
                 (lambda (cb)
                   (let ((tries 0))
                     (named-let poll ()
                       (if (equal (plist-get (cdr (bndos-test-cli repo "status" "busy")) :state) "waiting")
                           (funcall cb t)
                         (if (> (cl-incf tries) 20) (funcall cb nil) (run-at-time 0.5 nil #'poll))))))
                 20))
        (let ((out (bndos-test-cli repo "migrate")))
          (should (eql (car out) 1))
          (should (string-match-p "busy is waiting" (plist-get (cdr out) :error))))
        (should (file-exists-p (expand-file-name ".git" repo)))
        (should (file-directory-p (plist-get started :worktree)))))))

;;; Usage limits

(defun bndos-test-poll (fn &optional tries)
  "Call FN every half second until it returns non-nil, at most TRIES times; its value."
  (let ((n (or tries 30)) value)
    (while (and (> n 0) (not (setq value (funcall fn))))
      (sleep-for 0.5) (cl-decf n))
    value))

(ert-deftest bndos-cli-a-limited-agent-shows-as-limited-not-working ()
  (bndos-test-with-stub-hydra
    (bndos-test-with-repo repo
      (bndos-test-cli repo "start" "--agent" "bndos-stub" "--base" "main" "--prompt" "RATELIMIT now" "lim")
      ;; Claude waits the limit out inside the turn: still busy, but limited.
      (let ((status (bndos-test-poll (lambda () (let ((s (cdr (bndos-test-cli repo "status" "lim"))))
                                                  (and (equal (plist-get s :state) "limited") s))))))
        (should status)
        (should-not (plist-get status :done))
        (let ((limit (plist-get status :limit)))
          (should (equal (plist-get limit :status) "rejected"))
          (should (equal (plist-get limit :type) "five_hour"))
          (should (equal (plist-get limit :source) "claude"))
          (should (stringp (plist-get limit :resets_at)))))
      ;; Prompting again: the limit is no longer the news.
      (bndos-test-cli repo "send" "lim" "--prompt" "hello again")
      (let ((status (cdr (bndos-test-cli repo "status" "lim"))))
        (should (equal (plist-get status :state) "working"))
        (should-not (plist-get status :limit))))))

(ert-deftest bndos-cli-close-to-a-limit-and-limits-agents-only-say ()
  (bndos-test-with-stub-hydra
    (bndos-test-with-repo repo
      (let ((out (cdr (bndos-test-cli repo "start" "--agent" "bndos-stub" "--base" "main"
                                      "--prompt" "RATEWARN" "--wait" "warn"))))
        (should (equal (plist-get (plist-get out :limit) :status) "allowed_warning"))
        (let ((status (cdr (bndos-test-cli repo "status" "warn"))))
          (should (plist-get status :done))
          (should (equal (plist-get status :state) "idle"))
          (should (equal (plist-get (plist-get status :limit) :type) "seven_day"))
          (should (= (plist-get (plist-get status :limit) :utilization) 0.91))))
      (bndos-test-cli repo "start" "--agent" "bndos-stub" "--base" "main" "--prompt" "USAGETEXT" "--wait" "said")
      (let ((status (cdr (bndos-test-cli repo "status" "said"))))
        (should (equal (plist-get status :state) "limited"))
        (should (equal (plist-get (plist-get status :limit) :source) "message"))
        (should (string-match-p "usage limit" (plist-get (plist-get status :limit) :message)))))))

(ert-deftest bndos-cli-a-limit-past-its-reset-is-gone ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (bndos-test-worktree repo "old" "main")
      (bndos-test-git repo "config" "branch.old.bndos-base" "main")
      (bndos-test-git repo "config" "branch.old.bndos-limit"
                      (json-serialize (list :status "rejected" :type "five_hour"
                                            :resets_at "2020-01-01T00:00:00+0000" :source "claude")))
      (should-not (plist-get (cdr (bndos-test-cli repo "status" "old")) :limit))
      (bndos-test-git repo "config" "branch.old.bndos-limit"
                      (json-serialize (list :status "rejected" :type "five_hour"
                                            :resets_at "2099-01-01T00:00:00+0000" :source "claude")))
      (let ((status (cdr (bndos-test-cli repo "status" "old"))))
        (should (equal (plist-get (plist-get status :limit) :type) "five_hour"))
        ;; No agent: nothing is held up, so not "limited".
        (should (equal (plist-get status :state) "none")))
      ;; Garbage is no limit, and doesn't break status.
      (bndos-test-git repo "config" "branch.old.bndos-limit"
                      (json-serialize (list :status "rejected" :resets_at "soon" :utilization "lots")))
      (let ((out (bndos-test-cli repo "status" "old")))
        (should (eql (car out) 0))
        (should-not (plist-get (cdr out) :limit)))
      (bndos-test-git repo "config" "branch.old.bndos-limit" "[1, 2]")
      (should-not (plist-get (cdr (bndos-test-cli repo "status" "old")) :limit)))))

(provide 'bndos-cli-tests)
;;; bndos-cli-tests.el ends here
