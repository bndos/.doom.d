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

(provide 'bndos-cli-tests)
;;; bndos-cli-tests.el ends here
