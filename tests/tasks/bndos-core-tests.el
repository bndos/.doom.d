;;; bndos-core-tests.el --- Process, worktrunk, store and git tests -*- lexical-binding: t; -*-

(require 'test-helper)


;;; bndos-proc

(ert-deftest bndos-proc-captures-exit-stdout-stderr ()
  (let ((r (bndos-test-await (lambda (cb) (bndos-proc-run "sh" '("-c" "echo out; echo err >&2; exit 3")
                                                          :directory temporary-file-directory :callback cb)))))
    (should (equal (plist-get r :exit) 3))
    (should (equal (plist-get r :stdout) "out\n"))
    (should (equal (string-trim (plist-get r :stderr)) "err"))
    (should-not (bndos-proc-ok-p r))
    (should (string-match-p "exited 3: err" (bndos-proc-error-message r)))))

(ert-deftest bndos-proc-passes-argv-without-a-shell ()
  (let ((r (bndos-test-await (lambda (cb) (bndos-proc-run "printf" '("%s|" "a b" "$(rm -rf /)" ";x")
                                                          :directory temporary-file-directory :callback cb)))))
    (should (equal (plist-get r :stdout) "a b|$(rm -rf /)|;x|"))))

(ert-deftest bndos-proc-reports-missing-program-and-directory ()
  (let ((r (bndos-test-await (lambda (cb) (bndos-proc-run "bndos-no-such-program" nil
                                                          :directory temporary-file-directory :callback cb)))))
    (should (plist-get r :error))
    (should-not (plist-get r :exit)))
  (let ((r (bndos-test-await (lambda (cb) (bndos-proc-run "true" nil :directory "/nonexistent/x/" :callback cb)))))
    (should (string-match-p "No such directory" (plist-get r :error)))))

(ert-deftest bndos-proc-cancel-reports-cancelled ()
  (let* ((result nil)
         (proc (bndos-proc-run "sleep" '("30") :directory temporary-file-directory
                               :callback (lambda (r) (setq result r)))))
    (bndos-proc-cancel proc)
    (bndos-proc-wait (lambda () result) 5)
    (should (plist-get result :cancelled))
    (should (equal (bndos-proc-error-message result) "cancelled"))))

(ert-deftest bndos-proc-stdin ()
  (let ((r (bndos-test-await (lambda (cb) (bndos-proc-run "cat" nil :stdin "piped" :directory temporary-file-directory
                                                          :callback cb)))))
    (should (equal (plist-get r :stdout) "piped"))))


(ert-deftest bndos-proc-chained-fast-processes-are-reliable ()
  "Regression: a child can exit before EOF is sent, inside another's callback."
  (let ((bad 0))
    (dotimes (_ 100)
      (let ((r (bndos-test-await
                (lambda (cb)
                  (bndos-proc-run "true" nil :directory temporary-file-directory
                                  :callback (lambda (_)
                                              (bndos-proc-run "echo" '("ok") :directory temporary-file-directory
                                                              :callback cb)))))))
        (unless (equal (plist-get r :stdout) "ok\n") (cl-incf bad))))
    (should (= bad 0))))


;;; worktrunk bridge

(ert-deftest bndos-wt-create-and-list-worktrees ()
  (skip-unless (bndos-test-wt-available-p))
  (bndos-test-with-repo repo
    (let ((bndos-task-wt--capabilities nil))
      (should-not (bndos-task-wt-capability-problem (bndos-test-await #'bndos-task-wt-capabilities)))
      (let ((made (bndos-test-await (lambda (cb) (bndos-task-wt-create repo "feat" "main" cb)))))
        (should (equal (plist-get made :branch) "feat"))
        (should (file-directory-p (plist-get made :path)))
        ;; Creating it twice fails with worktrunk's own explanation.
        (let ((again (bndos-test-await (lambda (cb) (bndos-task-wt-create repo "feat" "main" cb)))))
          (should (bndos-task-wt-error-p again))
          (should (string-match-p "already exists" (plist-get again :message))))
        (bndos-test-write (plist-get made :path) "new.txt" "x\n")
        (let* ((items (bndos-test-await (lambda (cb) (bndos-task-wt-list repo cb))))
               (feat (seq-find (lambda (i) (equal (plist-get i :branch) "feat")) items)))
          (should (seq-find (lambda (i) (plist-get i :main)) items))
          (should (equal (file-truename (plist-get feat :path)) (file-truename (plist-get made :path))))
          (should (bndos-task-get feat :changes :untracked)))))))

(ert-deftest bndos-wt-never-auto-approves-hooks ()
  (skip-unless (bndos-test-wt-available-p))
  (bndos-test-with-repo repo
    (bndos-test-commit repo "hooks" ".config/wt.toml" "[post-create]\nmark = \"touch HOOK_RAN\"\n")
    (let ((out (bndos-test-await (lambda (cb) (bndos-task-wt-create repo "hooked" "main" cb)))))
      (should (bndos-task-wt-error-p out))
      (should (string-match-p "approval" (plist-get out :message))))
    (should-not (file-expand-wildcards (expand-file-name "../*/HOOK_RAN" repo)))))

(ert-deftest bndos-wt-missing-executable-is-reported ()
  (let ((bndos-task-wt-program "bndos-definitely-not-wt")
        (bndos-task-wt--capabilities nil))
    (should (string-match-p "not found" (bndos-task-wt-capability-problem
                                         (bndos-test-await #'bndos-task-wt-capabilities))))))


;;; Store

(ert-deftest bndos-store-roundtrip-and-atomic-save ()
  (bndos-test-with-store
    (let* ((record (list :key "k1" :branch "b" :flag t :none nil
                         :agent (list :session-id "s" :provider "claude-code")
                         :threads (list (list :id "c-1" :replies (list (list :body "r" :files '("a" "b")))))))
           (saved (bndos-task-store-save record)))
      (should (equal (plist-get saved :version) bndos-task-store-version))
      (clrhash bndos-task-store--cache)
      (let ((loaded (bndos-task-store-load "k1")))
        (should (equal (bndos-task-get loaded :agent :session-id) "s"))
        (should (eq (plist-get loaded :flag) t))
        (should (null (plist-get loaded :none)))
        (should (equal (plist-get (car (plist-get (car (plist-get loaded :threads)) :replies)) :files)
                       '("a" "b"))))
      (should-not (file-expand-wildcards (file-name-concat bndos-task-store-directory "tasks" ".tmp-*"))))))

(ert-deftest bndos-store-never-evaluates-and-rejects-newer-versions ()
  (bndos-test-with-store
    (make-directory (file-name-concat bndos-task-store-directory "tasks") t)
    (with-temp-file (bndos-task-store-file "evil")
      (insert "(progn (setq bndos-test-pwned t))"))
    (defvar bndos-test-pwned nil)
    (should-error (bndos-task-store-load "evil"))
    (should-not bndos-test-pwned)
    (with-temp-file (bndos-task-store-file "future") (insert "{\"version\": 99, \"key\": \"future\"}"))
    (should-error (bndos-task-store-load "future"))))

(ert-deftest bndos-store-keys-use-repo-identity-not-names ()
  (let ((a (make-temp-file "proj-a-" t)) (b (make-temp-file "proj-b-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "wt/fix-login" a) t)
          (make-directory (expand-file-name "wt/fix-login" b) t)
          (should-not (equal (bndos-task-key (expand-file-name ".git" a) (expand-file-name "wt/fix-login" a))
                             (bndos-task-key (expand-file-name ".git" b) (expand-file-name "wt/fix-login" b))))
          (should (equal (bndos-task-key a (expand-file-name "wt/fix-login" a))
                         (bndos-task-key (concat a "/") (expand-file-name "wt/fix-login/" a)))))
      (delete-directory a t) (delete-directory b t))))

;;; Base selection and scopes

(ert-deftest bndos-git-base-is-the-saved-parent-not-main ()
  (bndos-test-with-repo repo
    (let* ((wt-a (bndos-test-worktree repo "task-a" "main")))
      (bndos-test-commit wt-a "a work" "a.txt" "a\n")
      (let* ((a-tip (bndos-test-git wt-a "rev-parse" "HEAD"))
             (wt-b (bndos-test-worktree repo "task-b" "task-a")))
        (bndos-test-commit wt-b "b work" "b.txt" "b\n")
        ;; main moves on after both branched.
        (bndos-test-commit repo "main moves" "main.txt" "m\n")
        (let ((base (bndos-test-await (lambda (cb) (bndos-task-git-resolve-base wt-b :branch "task-b" :callback cb)))))
          (should (equal (plist-get base :ref) "task-a"))
          (should (equal (plist-get base :merge-base) a-tip))
          (let* ((snap (bndos-test-await (lambda (cb) (bndos-task-git-snapshot wt-b cb))))
                 (files (bndos-test-await (lambda (cb) (bndos-task-git-diff wt-b (plist-get base :merge-base)
                                                                            (plist-get snap :tree) cb)))))
            ;; Only task-b's own change: not a.txt (the parent's) nor main.txt.
            (should (equal (mapcar (lambda (f) (plist-get f :path)) files) '("b.txt")))))
        ;; Changing the recorded parent changes the comparison.
        (should-not (bndos-test-await (lambda (cb) (bndos-task-git-set-base wt-b "task-b" "main" cb))))
        (let ((base (bndos-test-await (lambda (cb) (bndos-task-git-resolve-base wt-b :branch "task-b" :callback cb)))))
          (should (equal (plist-get base :ref) "main")))))))

(ert-deftest bndos-git-base-errors-offer-candidates-without-guessing ()
  (bndos-test-with-repo repo
    (bndos-test-git repo "worktree" "add" "-q" "-b" "loose" "../wt-loose" "main")
    (let* ((wt (expand-file-name "../wt-loose" repo))
           (base (bndos-test-await (lambda (cb) (bndos-task-git-resolve-base wt :branch "loose" :callback cb)))))
      (should (eq (plist-get base :error) 'no-saved-base))
      (should (member "main" (plist-get base :candidates)))
      (should-not (plist-get base :merge-base)))
    (bndos-test-git repo "config" "branch.loose.bndos-base" "deleted-parent")
    (let ((base (bndos-test-await (lambda (cb) (bndos-task-git-resolve-base
                                                (expand-file-name "../wt-loose" repo) :branch "loose" :callback cb)))))
      (should (eq (plist-get base :error) 'base-missing))
      (should (equal (plist-get base :saved) "deleted-parent")))))

(defun bndos-test-diff-paths (dir from to)
  (sort (mapcar (lambda (f) (plist-get f :path))
                (bndos-test-await (lambda (cb) (bndos-task-git-diff dir from to cb))))
        #'string<))

(ert-deftest bndos-git-snapshot-sees-everything-and-touches-nothing ()
  (bndos-test-with-repo repo
    (let ((wt (bndos-test-worktree repo "t" "main")))
      (bndos-test-commit wt "committed" "committed.txt" "c\n")
      (bndos-test-git wt "mv" "README" "README.md")
      (bndos-test-git wt "commit" "-q" "-m" "rename")
      (bndos-test-write wt "staged.txt" "s\n")
      (bndos-test-git wt "add" "staged.txt")
      (bndos-test-write wt "src/a.txt" "one\nTWO\nthree\n")
      (bndos-test-write wt "untracked.txt" "u\n")
      (with-temp-file (expand-file-name "blob.bin" wt) (set-buffer-multibyte nil) (insert "\0\1\2\377"))
      (let* ((index (bndos-test-git wt "rev-parse" "--path-format=absolute" "--git-path" "index"))
             ;; `git status' itself refreshes the index; read it after.
             (status-before (bndos-test-git wt "status" "--porcelain"))
             (index-before (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally index) (buffer-string)))
             (base (bndos-test-await (lambda (cb) (bndos-task-git-resolve-base wt :branch "t" :callback cb))))
             (snap (bndos-test-await (lambda (cb) (bndos-task-git-snapshot wt cb))))
             (mb (plist-get base :merge-base)))
        (bndos-test-await (lambda (cb) (bndos-task-git-status wt cb)))
        (should (equal index-before (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally index)
                                                      (buffer-string))))
        (should (equal status-before (bndos-test-git wt "status" "--porcelain")))
        (should (equal (bndos-test-diff-paths wt mb (plist-get snap :tree))
                       '("README.md" "blob.bin" "committed.txt" "src/a.txt" "staged.txt" "untracked.txt")))
        (should (equal (bndos-test-diff-paths wt mb (plist-get snap :head)) '("README.md" "committed.txt")))
        (should (equal (bndos-test-diff-paths wt (plist-get snap :head) (plist-get snap :tree))
                       '("blob.bin" "src/a.txt" "staged.txt" "untracked.txt")))
        (should (equal (bndos-test-diff-paths wt (plist-get snap :head) (plist-get snap :index-tree))
                       '("staged.txt")))
        (should (equal (bndos-test-diff-paths wt (plist-get snap :index-tree) (plist-get snap :tree))
                       '("blob.bin" "src/a.txt" "untracked.txt")))
        (let ((files (bndos-test-await (lambda (cb) (bndos-task-git-diff wt mb (plist-get snap :tree) cb)))))
          (should (equal (plist-get (seq-find (lambda (f) (equal (plist-get f :path) "README.md")) files) :status)
                         "renamed"))
          (should (plist-get (seq-find (lambda (f) (equal (plist-get f :path) "blob.bin")) files) :binary)))
        (should (equal (bndos-test-await (lambda (cb) (bndos-task-git-status wt cb)))
                       '(:staged 1 :unstaged 1 :untracked 2 :conflicted 0)))))))

(ert-deftest bndos-git-empty-diff ()
  (bndos-test-with-repo repo
    (let* ((wt (bndos-test-worktree repo "t" "main"))
           (snap (bndos-test-await (lambda (cb) (bndos-task-git-snapshot wt cb)))))
      (should (null (bndos-test-await (lambda (cb) (bndos-task-git-diff wt (plist-get snap :head)
                                                                         (plist-get snap :tree) cb))))))))


;;; Diff parsing

(ert-deftest bndos-git-parse-diff-shapes ()
  (let ((files (bndos-task-git-parse-diff
                (concat "diff --git a/old name.txt b/new name.txt\n"
                        "similarity index 90%\nrename from old name.txt\nrename to new name.txt\n"
                        "--- a/old name.txt\n+++ b/new name.txt\n"
                        "@@ -1,2 +1,2 @@\n keep\n-gone\n+came\n\\ No newline at end of file\n"
                        "diff --git a/del.txt b/del.txt\ndeleted file mode 100644\n"
                        "--- a/del.txt\n+++ /dev/null\n@@ -1 +0,0 @@\n-bye\n"
                        "diff --git a/bin.png b/bin.png\nnew file mode 100644\n"
                        "Binary files /dev/null and b/bin.png differ\n"
                        "diff --git \"a/caf\\303\\251.txt\" \"b/caf\\303\\251.txt\"\n"
                        "--- \"a/caf\\303\\251.txt\"\n+++ \"b/caf\\303\\251.txt\"\n@@ -3 +3 @@\n-x\n+y\n"))))
    (should (equal (mapcar (lambda (f) (plist-get f :path)) files)
                   '("new name.txt" "del.txt" "bin.png" "café.txt")))
    (should (equal (mapcar (lambda (f) (plist-get f :status)) files)
                   '("renamed" "deleted" "new file" "modified")))
    (let* ((rename (car files))
           (lines (plist-get (car (plist-get rename :hunks)) :lines)))
      (should (equal (plist-get rename :old-path) "old name.txt"))
      (should (equal (mapcar (lambda (l) (list (plist-get l :type) (plist-get l :old) (plist-get l :new))) lines)
                     '((context 1 1) (removed 2 nil) (added nil 2) (meta nil nil)))))
    (should (plist-get (nth 2 files) :binary))
    (should (equal (plist-get (car (plist-get (nth 3 files) :hunks)) :new-start) 3))))


;;; Re-anchoring

(ert-deftest bndos-reanchor-conservative ()
  (require 'bndos-task-review)
  (let ((anchor (list :text "target" :line 3 :before '("b1" "b2") :after '("a1"))))
    (should (eq (plist-get (bndos-task-review-reanchor-anchor anchor '("x" "b1" "b2" "target" "a1")) :status)
                'moved))
    (should (equal (plist-get (bndos-task-review-reanchor-anchor anchor '("x" "b1" "b2" "target" "a1")) :line) 4))
    (should (eq (plist-get (bndos-task-review-reanchor-anchor anchor '("b1" "b2" "target" "a1")) :status) 'ok))
    (should (eq (plist-get (bndos-task-review-reanchor-anchor anchor '("b1" "b2" "other" "a1")) :status) 'stale))
    ;; Two equally good matches: ambiguous, so stale rather than guessed.
    (should (string-match-p "ambiguous"
                            (plist-get (bndos-task-review-reanchor-anchor
                                        anchor '("q" "target" "q" "target" "q" "q")) :reason)))
    ;; Context disambiguates duplicates.
    (should (equal (plist-get (bndos-task-review-reanchor-anchor
                               anchor '("target" "z" "b1" "b2" "target" "a1")) :line)
                   5))
    (should (eq (plist-get (bndos-task-review-reanchor-anchor anchor nil) :status) 'stale))))

(provide 'bndos-core-tests)
;;; bndos-core-tests.el ends here
