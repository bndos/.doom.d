;;; test-helper.el --- Shared fixtures for the task/review tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'bndos-proc)
(require 'bndos-task-store)
(require 'bndos-task-git)
(require 'bndos-task-wt)

(defconst bndos-test-dir (file-name-directory (or load-file-name buffer-file-name)))

(defmacro bndos-test-with-store (&rest body)
  "Run BODY with an empty, private task store."
  (declare (indent 0))
  `(let* ((bndos-task-store-directory (make-temp-file "bndos-store-" t))
          (bndos-task-store--cache (make-hash-table :test #'equal)))
     (unwind-protect (progn ,@body)
       (delete-directory bndos-task-store-directory t))))

(defun bndos-test-await (fn &optional timeout)
  "Call FN with a callback; wait for and return the value it receives."
  (let ((box (list 'pending)))
    (funcall fn (lambda (&rest args) (setcar box (if (cdr args) args (car args)))))
    (bndos-proc-wait (lambda () (not (eq (car box) 'pending))) (or timeout 20))
    (when (eq (car box) 'pending) (error "Timed out waiting for callback"))
    (car box)))

(defun bndos-test-git (dir &rest args)
  "Run git ARGS synchronously in DIR; return trimmed stdout, error on failure."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "git %s failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun bndos-test-write (dir file content)
  (let ((path (expand-file-name file dir)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    path))

(defun bndos-test-commit (dir message &rest files)
  "Write FILES (alternating name, content) in DIR and commit them."
  (while files
    (bndos-test-write dir (pop files) (pop files)))
  (bndos-test-git dir "add" "-A")
  (bndos-test-git dir "commit" "-q" "-m" message))

(defmacro bndos-test-with-repo (var &rest body)
  "Bind VAR to a fresh repository on branch main with one commit."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (file-truename (make-temp-file "bndos-repo-" t))))
          (,var (file-name-as-directory (expand-file-name "repo" root)))
          (process-environment (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                                         "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t"
                                         "GIT_CONFIG_NOSYSTEM=1")
                                       process-environment)))
     (make-directory ,var t)
     (bndos-test-git ,var "init" "-q" "-b" "main")
     (bndos-test-git ,var "config" "commit.gpgsign" "false")
     (bndos-test-commit ,var "init" "README" "hello\n" "src/a.txt" "one\ntwo\nthree\n")
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun bndos-test-worktree (repo branch base)
  "Add a worktree for new BRANCH from BASE next to REPO, recording its parent."
  (let ((path (file-name-as-directory (expand-file-name (concat "../wt-" branch) repo))))
    (bndos-test-git repo "worktree" "add" "-q" "-b" branch (directory-file-name path) base)
    (bndos-test-git repo "config" (format "branch.%s.bndos-base" branch) base)
    (file-name-as-directory (file-truename path))))

(defun bndos-test-wt-available-p ()
  (executable-find bndos-task-wt-program))

(defun bndos-test-fake-file (dir name content)
  (with-temp-file (expand-file-name name dir) (insert content)))

(provide 'test-helper)
;;; test-helper.el ends here
