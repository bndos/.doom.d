;;; bndos-task-git.el --- Git plumbing for task reviews -*- lexical-binding: t; -*-

;;; Commentary:

;; Git helpers for reviewing a task worktree: repository identity, the
;; recorded parent (git config branch.<branch>.bndos-base, written when the
;; task is created or its base is changed), merge-base, and a snapshot of the
;; whole worktree (asynchronous, or synchronous for diff-review's scan).
;;
;; Every comparison is between two tree-ish objects.  The worktree snapshot is
;; a tree built through a temporary index (`git add -A' + `write-tree' with
;; GIT_INDEX_FILE pointing at a copy), so it contains committed, staged,
;; unstaged and untracked (non-ignored) content without touching the user's
;; index, working tree or refs.  Only loose objects are written.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'bndos-proc)

(defconst bndos-task-git-empty-tree "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  "The empty tree (SHA-1 repositories).")

(defun bndos-task-git (directory args callback &optional env)
  "Run git ARGS in DIRECTORY; call CALLBACK with the `bndos-proc-run' result."
  ;; --no-optional-locks: background reads must never rewrite the index.
  (bndos-proc-run "git" (append '("--no-pager" "--no-optional-locks" "-c" "core.quotePath=false"
                                  "-c" "color.ui=false")
                                args)
                  :directory directory :env env :callback callback))

(defun bndos-task-git--lines (result)
  (split-string (string-trim-right (or (plist-get result :stdout) "")) "\n" t))

(defmacro bndos-task-git--chain (directory args result-var on-error &rest body)
  "Run git ARGS in DIRECTORY; on success bind RESULT-VAR and run BODY.
On failure call ON-ERROR with a message string."
  (declare (indent 4))
  `(bndos-task-git ,directory ,args
                   (lambda (,result-var)
                     (if (bndos-proc-ok-p ,result-var)
                         (progn ,@body)
                       (funcall ,on-error (bndos-proc-error-message ,result-var))))))


;;; Identity

(defun bndos-task-git-identity (directory callback)
  "Call CALLBACK with DIRECTORY's repository identity or (:error MSG).
Identity is (:common-dir DIR :toplevel DIR :branch NAME-or-nil :head SHA-or-nil)."
  (let ((fail (lambda (msg) (funcall callback (list :error msg)))))
    (bndos-task-git--chain directory
                           '("rev-parse" "--path-format=absolute" "--git-common-dir" "--show-toplevel")
                           r1 fail
                           (pcase-let ((`(,common ,top) (bndos-task-git--lines r1)))
                             (bndos-task-git directory '("symbolic-ref" "-q" "--short" "HEAD")
                                             (lambda (r2)
                                               (bndos-task-git directory '("rev-parse" "-q" "--verify" "HEAD^{commit}")
                                                               (lambda (r3)
                                                                 (funcall callback
                                                                          (list :common-dir (file-truename common)
                                                                                :toplevel (file-name-as-directory
                                                                                           (file-truename top))
                                                                                :branch (and (bndos-proc-ok-p r2)
                                                                                             (car (bndos-task-git--lines r2)))
                                                                                :head (and (bndos-proc-ok-p r3)
                                                                                           (car (bndos-task-git--lines r3)))))))))))))


;;; Base resolution

(defun bndos-task-git--verify-commit (directory ref callback)
  "Call CALLBACK with REF's commit SHA in DIRECTORY, or nil."
  (bndos-task-git directory (list "rev-parse" "-q" "--verify" (concat ref "^{commit}"))
                  (lambda (result)
                    (funcall callback (and (bndos-proc-ok-p result)
                                           (car (bndos-task-git--lines result)))))))

(defun bndos-task-git-base-key (branch)
  "Git config key recording BRANCH's parent."
  (format "branch.%s.bndos-base" branch))

(defun bndos-task-git-saved-base (directory branch callback)
  "Call CALLBACK with BRANCH's recorded parent ref, or nil."
  (if (not branch)
      (funcall callback nil)
    (bndos-task-git directory (list "config" "--get" (bndos-task-git-base-key branch))
                    (lambda (result)
                      (funcall callback (and (bndos-proc-ok-p result)
                                             (car (bndos-task-git--lines result))))))))

(defun bndos-task-git-saved-base-sync (directory branch)
  "BRANCH's recorded parent ref, read in DIRECTORY, or nil."
  (and branch
       (car (ignore-errors
              (let ((default-directory (file-name-as-directory directory)))
                (process-lines "git" "config" "--get" (bndos-task-git-base-key branch)))))))

(defun bndos-task-git-set-base (directory branch ref callback)
  "Record REF as BRANCH's parent; CALLBACK gets nil or an error message."
  (bndos-task-git directory (list "config" (bndos-task-git-base-key branch) ref)
                  (lambda (result)
                    (funcall callback (unless (bndos-proc-ok-p result)
                                        (bndos-proc-error-message result))))))

(defun bndos-task-git-base-candidates (directory callback)
  "Call CALLBACK with plausible fallback base refs that exist in DIRECTORY."
  (bndos-task-git directory '("symbolic-ref" "-q" "--short" "refs/remotes/origin/HEAD")
                  (lambda (result)
                    (let* ((origin (and (bndos-proc-ok-p result) (car (bndos-task-git--lines result))))
                           (names (delete-dups (delq nil (list origin "main" "master"
                                                               "origin/main" "origin/master"))))
                           (found nil)
                           (pending (length names)))
                      (dolist (name names)
                        (bndos-task-git--verify-commit
                         directory name
                         (lambda (sha)
                           (when sha (push name found))
                           (when (zerop (cl-decf pending))
                             (funcall callback (seq-filter (lambda (n) (member n found)) names))))))))))

(cl-defun bndos-task-git-resolve-base (directory &key branch callback)
  "Resolve the comparison base for DIRECTORY and call CALLBACK with a plist.

The parent ref is BRANCH's recorded branch.<BRANCH>.bndos-base.  The
result is

  (:ref REF :saved SAVED :commit SHA :merge-base SHA)

or, when no usable base exists, (:error SYMBOL :message STR :saved SAVED
:candidates REFS) with SYMBOL one of `no-saved-base', `base-missing',
`no-merge-base'.  Nothing is guessed: candidates are offered, not chosen."
  (bndos-task-git-saved-base
   directory branch
   (lambda (saved)
     (let ((ref saved))
       (cl-flet ((fail (kind message)
                   (bndos-task-git-base-candidates
                    directory
                    (lambda (candidates)
                      (funcall callback (list :error kind :message message :saved saved
                                              :ref ref :candidates candidates))))))
         (if (not ref)
             (fail 'no-saved-base
                   (format "No recorded parent for %s" (or branch "detached HEAD")))
           (bndos-task-git--verify-commit
            directory ref
            (lambda (commit)
              (if (not commit)
                  (fail 'base-missing (format "Base %s no longer exists" ref))
                (bndos-task-git directory (list "merge-base" commit "HEAD")
                                (lambda (result)
                                  (if (bndos-proc-ok-p result)
                                      (funcall callback
                                               (list :ref ref :saved saved
                                                     :commit commit
                                                     :merge-base (car (bndos-task-git--lines result))))
                                    (fail 'no-merge-base
                                          (format "%s and HEAD share no history" ref))))))))))))))


;;; Snapshots

(defun bndos-task-git--index-file ()
  "A fresh temporary index file.
They live in one directory so ones stranded by an Emacs that exited
mid-snapshot are swept on a later snapshot."
  (let ((dir (expand-file-name "bndos-task-index/" temporary-file-directory)))
    (make-directory dir t)
    (dolist (stale (directory-files dir t "\\`index-"))
      (when (time-less-p (file-attribute-modification-time (file-attributes stale))
                         (time-subtract nil 600))
        (ignore-errors (delete-file stale))))
    (make-temp-file (expand-file-name "index-" dir))))

(defun bndos-task-git-snapshot (directory callback)
  "Snapshot DIRECTORY's whole worktree and call CALLBACK with a plist.
Result: (:tree SHA :index-tree SHA :head SHA-or-nil) or (:error MSG).
:tree holds committed + staged + unstaged + untracked content; :index-tree
is the current index (for staged-only and unstaged-only views)."
  (let ((fail (lambda (msg) (funcall callback (list :error msg)))))
    (bndos-task-git--chain directory
                           '("rev-parse" "--path-format=absolute" "--git-path" "index")
                           r-index fail
                           (let* ((index (car (bndos-task-git--lines r-index)))
                                  ;; Both trees are written through copies of the index: even a
                                  ;; plain `write-tree' may rewrite the real index's cache-tree.
                                  (work-index (bndos-task-git--index-file))
                                  (staged-index (bndos-task-git--index-file))
                                  (cleanup (lambda ()
                                             (ignore-errors (delete-file work-index))
                                             (ignore-errors (delete-file staged-index))))
                                  (fail (lambda (msg) (funcall cleanup) (funcall fail msg))))
                             ;; Starting from a copy keeps git's stat cache, so this is cheap.
                             ;; Its time is kept too: git rechecks files as new as the index
                             ;; ("racy" ones), which a newer copy would hide.
                             (if (file-exists-p index)
                                 (progn (copy-file index work-index t t)
                                        (copy-file index staged-index t t))
                               (delete-file work-index)
                               (delete-file staged-index))
                             (bndos-task-git
                              directory '("add" "-A" "--" ".")
                              (lambda (r-add)
                                (if (not (bndos-proc-ok-p r-add))
                                    (funcall fail (bndos-proc-error-message r-add))
                                  (bndos-task-git
                                   directory '("write-tree")
                                   (lambda (r-tree)
                                     (if (not (bndos-proc-ok-p r-tree))
                                         (funcall fail (bndos-proc-error-message r-tree))
                                       (bndos-task-git
                                        directory '("write-tree")
                                        (lambda (r-index-tree)
                                          (if (not (bndos-proc-ok-p r-index-tree))
                                              (funcall fail (bndos-proc-error-message r-index-tree))
                                            (bndos-task-git
                                             directory '("rev-parse" "-q" "--verify" "HEAD^{commit}")
                                             (lambda (r-head)
                                               (funcall cleanup)
                                               (funcall callback
                                                        (list :tree (car (bndos-task-git--lines r-tree))
                                                              :index-tree (car (bndos-task-git--lines r-index-tree))
                                                              :head (and (bndos-proc-ok-p r-head)
                                                                         (car (bndos-task-git--lines r-head)))))))))
                                        (list (concat "GIT_INDEX_FILE=" staged-index)))))
                                   (list (concat "GIT_INDEX_FILE=" work-index)))))
                              (list (concat "GIT_INDEX_FILE=" work-index)))))))

(defun bndos-task-git-snapshot-sync (directory)
  "Like `bndos-task-git-snapshot', synchronously: (:tree :index-tree :head) or (:error MSG)."
  (let ((work-index (bndos-task-git--index-file))
        (staged-index (bndos-task-git--index-file)))
    (cl-flet ((git (index &rest args)
                "Git ARGS's output, or nil when it fails."
                (with-temp-buffer
                  (let ((default-directory (file-name-as-directory directory))
                        (process-environment (if index (cons (concat "GIT_INDEX_FILE=" index) process-environment)
                                               process-environment)))
                    (and (zerop (apply #'process-file "git" nil '(t nil) nil "--no-optional-locks" args))
                         (string-trim (buffer-string)))))))
      (unwind-protect
          (let ((index (git nil "rev-parse" "--path-format=absolute" "--git-path" "index")))
            ;; Copies of the index, as in `bndos-task-git-snapshot'.
            (if (and index (file-exists-p index))
                (progn (copy-file index work-index t t) (copy-file index staged-index t t))
              (delete-file work-index) (delete-file staged-index))
            (let ((tree (and (git work-index "add" "-A" "--" ".") (git work-index "write-tree")))
                  (index-tree (git staged-index "write-tree")))
              (if (and tree index-tree)
                  (list :tree tree :index-tree index-tree
                        :head (git nil "rev-parse" "-q" "--verify" "HEAD^{commit}"))
                (list :error (format "couldn't snapshot %s" directory)))))
        (ignore-errors (delete-file work-index))
        (ignore-errors (delete-file staged-index))))))


(provide 'bndos-task-git)
;;; bndos-task-git.el ends here
