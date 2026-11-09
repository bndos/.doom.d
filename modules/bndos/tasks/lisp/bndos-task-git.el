;;; bndos-task-git.el --- Git plumbing for task reviews -*- lexical-binding: t; -*-

;;; Commentary:

;; Asynchronous git helpers for reviewing a task worktree: repository
;; identity, the recorded parent (git config branch.<branch>.bndos-base,
;; written when the task is created or its base is changed), merge-base,
;; a snapshot of the whole worktree, and tree-to-tree diffs parsed into
;; files/hunks/lines.
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
                             (if (file-exists-p index)
                                 (progn (copy-file index work-index t)
                                        (copy-file index staged-index t))
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

(defun bndos-task-git-object-exists (directory object callback)
  "Call CALLBACK with non-nil when OBJECT exists in DIRECTORY's repository."
  (bndos-task-git directory (list "cat-file" "-e" object)
                  (lambda (result) (funcall callback (bndos-proc-ok-p result)))))


;;; Worktree status

(defun bndos-task-git-parse-status (text)
  "Count entries in `git status --porcelain=v2 -z' TEXT.
Return (:staged N :unstaged N :untracked N :conflicted N)."
  (let ((staged 0) (unstaged 0) (untracked 0) (conflicted 0)
        (fields (split-string text "\0" t))
        (skip 0))
    (dolist (field fields)
      (if (> skip 0)
          (cl-decf skip)
        (pcase (substring field 0 (min 2 (length field)))
          ("? " (cl-incf untracked))
          ("u " (cl-incf conflicted))
          ((or "1 " "2 ")
           (let ((xy (substring field 2 4)))
             (unless (eq (aref xy 0) ?.) (cl-incf staged))
             (unless (eq (aref xy 1) ?.) (cl-incf unstaged)))
           ;; Renames/copies carry the original path as an extra field.
           (when (string-prefix-p "2 " field) (setq skip 1))))))
    (list :staged staged :unstaged unstaged :untracked untracked :conflicted conflicted)))

(defun bndos-task-git-status (directory callback)
  "Call CALLBACK with DIRECTORY's change counts (see `bndos-task-git-parse-status')."
  (bndos-task-git directory '("status" "--porcelain=v2" "-z" "--untracked-files=all")
                  (lambda (result)
                    (funcall callback
                             (if (bndos-proc-ok-p result)
                                 (bndos-task-git-parse-status (plist-get result :stdout))
                               (list :error (bndos-proc-error-message result)))))))

(defun bndos-task-git-count-commits (directory from to callback)
  "Call CALLBACK with the number of commits in FROM..TO, or nil."
  (bndos-task-git directory (list "rev-list" "--count" (format "%s..%s" from to))
                  (lambda (result)
                    (funcall callback (and (bndos-proc-ok-p result)
                                           (string-to-number (plist-get result :stdout)))))))


;;; Diffs

(defun bndos-task-git-diff (directory from to callback)
  "Call CALLBACK with the parsed diff from tree-ish FROM to TO, or (:error MSG).
The result is a list of file plists (see `bndos-task-git-parse-diff')."
  (bndos-task-git directory
                  (list "diff" "--no-ext-diff" "--no-textconv" "-M" "--no-color"
                        "--src-prefix=a/" "--dst-prefix=b/" from to)
                  (lambda (result)
                    (funcall callback
                             (if (bndos-proc-ok-p result)
                                 (bndos-task-git-parse-diff (plist-get result :stdout))
                               (list :error (bndos-proc-error-message result)))))))

(defun bndos-task-git--unquote (path)
  "Undo git's C-style quoting of PATH, if any."
  (if (and (string-prefix-p "\"" path) (string-suffix-p "\"" path) (> (length path) 1))
      (let ((s (substring path 1 -1)) (out nil) (i 0))
        (while (< i (length s))
          (let ((c (aref s i)))
            (if (and (eq c ?\\) (< (1+ i) (length s)))
                (let ((n (aref s (1+ i))))
                  (cond
                   ((and (<= ?0 n ?7) (< (+ i 3) (length s)))
                    (push (string-to-number (substring s (1+ i) (+ i 4)) 8) out)
                    (cl-incf i 4))
                   (t (push (pcase n (?n ?\n) (?t ?\t) (?r ?\r) (?a ?\a) (?b ?\b) (?f ?\f)
                                   (?v ?\v) (_ n))
                            out)
                      (cl-incf i 2))))
              (push c out)
              (cl-incf i))))
        (decode-coding-string (apply #'unibyte-string
                                     (mapcan (lambda (ch)
                                               (if (< ch 256) (list ch)
                                                 (append (encode-coding-string (string ch) 'utf-8) nil)))
                                             (nreverse out)))
                              'utf-8))
    path))

(defun bndos-task-git--strip-prefix (path prefix)
  (let ((path (bndos-task-git--unquote path)))
    (if (string-prefix-p prefix path) (substring path (length prefix)) path)))

(defun bndos-task-git--parse-hunk-header (line)
  (when (string-match "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" line)
    (list :old-start (string-to-number (match-string 1 line))
          :old-count (if (match-string 2 line) (string-to-number (match-string 2 line)) 1)
          :new-start (string-to-number (match-string 3 line))
          :new-count (if (match-string 4 line) (string-to-number (match-string 4 line)) 1))))

(defun bndos-task-git-parse-diff (text)
  "Parse unified diff TEXT from `git diff' into a list of file plists:

  (:path P :old-path P :new-path P :status STATUS :binary BOOL
   :added N :removed N :hunks HUNKS)

STATUS is \"new file\", \"deleted\", \"renamed\", \"modified\" or \"mode\".
Each hunk is (:header STR :old-start N :old-count N :new-start N
:new-count N :lines LINES); each line is (:type added|removed|context|meta
:text STR :old N-or-nil :new N-or-nil)."
  (let (files file hunk old new)
    (cl-flet ((finish-hunk ()
                (when hunk
                  (setq file (plist-put file :hunks
                                        (cons (plist-put hunk :lines (nreverse (plist-get hunk :lines)))
                                              (plist-get file :hunks))))
                  (setq hunk nil)))
              (finish-file ()
                (when file
                  (let ((hunks (nreverse (plist-get file :hunks))))
                    (setq file (plist-put file :hunks hunks))
                    (setq file (plist-put file :path (or (plist-get file :new-path)
                                                         (plist-get file :old-path))))
                    (unless (plist-get file :status)
                      (setq file (plist-put file :status
                                            (if (or hunks (plist-get file :binary)) "modified" "mode"))))
                    (push file files))
                  (setq file nil))))
      (dolist (line (split-string text "\n"))
        (cond
         ((string-prefix-p "diff --git " line)
          (finish-hunk) (finish-file)
          (setq file (list :added 0 :removed 0 :hunks nil))
          ;; Fallback paths; overridden by ---/+++ or rename headers below.
          (when (string-match "\\`diff --git \\(\"?a/.*?\"?\\) \\(\"?b/.*\"?\\)\\'" line)
            (setq file (plist-put file :old-path (bndos-task-git--strip-prefix (match-string 1 line) "a/")))
            (setq file (plist-put file :new-path (bndos-task-git--strip-prefix (match-string 2 line) "b/")))))
         ((null file))
         ((and (null hunk) (string-prefix-p "new file mode" line))
          (setq file (plist-put file :status "new file")))
         ((and (null hunk) (string-prefix-p "deleted file mode" line))
          (setq file (plist-put file :status "deleted")))
         ((and (null hunk) (string-prefix-p "rename from " line))
          (setq file (plist-put file :status "renamed"))
          (setq file (plist-put file :old-path (bndos-task-git--unquote (substring line 12)))))
         ((and (null hunk) (string-prefix-p "rename to " line))
          (setq file (plist-put file :new-path (bndos-task-git--unquote (substring line 10)))))
         ((and (null hunk) (string-prefix-p "--- " line))
          (let ((p (substring line 4)))
            (unless (equal p "/dev/null")
              (setq file (plist-put file :old-path (bndos-task-git--strip-prefix p "a/"))))))
         ((and (null hunk) (string-prefix-p "+++ " line))
          (let ((p (substring line 4)))
            (unless (equal p "/dev/null")
              (setq file (plist-put file :new-path (bndos-task-git--strip-prefix p "b/"))))))
         ((and (null hunk) (or (string-prefix-p "Binary files " line)
                               (string-prefix-p "GIT binary patch" line)))
          (setq file (plist-put file :binary t)))
         ((string-prefix-p "@@ " line)
          (finish-hunk)
          (when-let* ((header (bndos-task-git--parse-hunk-header line)))
            (setq hunk (append (list :header line :lines nil) header)
                  old (plist-get header :old-start)
                  new (plist-get header :new-start))))
         ((null hunk))
         ((string-prefix-p "+" line)
          (push (list :type 'added :text (substring line 1) :old nil :new new)
                (plist-get hunk :lines))
          (setq file (plist-put file :added (1+ (plist-get file :added))))
          (cl-incf new))
         ((string-prefix-p "-" line)
          (push (list :type 'removed :text (substring line 1) :old old :new nil)
                (plist-get hunk :lines))
          (setq file (plist-put file :removed (1+ (plist-get file :removed))))
          (cl-incf old))
         ((string-prefix-p " " line)
          (push (list :type 'context :text (substring line 1) :old old :new new)
                (plist-get hunk :lines))
          (cl-incf old) (cl-incf new))
         ((string-prefix-p "\\" line)
          (push (list :type 'meta :text line :old nil :new nil)
                (plist-get hunk :lines)))))
      (finish-hunk)
      (finish-file))
    (nreverse files)))

(provide 'bndos-task-git)
;;; bndos-task-git.el ends here
