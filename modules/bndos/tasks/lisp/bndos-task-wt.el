;;; bndos-task-wt.el --- Asynchronous worktrunk (wt) bridge -*- lexical-binding: t; -*-

;;; Commentary:

;; Worktrees for tasks come from worktrunk's machine-readable commands:
;; `wt list --format json' for the inventory and
;; `wt switch --create BRANCH --base BASE --no-cd --format json' to create one
;; (without --create, to bring back a gone task's worktree for its branch).
;; `--yes' is never passed, so a repository's hooks still need the user's
;; approval (`wt config approvals add'); unapproved hooks make creation fail
;; with worktrunk's explanation instead of running.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'bndos-proc)

(defcustom bndos-task-wt-program "wt"
  "The worktrunk executable."
  :type 'string
  :group 'bndos-task)

(defconst bndos-task-wt-tested-version "0.81.0"
  "The worktrunk version this bridge was developed and tested against.")

(defconst bndos-task-wt-list-schema 2
  "The `wt list --format json' schema this bridge understands.")

(defvar bndos-task-wt--capabilities nil
  "Cached (PROGRAM-PATH . CAPABILITIES).")

(defun bndos-task-wt-error-p (value)
  (and (consp value) (keywordp (car value)) (plist-get value :error)))

(defun bndos-task-wt--message (result)
  "Worktrunk's own explanation from RESULT's stderr, or a generic message."
  (let ((lines (seq-filter (lambda (l) (string-match-p "\\`[✗↳]" l))
                           (split-string (or (plist-get result :stderr) "") "\n" t))))
    (if lines
        (string-join (mapcar #'string-trim lines) " ")
      (bndos-proc-error-message result))))

(cl-defun bndos-task-wt--call (args &key directory json callback)
  "Run wt ARGS (with -C DIRECTORY); CALLBACK gets stdout, parsed JSON, or an error."
  (bndos-proc-run bndos-task-wt-program
                  (append (and directory (list "-C" (expand-file-name directory))) args)
                  :directory (or directory temporary-file-directory)
                  :callback
                  (lambda (result)
                    (funcall callback
                             (cond
                              ((plist-get result :error)
                               (list :error 'missing-executable :message (plist-get result :error)))
                              ((not (bndos-proc-ok-p result))
                               (list :error 'command-failed :message (bndos-task-wt--message result)))
                              ((not json) (plist-get result :stdout))
                              (t (condition-case err
                                     (json-parse-string (plist-get result :stdout)
                                                        :object-type 'plist :array-type 'list
                                                        :null-object nil :false-object nil)
                                   (error (list :error 'invalid-json
                                                :message (format "wt %s: invalid JSON (%s)" (car args)
                                                                 (error-message-string err)))))))))))

(defun bndos-task-wt-capabilities (callback)
  "Call CALLBACK with (:version V :missing FLAGS) or an error plist."
  (let ((path (executable-find bndos-task-wt-program)))
    (cond
     ((not path)
      (run-at-time 0 nil callback
                   (list :error 'missing-executable
                         :message (format "`%s' (worktrunk) not found in exec-path; install worktrunk %s or newer"
                                          bndos-task-wt-program bndos-task-wt-tested-version))))
     ((equal (car bndos-task-wt--capabilities) path)
      (run-at-time 0 nil callback (cdr bndos-task-wt--capabilities)))
     (t
      (bndos-task-wt--call
       '("--version")
       :callback
       (lambda (out)
         (if (bndos-task-wt-error-p out)
             (funcall callback out)
           (let ((version (string-remove-prefix "v" (car (last (split-string (string-trim out))))))
                 (pending 2)
                 (missing nil))
             (pcase-dolist (`(,cmd . ,flags) '(("switch" "--create" "--base" "--no-cd" "--format")
                                               ("list" "--format")))
               (bndos-task-wt--call
                (list cmd "--help")
                :callback
                (lambda (help)
                  (dolist (flag flags)
                    (unless (and (stringp help) (string-search flag help))
                      (push (format "wt %s %s" cmd flag) missing)))
                  (when (zerop (cl-decf pending))
                    (let ((caps (list :version version :missing missing)))
                      (setq bndos-task-wt--capabilities (cons path caps))
                      (funcall callback caps))))))))))))))

(defun bndos-task-wt-capability-problem (caps)
  "A user-facing message when CAPS can't support this bridge, else nil."
  (cond
   ((bndos-task-wt-error-p caps) (plist-get caps :message))
   ((plist-get caps :missing)
    (format "worktrunk %s lacks %s; tested with %s" (plist-get caps :version)
            (string-join (plist-get caps :missing) ", ") bndos-task-wt-tested-version))))

(defun bndos-task-wt-list (repo callback)
  "Call CALLBACK with REPO's worktrees, or an error plist.
Each is a plist (:branch :path :main :changes (:staged :modified :untracked
...) :head (:sha ...)), flattened from `wt list --format json'."
  (bndos-task-wt--call
   '("list" "--format" "json") :directory repo :json t
   :callback
   (lambda (out)
     (funcall callback
              (cond
               ((bndos-task-wt-error-p out) out)
               ((not (eql (plist-get out :schema) bndos-task-wt-list-schema))
                (list :error 'unsupported-schema
                      :message (format "wt list schema %S; this bridge understands %s"
                                       (plist-get out :schema) bndos-task-wt-list-schema)))
               (t (delq nil
                        (mapcar (lambda (item)
                                  (when-let* ((wt (plist-get item :worktree))
                                              (path (plist-get wt :path)))
                                    (list :branch (plist-get item :branch)
                                          :path (file-name-as-directory path)
                                          :main (plist-get wt :main)
                                          :changes (plist-get wt :changes)
                                          :head (plist-get item :head))))
                                (plist-get out :items)))))))))

(defun bndos-task-wt--switch (repo args callback)
  "Run wt switch ARGS in REPO; CALLBACK gets its JSON with :path, or an error plist."
  (bndos-task-wt--call (append '("switch") args '("--no-cd" "--format" "json"))
                       :directory repo :json t
                       :callback
                       (lambda (out)
                         (funcall callback
                                  (cond
                                   ((bndos-task-wt-error-p out) out)
                                   ((plist-get out :path)
                                    (plist-put out :path (file-name-as-directory (plist-get out :path))))
                                   (t
                                    (list :error 'unexpected-output
                                          :message (format "wt switch printed %S" out))))))))

(defun bndos-task-wt-create (repo branch base callback)
  "Create BRANCH from BASE in a new worktree of REPO.
CALLBACK gets (:path DIR :branch B :base_branch BASE ...) or an error plist."
  (bndos-task-wt--switch repo (list "--create" branch "--base" base) callback))

(defun bndos-task-wt-restore (repo branch callback)
  "Make a worktree of REPO for existing BRANCH again.
CALLBACK gets (:path DIR :branch B ...) or an error plist."
  (bndos-task-wt--switch repo (list branch) callback))

(provide 'bndos-task-wt)
;;; bndos-task-wt.el ends here
