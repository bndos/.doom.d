;;; bndos-review-github.el --- A pull request's GitHub threads in diff-review, read only -*- lexical-binding: t; -*-

;;; Commentary:

;; A review of a branch with a pull request shows the pull request's review
;; threads with its own, read only (`diff-review-external-comments').  They
;; are fetched with gh, as the identity this project's environment gives,
;; and placed on the lines they were written on, in the blob of the commit
;; they were on, so the review follows them to today's lines or marks them
;; outdated.  Threads on GitHub's left side are on removed lines.  Nothing
;; is ever sent to GitHub.

;;; Code:

(require 'diff-review)

(declare-function bndos-gh-token "autoload")

(defcustom bndos-review-github-threads nil
  "Whether reviews show their pull request's GitHub review threads, read only."
  :type 'boolean
  :group 'diff-review)

(defcustom bndos-review-github-program "gh"
  "The GitHub CLI."
  :type 'string
  :group 'diff-review)

(defvar-local bndos-review-github--state nil
  "In a review: (:number N :url URL :threads N :resolved N :at TIME), (:error MESSAGE)
or (:fetching t).")
(defvar-local bndos-review-github--comments nil "In a review: the threads fetched, as comments.")
(defvar-local bndos-review-github--hidden nil "In a review: non-nil when its GitHub threads are hidden.")

(defconst bndos-review-github--query
  "query($owner: String!, $name: String!, $number: Int!, $after: String) {
  repository(owner: $owner, name: $name) { pullRequest(number: $number) {
    reviewThreads(first: 50, after: $after) {
      pageInfo { hasNextPage endCursor }
      nodes { id isResolved path diffSide originalLine originalStartLine
        comments(first: 100) { nodes {
          url body createdAt author { login } originalCommit { oid } } } } } } } }"
  "A page of a pull request's review threads.")

;;;; gh

(defun bndos-review-github--gh (args callback)
  "Run gh with ARGS in this buffer's directory and environment, as this
project's GitHub identity; call CALLBACK with the parsed JSON, or with
\(:error MESSAGE)."
  (let* ((token (ignore-errors (bndos-gh-token)))
         (process-environment (append (and token (list (concat "GH_TOKEN=" token)))
                                      '("GH_PROMPT_DISABLED=1" "NO_COLOR=1")
                                      process-environment))
         (out (generate-new-buffer " *bndos-review-github*"))
         (err (generate-new-buffer " *bndos-review-github-err*")))
    (make-process
     :name "bndos-review-github" :buffer out :stderr err :noquery t
     :command (cons bndos-review-github-program args)
     :sentinel (lambda (process _)
                 (unless (process-live-p process)
                   (let ((result (if (zerop (process-exit-status process))
                                     (with-current-buffer out
                                       (condition-case e
                                           (json-parse-string (buffer-string) :object-type 'hash-table
                                                              :null-object nil :false-object nil)
                                         (error (list :error (format "unreadable reply: %s" (error-message-string e))))))
                                   (list :error (with-current-buffer err (string-trim (buffer-string)))))))
                     (kill-buffer out)
                     (when (buffer-live-p err) (kill-buffer err))
                     (funcall callback result)))))))

(defun bndos-review-github--get (object &rest keys)
  (while (and keys (hash-table-p object))
    (setq object (gethash (pop keys) object)))
  (and (not keys) object))

;;;; Threads as comments

(defun bndos-review-github--git (&rest args)
  "ARGS' trimmed output in this directory, or nil."
  (with-temp-buffer
    (when (zerop (apply #'process-file "git" nil '(t nil) nil "--no-optional-locks" args))
      (let ((out (string-trim (buffer-string)))) (unless (string-empty-p out) out)))))

(defun bndos-review-github--message (comment)
  (let ((m (make-hash-table :test #'equal)))
    (puthash "author" (concat "@" (or (bndos-review-github--get comment "author" "login") "ghost")) m)
    (puthash "at" (gethash "createdAt" comment) m)
    (puthash "body" (or (gethash "body" comment) "") m)
    m))

(defun bndos-review-github--comment (thread base)
  "THREAD from GitHub as a comment of the review, its lines where they were
written: in the blob of its commit or, on a removed line, of that commit's
merge base with BASE, the pull request's base.  Without that blob here it is
outdated: drawn where it was, not followed.  `default-directory' is the
review's root."
  (let* ((comments (append (bndos-review-github--get thread "comments" "nodes") nil))
         (c (bndos-review-github--message (car comments)))
         (path (gethash "path" thread))
         (last (gethash "originalLine" thread))
         (commit (bndos-review-github--get (car comments) "originalCommit" "oid"))
         (old (equal (gethash "diffSide" thread) "LEFT")))
    (puthash "id" (concat "github:" (gethash "id" thread)) c)
    (puthash "path" path c)
    (puthash "external" "GitHub" c)
    (when-let* ((url (gethash "url" (car comments)))) (puthash "url" url c))
    (when last
      (puthash "lines" (vector (or (gethash "originalStartLine" thread) last) last) c)
      (let* ((rev (if old (and commit base (bndos-review-github--git "merge-base" base commit)) commit))
             (blob (and rev (bndos-review-github--git "rev-parse" "--verify" "--quiet" (concat rev ":" path)))))
        (if (not blob)
            (puthash "outdated" t c)
          (puthash "blob" blob c)
          (when old (puthash "base-blob" blob c))))
      (when old (puthash "side" "old" c)))
    (puthash "replies" (vconcat (mapcar #'bndos-review-github--message (cdr comments))) c)
    (when (gethash "isResolved" thread) (puthash "resolved" t c))
    c))

;;;; Which pull request

(defconst bndos-review-github--fields "number,url,state,baseRefName,baseRefOid"
  "What is asked of a pull request.")

(defun bndos-review-github--lookup ()
  "The gh arguments that find this review's pull request, or nil.
A review of the worktree asks for its checked-out branch, as gh tracks it
(upstream, push remote, forks); a review of a branch names it; a review of
commits has no pull request."
  (cond ((and (not diff-review--tip) (not diff-review--tip-tree)
              (bndos-review-github--git "symbolic-ref" "--quiet" "HEAD"))
         (list "pr" "view" "--json" bndos-review-github--fields))
        ((and diff-review--tip diff-review--head-name
              (bndos-review-github--git "rev-parse" "--verify" "--quiet"
                                        (concat "refs/heads/" diff-review--head-name)))
         (list "pr" "view" diff-review--head-name "--json" bndos-review-github--fields))))

(defun bndos-review-github--remote (owner name)
  "The remote of this repository that points at OWNER/NAME on GitHub, or origin."
  (let ((pattern (format "github\\.com[:/]%s/%s\\(?:\\.git\\)?/?\\'" (regexp-quote owner) (regexp-quote name))))
    (or (seq-some (lambda (remote)
                    (let ((url (bndos-review-github--git "remote" "get-url" remote)))
                      (and url (string-match-p pattern url) remote)))
                  (split-string (or (bndos-review-github--git "remote") "") "\n" t))
        "origin")))

(defun bndos-review-github--missing (oids)
  "Those of OIDS that aren't commits here."
  (seq-remove (lambda (oid)   ; by its exit status: it prints nothing either way
                (zerop (process-file "git" nil nil nil "cat-file" "-e" (concat oid "^{commit}"))))
              (seq-uniq (delq nil oids))))

(defun bndos-review-github--fetch-commits (remote number base-ref callback)
  "Fetch pull request NUMBER's commits and BASE-REF from REMOTE; then CALLBACK."
  (make-process
   :name "bndos-review-github-fetch" :noquery t :buffer nil
   :command (list "git" "fetch" "--quiet" "--no-tags" "--no-write-fetch-head" remote
                  (format "refs/pull/%d/head" number) (concat "refs/heads/" base-ref))
   :sentinel (lambda (process _) (unless (process-live-p process) (funcall callback)))))

;;;; The review

(defun bndos-review-github--show (review)
  "Give REVIEW its GitHub threads, or none when hidden, and redraw it."
  (when (buffer-live-p review)
    (with-current-buffer review
      (setq diff-review-external-comments (unless bndos-review-github--hidden bndos-review-github--comments))
      (diff-review--render-all)
      (diff-review--redecorate-files))))

(defun bndos-review-github--fail (review message)
  (when (buffer-live-p review)
    (with-current-buffer review
      (setq bndos-review-github--state (list :error message :manual (plist-get bndos-review-github--state :manual)))
      (diff-review--render-all))))

(defun bndos-review-github--threads (review owner name number after acc done)
  "Fetch the review threads of pull request NUMBER page by page, then DONE."
  (with-current-buffer review
    (bndos-review-github--gh
     (append (list "api" "graphql" "-f" (concat "query=" bndos-review-github--query)
                   "-f" (concat "owner=" owner) "-f" (concat "name=" name)
                   "-F" (format "number=%d" number))
             (and after (list "-f" (concat "after=" after))))
     (lambda (data)
       (if (plist-get data :error)
           (bndos-review-github--fail review (plist-get data :error))
         (let* ((page (bndos-review-github--get data "data" "repository" "pullRequest" "reviewThreads"))
                (acc (append acc (append (gethash "nodes" page) nil))))
           (if (bndos-review-github--get page "pageInfo" "hasNextPage")
               (bndos-review-github--threads review owner name number
                                             (bndos-review-github--get page "pageInfo" "endCursor") acc done)
             (funcall done acc))))))))

(defun bndos-review-github-refresh (&optional review manual)
  "Fetch REVIEW's pull request threads from GitHub (default this buffer's review).
MANUAL (interactively, or from a refresh by hand) also reports a review
without a pull request, and shows a closed one's threads; otherwise those
stay quiet."
  (interactive (list nil t))
  (let ((review (or review (diff-review--in-review (current-buffer)))))
    (with-current-buffer review
      (let* ((default-directory diff-review--root)
             (lookup (bndos-review-github--lookup)))
        (if (not lookup)
            (progn (setq bndos-review-github--state (list :manual manual))
                   (bndos-review-github--fail review "a review of commits, not a branch"))
          (setq bndos-review-github--state (list :fetching t :manual manual))
          (bndos-review-github--gh
           lookup
           (lambda (pr)
             (cond
              ((plist-get pr :error)
               (bndos-review-github--fail review (if (string-match-p "no pull requests" (plist-get pr :error))
                                                     "no pull request for this branch"
                                                   (plist-get pr :error))))
              ((not (string-match "github\\.com/\\([^/]+\\)/\\([^/]+\\)/pull/" (gethash "url" pr "")))
               (bndos-review-github--fail review "not a GitHub pull request"))
              (t (bndos-review-github--load review pr (match-string 1 (gethash "url" pr))
                                            (match-string 2 (gethash "url" pr)) manual))))))))))

(defun bndos-review-github--load (review pr owner name manual)
  "Fetch PR's threads into REVIEW, its missing commits first."
  (let ((number (gethash "number" pr))
        (base (gethash "baseRefOid" pr)))
    (bndos-review-github--threads
     review owner name number nil nil
     (lambda (threads)
       (when (buffer-live-p review)
         (with-current-buffer review
           (let* ((default-directory diff-review--root)
                  (commits (cons base (mapcar (lambda (th) (bndos-review-github--get
                                                            (car (append (bndos-review-github--get th "comments" "nodes") nil))
                                                            "originalCommit" "oid"))
                                              threads)))
                  (finish
                   (lambda ()
                     (when (buffer-live-p review)
                       (with-current-buffer review
                         (let* ((default-directory diff-review--root)
                                (base (if (bndos-review-github--missing (list base)) diff-review--base base))
                                (comments (mapcar (lambda (th) (bndos-review-github--comment th base)) threads))
                                (open (equal (gethash "state" pr) "OPEN")))
                           (setq bndos-review-github--comments comments
                                 ;; A closed pull request's threads wait to be asked for.
                                 bndos-review-github--hidden (and (not open) (not manual))
                                 bndos-review-github--state
                                 (list :number number :url (gethash "url" pr)
                                       :pr-state (downcase (or (gethash "state" pr) "?"))
                                       :threads (length comments)
                                       :resolved (seq-count (lambda (c) (gethash "resolved" c)) comments)
                                       :outdated (seq-count (lambda (c) (gethash "outdated" c)) comments)
                                       :at (current-time) :manual manual))
                           (bndos-review-github--show review)))))))
             (if (bndos-review-github--missing commits)
                 (bndos-review-github--fetch-commits (bndos-review-github--remote owner name) number
                                                     (or (gethash "baseRefName" pr) "main") finish)
               (funcall finish)))))))))

(defun bndos-review-github-toggle ()
  "Show or hide this review's GitHub threads."
  (interactive)
  (diff-review--in-review
    (setq bndos-review-github--hidden (not bndos-review-github--hidden))
    (if (or bndos-review-github--comments bndos-review-github--hidden)
        (bndos-review-github--show (current-buffer))
      (bndos-review-github-refresh (current-buffer)))
    (message "GitHub threads %s" (if bndos-review-github--hidden "hidden" "shown"))))

(defun bndos-review-github--overview ()
  "The GitHub line of the overview (`diff-review-overview-functions')."
  (when-let* ((state bndos-review-github--state)
              ((or (plist-get state :number) (plist-get state :manual))))
    (insert (propertize (format "%-9s " "GitHub") 'face 'font-lock-keyword-face)
            (cond
             ((plist-get state :fetching) (propertize "fetching…" 'face 'shadow))
             ((plist-get state :error) (propertize (plist-get state :error) 'face 'shadow))
             (t (concat (format "PR #%d" (plist-get state :number))
                        (let ((pr-state (plist-get state :pr-state)))
                          (if (member pr-state '(nil "open")) ""
                            (propertize (concat " " pr-state) 'face 'warning)))
                        (format " · %d thread%s (%d resolved%s)"
                                (plist-get state :threads)
                                (if (= 1 (plist-get state :threads)) "" "s")
                                (plist-get state :resolved)
                                (if (> (or (plist-get state :outdated) 0) 0)
                                    (format ", %d outdated" (plist-get state :outdated)) ""))
                        (propertize (format " · fetched %s" (format-time-string "%H:%M" (plist-get state :at)))
                                    'face 'shadow)
                        (if bndos-review-github--hidden (propertize " · hidden" 'face 'warning) ""))))
            "\n\n")))

(defun bndos-review-github--maybe-fetch ()
  "Fetch the threads of a review that opens, with `bndos-review-github-threads'."
  (when (and bndos-review-github-threads diff-review--root (not bndos-review-github--state))
    (bndos-review-github-refresh (current-buffer))))

(defun bndos-review-github--after-refresh (&rest _)
  "Fetch again when the review is refreshed by hand."
  (when (and bndos-review-github-threads (ignore-errors (diff-review--review-buffer)))
    (bndos-review-github-refresh (diff-review--review-buffer) t)))

(add-hook 'diff-review-overview-functions #'bndos-review-github--overview)
(add-hook 'diff-review-mode-hook #'bndos-review-github--maybe-fetch)
(advice-add 'diff-review-refresh :after #'bndos-review-github--after-refresh)

(provide 'bndos-review-github)
;;; bndos-review-github.el ends here
