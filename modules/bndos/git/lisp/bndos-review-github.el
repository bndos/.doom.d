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

(require 'cl-lib)
(require 'diff-review)

(declare-function bndos-gh-token "autoload")
(declare-function bndos-review-outbox-comments "bndos-review-outbox")
(declare-function bndos-review-outbox-live-count "bndos-review-outbox")
(declare-function bndos-review-outbox-reconcile "bndos-review-outbox")

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

(defconst bndos-review-github--comment-fields
  "pageInfo { hasNextPage endCursor }
   nodes { databaseId url body createdAt author { login } originalCommit { oid } }"
  "What is asked of a page of a thread's comments.")

(defconst bndos-review-github--query
  (concat "query($owner: String!, $name: String!, $number: Int!, $after: String) {
  repository(owner: $owner, name: $name) { pullRequest(number: $number) {
    reviewThreads(first: 50, after: $after) {
      pageInfo { hasNextPage endCursor }
      nodes { id isResolved isOutdated path diffSide originalLine originalStartLine
        comments(first: 100) { " bndos-review-github--comment-fields " } } } } } }")
  "A page of a pull request's review threads.")

(defconst bndos-review-github--comments-query
  (concat "query($id: ID!, $after: String) {
  node(id: $id) { ... on PullRequestReviewThread {
    comments(first: 100, after: $after) { " bndos-review-github--comment-fields " } } } }")
  "A further page of a review thread's comments.")

(defconst bndos-review-github--conversation-query
  "query($owner: String!, $name: String!, $number: Int!) {
  viewer { login }
  repository(owner: $owner, name: $name) { pullRequest(number: $number) {
    author { login }
    comments(last: 100) { nodes { id url body createdAt author { login } } }
    reviews(last: 100) { nodes { id url body state submittedAt author { login } } }
    pending: reviews(states: [PENDING]) { totalCount } } } }"
  "A pull request's latest comments and reviews, the ones on no lines; and
whether one of mine is pending (only its author sees a pending review); who
posts, and whose the pull request is.")

(defconst bndos-review-github--review-chips
  '(("APPROVED" "Approved" success) ("CHANGES_REQUESTED" "Changes requested" error)
    ("COMMENTED" "Review" diff-review-state-note) ("DISMISSED" "Dismissed" shadow))
  "(STATE CHIP FACE) for a review's summary.")

;;;; gh

(defun bndos-review-github--gh (args callback &optional raw)
  "Run gh with ARGS in this buffer's directory and environment, as this
project's GitHub identity; call CALLBACK with the parsed JSON, or with
\(:error MESSAGE).  With RAW, with (:exit CODE :out STDOUT :err STDERR)
instead, CODE nil when gh couldn't run to its end."
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
                   (let ((result (cond
                                  (raw
                                   (list :exit (and (eq (process-status process) 'exit)
                                                    (process-exit-status process))
                                         :out (with-current-buffer out (buffer-string))
                                         :err (with-current-buffer err (string-trim (buffer-string)))))
                                  ((zerop (process-exit-status process))
                                     (with-current-buffer out
                                       (condition-case e
                                           (json-parse-string (buffer-string) :object-type 'hash-table
                                                              :null-object nil :false-object nil)
                                         (error (list :error (format "unreadable reply: %s" (error-message-string e)))))))
                                  (t (list :error (with-current-buffer err (string-trim (buffer-string))))))))
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

(defconst bndos-review-github-marker-regexp "\n*<!-- bndos-draft:\\([A-Za-z0-9-]+\\) -->[ \t\n]*\\'"
  "The marker a reply sent from the outbox ends with: the draft's id.")

(defun bndos-review-github--message (comment)
  "A message of a thread: its body without the outbox's marker, which is
kept as \"sent-draft\" with its \"url\"."
  (let ((m (make-hash-table :test #'equal))
        (body (or (gethash "body" comment) "")))
    (puthash "author" (concat "@" (or (bndos-review-github--get comment "author" "login") "ghost")) m)
    (puthash "at" (gethash "createdAt" comment) m)
    (when (string-match bndos-review-github-marker-regexp body)
      (puthash "sent-draft" (match-string 1 body) m)
      (puthash "url" (gethash "url" comment) m)
      (setq body (substring body 0 (match-beginning 0))))
    (puthash "body" body m)
    m))

(defun bndos-review-github--sent-drafts (comments)
  "The drafts COMMENTS show on GitHub, as (ID URL WHERE): WHERE the id of
the thread it is in, `conversation', or `review' (a review's summary)."
  (let (sent)
    (dolist (c comments)
      (let ((where (cond ((gethash "thread-id" c))
                         ((gethash "conversation" c) 'conversation)
                         ((gethash "review" c) 'review))))
        (dolist (m (cons c (append (gethash "replies" c) nil)))
          (when-let* ((id (gethash "sent-draft" m))
                      ((not (gethash "draft" m))))
            (push (list id (gethash "url" m) where) sent)))))
    sent))

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
    (puthash "thread-id" (gethash "id" thread) c)
    (when commit (puthash "commit" commit c))
    (when-let* ((first (gethash "databaseId" (car comments)))) (puthash "reply-to" first c))
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
    (when (gethash "isOutdated" thread) (puthash "github-outdated" t c))
    c))

(defun bndos-review-github--note (node kind)
  "A comment on the pull request or a review's summary, NODE, as a note of
the review: on no lines, nothing to resolve.  KIND is `comment' or `review'."
  (let ((c (bndos-review-github--message node))
        (chip (if (eq kind 'review)
                  (cdr (assoc (gethash "state" node) bndos-review-github--review-chips))
                (list "Comment" 'diff-review-state-note))))
    (when (eq kind 'review) (puthash "at" (gethash "submittedAt" node) c))
    (puthash "id" (concat "github:" (gethash "id" node)) c)
    (puthash "external" "GitHub" c)
    (puthash "note" t c)
    (puthash (if (eq kind 'review) "review" "conversation") t c)
    (puthash "where" "conversation" c)
    (puthash "chip" (car chip) c)
    (puthash "chip-face" (cadr chip) c)
    (when-let* ((url (gethash "url" node))) (puthash "url" url c))
    c))

(defun bndos-review-github--notes (pull-request)
  "PULL-REQUEST's comments and reviews with something to say, as notes."
  (append
   (mapcar (lambda (n) (bndos-review-github--note n 'comment))
           (bndos-review-github--get pull-request "comments" "nodes"))
   (delq nil (mapcar (lambda (n)
                       (and (assoc (gethash "state" n) bndos-review-github--review-chips)
                            (or (not (string-empty-p (or (gethash "body" n) "")))
                                (member (gethash "state" n) '("APPROVED" "CHANGES_REQUESTED")))
                            (bndos-review-github--note n 'review)))
                     (bndos-review-github--get pull-request "reviews" "nodes")))))

;;;; Which pull request

(defconst bndos-review-github--fields "number,url,state,baseRefName,baseRefOid,headRefOid"
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

(defvar-local bndos-review-github-borrow nil
  "In a review showing some of another review's threads: (SOURCE . KEEP),
SOURCE that review's buffer, KEEP a predicate on its threads and comments.
Such a review fetches nothing: it shows what SOURCE has.")
(put 'bndos-review-github-borrow 'permanent-local t)

(defvar bndos-review-github--borrow-next nil
  "The `bndos-review-github-borrow' of the review being opened.")

(defun bndos-review-github--show (review)
  "Give REVIEW its GitHub threads, or none when hidden, and redraw it."
  (if (and (buffer-live-p review) (buffer-local-value 'bndos-review-github-borrow review))
      (bndos-review-github--borrow review)
  (when (buffer-live-p review)
    (with-current-buffer review
      (setq diff-review-external-comments
            (unless bndos-review-github--hidden
              (append bndos-review-github--comments
                      ;; Comments on the conversation not sent yet.
                      (and (fboundp 'bndos-review-outbox-comments) (bndos-review-outbox-comments review)))))
      (diff-review--render-all)
      (diff-review--redecorate-files))
    ;; And the reviews showing some of its threads.
    (dolist (other (buffer-list))
      (when (eq (car (buffer-local-value 'bndos-review-github-borrow other)) review)
        (bndos-review-github--borrow other))))))

(defun bndos-review-github--known (state)
  "What STATE knows of the pull request, kept while it is fetched again or
a fetch fails: drafts are found by it."
  (cl-loop for key in '(:number :url :owner :name :pr-state :threads :resolved :outdated :notes :pending-review :head-oid :base-oid :viewer :author :at)
           when (plist-member state key) append (list key (plist-get state key))))

(defun bndos-review-github--fail (review message)
  (when (buffer-live-p review)
    (with-current-buffer review
      (setq bndos-review-github--state (append (list :error message :manual (plist-get bndos-review-github--state :manual))
                                               (bndos-review-github--known bndos-review-github--state)))
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
             (bndos-review-github--all-comments review acc (lambda () (funcall done acc))))))))))

(defun bndos-review-github--all-comments (review threads done)
  "Fetch the comments past the first page of each of THREADS into it, then DONE."
  (let ((thread (seq-find (lambda (th) (bndos-review-github--get th "comments" "pageInfo" "hasNextPage"))
                          threads)))
    (if (not thread)
        (funcall done)
      (with-current-buffer review
        (bndos-review-github--gh
         (list "api" "graphql" "-f" (concat "query=" bndos-review-github--comments-query)
               "-f" (concat "id=" (gethash "id" thread))
               "-f" (concat "after=" (bndos-review-github--get thread "comments" "pageInfo" "endCursor")))
         (lambda (data)
           (let ((page (bndos-review-github--get data "data" "node" "comments"))
                 (comments (gethash "comments" thread)))
             (if (or (plist-get data :error) (not (hash-table-p page)))
                 (bndos-review-github--fail review (or (plist-get data :error) "a thread's comments couldn't be read"))
               (puthash "nodes" (vconcat (gethash "nodes" comments) (gethash "nodes" page)) comments)
               (puthash "pageInfo" (gethash "pageInfo" page) comments)
               (bndos-review-github--all-comments review threads done)))))))))

(defun bndos-review-github-refresh (&optional review manual)
  "Fetch REVIEW's pull request threads from GitHub (default this buffer's review).
MANUAL (interactively, or from a refresh by hand) also reports a review
without a pull request, and shows a closed one's threads; otherwise those
stay quiet."
  (interactive (list nil t))
  (let ((review (or review (diff-review--in-review (current-buffer)))))
    (if (buffer-local-value 'bndos-review-github-borrow review)
        (bndos-review-github--borrow review)
    (with-current-buffer review
      (let* ((default-directory diff-review--root)
             (lookup (bndos-review-github--lookup)))
        (if (not lookup)
            (progn (setq bndos-review-github--state (list :manual manual))
                   (bndos-review-github--fail review "a review of commits, not a branch"))
          (setq bndos-review-github--state (append (list :fetching t :manual manual)
                                                   (bndos-review-github--known bndos-review-github--state)))
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
                                            (match-string 2 (gethash "url" pr)) manual)))))))))))

(defun bndos-review-github--conversation (review owner name number done)
  "Fetch pull request NUMBER's comments and reviews, then DONE with them as
notes, whether a review of mine is pending, and (VIEWER AUTHOR): who posts,
whose the pull request is."
  (with-current-buffer review
    (bndos-review-github--gh
     (list "api" "graphql" "-f" (concat "query=" bndos-review-github--conversation-query)
           "-f" (concat "owner=" owner) "-f" (concat "name=" name) "-F" (format "number=%d" number))
     (lambda (data)
       (if (plist-get data :error)
           (bndos-review-github--fail review (plist-get data :error))
         (let ((pull-request (bndos-review-github--get data "data" "repository" "pullRequest")))
           (funcall done (bndos-review-github--notes pull-request)
                    (> (or (bndos-review-github--get pull-request "pending" "totalCount") 0) 0)
                    (list (bndos-review-github--get data "data" "viewer" "login")
                          (bndos-review-github--get pull-request "author" "login")))))))))

(defun bndos-review-github--load (review pr owner name manual)
  "Fetch PR's threads, comments and reviews into REVIEW, its missing commits first."
  (let ((number (gethash "number" pr))
        (base (gethash "baseRefOid" pr)))
    (bndos-review-github--threads
     review owner name number nil nil
     (lambda (threads)
      (bndos-review-github--conversation
       review owner name number
       (lambda (notes pending people)
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
                           (setq bndos-review-github--comments (append comments notes)
                                 ;; A closed pull request's threads wait to be asked for.
                                 bndos-review-github--hidden (and (not open) (not manual))
                                 bndos-review-github--state
                                 (list :number number :url (gethash "url" pr) :owner owner :name name
                                       :pr-state (downcase (or (gethash "state" pr) "?"))
                                       :threads (length comments)
                                       :resolved (seq-count (lambda (c) (gethash "resolved" c)) comments)
                                       :outdated (seq-count (lambda (c) (gethash "outdated" c)) comments)
                                       :notes (length notes) :pending-review pending
                                       :viewer (car people) :author (cadr people)
                                       :head-oid (gethash "headRefOid" pr) :base-oid (gethash "baseRefOid" pr)
                                       :at (current-time) :manual manual))
                           (when (fboundp 'bndos-review-outbox-reconcile)
                             (bndos-review-outbox-reconcile review (bndos-review-github--sent-drafts bndos-review-github--comments)))
                           (bndos-review-github--show review)))))))
             (if (bndos-review-github--missing commits)
                 (bndos-review-github--fetch-commits (bndos-review-github--remote owner name) number
                                                     (or (gethash "baseRefName" pr) "main") finish)
               (funcall finish)))))))))))

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
                        (format " · %d thread%s (%d resolved%s)%s"
                                (plist-get state :threads)
                                (if (= 1 (plist-get state :threads)) "" "s")
                                (plist-get state :resolved)
                                (if (> (or (plist-get state :outdated) 0) 0)
                                    (format ", %d outdated" (plist-get state :outdated)) "")
                                (if (> (or (plist-get state :notes) 0) 0)
                                    (format " · %d comment%s" (plist-get state :notes)
                                            (if (= 1 (plist-get state :notes)) "" "s"))
                                  ""))
                        (propertize (format " · fetched %s" (format-time-string "%H:%M" (plist-get state :at)))
                                    'face 'shadow)
                        (if bndos-review-github--hidden (propertize " · hidden" 'face 'warning) "")
                        (if-let* (((fboundp 'bndos-review-outbox-live-count))
                                  (n (bndos-review-outbox-live-count (current-buffer)))
                                  ((> n 0)))
                            (concat (propertize (format " · ✎ %d draft%s" n (if (= n 1) "" "s")) 'face 'diff-review-draft)
                                    (propertize (substitute-command-keys " (\\[bndos-review-send] sends)") 'face 'shadow))
                          ""))))
            "\n\n")))

(defun bndos-review-github--borrow (review)
  "Give REVIEW the threads its source has that it keeps, and redraw it."
  (when (buffer-live-p review)
    (with-current-buffer review
      (pcase-let ((`(,source . ,keep) bndos-review-github-borrow))
        (let ((live (buffer-live-p source)))
          (setq bndos-review-github--state (and live (copy-sequence (buffer-local-value 'bndos-review-github--state source)))
                bndos-review-github--comments (and live (seq-filter keep (buffer-local-value 'bndos-review-github--comments source)))
                bndos-review-github--hidden nil
                diff-review-external-comments
                (append bndos-review-github--comments
                        (and (fboundp 'bndos-review-outbox-comments) (seq-filter keep (bndos-review-outbox-comments review))))))
        (diff-review--render-all)
        (diff-review--redecorate-files)))))

(defun bndos-review-github--maybe-fetch ()
  "Fetch the threads of a review that opens, with `bndos-review-github-threads'."
  (when bndos-review-github--borrow-next
    (setq bndos-review-github-borrow bndos-review-github--borrow-next))
  (cond
   (bndos-review-github-borrow
    ;; Once the review has drawn itself.
    (run-at-time 0 nil #'bndos-review-github--borrow (current-buffer)))
   ((and bndos-review-github-threads diff-review--root (not bndos-review-github--state))
    (bndos-review-github-refresh (current-buffer)))))

(defun bndos-review-github--after-refresh (&rest _)
  "Fetch again when the review is refreshed by hand."
  (when (and bndos-review-github-threads (ignore-errors (diff-review--review-buffer)))
    (bndos-review-github-refresh (diff-review--review-buffer) t)))

(add-hook 'diff-review-overview-functions #'bndos-review-github--overview)
(add-hook 'diff-review-mode-hook #'bndos-review-github--maybe-fetch)
(advice-add 'diff-review-refresh :after #'bndos-review-github--after-refresh)

(provide 'bndos-review-github)
;;; bndos-review-github.el ends here
