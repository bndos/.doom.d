;;; bndos-task-review.el --- Human review of a task worktree -*- lexical-binding: t; -*-

;;; Commentary:

;; One review buffer per task (repository + worktree), built on Hutch's
;; `hutch-mode'.  Hutch keeps its optional gptel findings and suggestion
;; states; this file renders, through Hutch's render hooks, the task header,
;; human discussion threads, and the task's full diff.
;;
;; The default scope compares the task's merge-base with its recorded parent
;; (not a hardcoded main) to a snapshot of the whole worktree, so committed
;; work and later uncommitted revisions are both visible.  Other scopes split
;; that up (committed / uncommitted / staged / unstaged) or show only the
;; latest iteration.  A magit-treediff tree follows the buffer.
;;
;; Discussion threads are stored by `bndos-task-store' and are distinct from
;; Hutch's suggestion states (pending/queued/applied/dismissed).  Talking to
;; the implementing agent lives in `bndos-task-agent'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit-section)
(require 'magit-treediff)
(require 'hutch)
(require 'bndos-proc)
(require 'bndos-task-store)
(require 'bndos-task-git)

(declare-function agent-shell-insert "ext:agent-shell")
(declare-function bndos-task-agent-summary "bndos-task-agent")
(declare-function bndos-task-agent-submit "bndos-task-agent")
(declare-function bndos-task-agent-open "bndos-task-agent")
(declare-function bndos-task-agent-ensure "bndos-task-agent")
(declare-function +workspaces-switch-to-project-h "ext:workspaces")
(declare-function projectile-add-known-project "ext:projectile")
(defvar +workspaces-switch-project-function)
(defvar +workspaces-on-switch-project-behavior)

(defcustom bndos-task-review-collapse-lines 600
  "Files with more diff lines than this start collapsed."
  :type 'integer
  :group 'bndos-task)

(defcustom bndos-task-review-show-tree t
  "Whether opening a review also shows its following file tree."
  :type 'boolean
  :group 'bndos-task)

(defconst bndos-task-review-scopes
  '((full        "Whole task"
                 "merge-base with the parent -> worktree: commits, staged, unstaged and untracked")
    (committed   "Committed"
                 "merge-base with the parent -> HEAD: commits only")
    (uncommitted "Uncommitted"
                 "HEAD -> worktree: staged, unstaged and untracked")
    (staged      "Staged"
                 "HEAD -> index")
    (unstaged    "Unstaged"
                 "index -> worktree, including untracked files")
    (iteration   "Latest iteration"
                 "last iteration snapshot -> worktree: what changed since the last revision request"))
  "Review scopes: (SYMBOL NAME DESCRIPTION).")

(defface bndos-task-review-label '((t :inherit font-lock-keyword-face))
  "Labels in the review header."
  :group 'bndos-task)
(defface bndos-task-review-warning '((t :inherit warning))
  "Problems shown in the review buffer."
  :group 'bndos-task)
(defface bndos-task-review-thread-open '((t :inherit font-lock-warning-face :weight bold))
  "Open discussion threads."
  :group 'bndos-task)
(defface bndos-task-review-thread-sent '((t :inherit font-lock-function-name-face))
  "Threads waiting on the agent."
  :group 'bndos-task)
(defface bndos-task-review-thread-addressed '((t :inherit success))
  "Threads the agent reports as addressed."
  :group 'bndos-task)
(defface bndos-task-review-thread-resolved '((t :inherit shadow))
  "Resolved threads."
  :group 'bndos-task)
(defface bndos-task-review-inline '((t :inherit font-lock-doc-face :extend t))
  "Inline thread markers under diff lines."
  :group 'bndos-task)


;;; Buffer state

(defvar-local bndos-task-review-key nil "Task key of this review buffer.")
(defvar-local bndos-task-review-directory nil "Worktree reviewed by this buffer.")
(defvar-local bndos-task-review-scope 'full "Current review scope symbol.")
(defvar-local bndos-task-review--data nil
  "Result of the last refresh: (:identity :base :snapshot :status :commits
:files :from :to :error :refreshed).")
(defvar-local bndos-task-review--generation 0
  "Incremented per refresh; stale async callbacks compare against it.")
(defvar-local bndos-task-review--processes nil)
(defvar-local bndos-task-review--marked nil "Thread ids marked for submission.")

(defun bndos-task-review-record ()
  "The current buffer's task record."
  (and bndos-task-review-key (bndos-task-store-load bndos-task-review-key)))

(defun bndos-task-review-buffer (key)
  "Return the live review buffer for task KEY, if any."
  (seq-find (lambda (buffer)
              (equal (buffer-local-value 'bndos-task-review-key buffer) key))
            (buffer-list)))

(defun bndos-task-review-buffers ()
  (seq-filter (lambda (buffer) (buffer-local-value 'bndos-task-review-key buffer))
              (buffer-list)))


;;; Mode

;; Actions live on C-c C-<key>, like pr-review's, so they work the same in
;; evil's normal, motion and visual states (with a region selected) and
;; never shadow navigation keys.
(defvar-keymap bndos-task-review-mode-map
  :parent hutch-mode-map
  "g"       #'bndos/task-review-refresh
  "RET"     #'bndos/task-review-visit
  "C-c C-c" #'bndos/task-review-comment
  "C-c C-s" #'bndos/task-review-submit
  "C-c C-q" #'bndos/task-review-ask
  "C-c C-i" #'bndos/task-review-instruct
  "C-c C-y" #'bndos/task-review-send-to-agent
  "C-c C-o" #'bndos/task-review-visit-agent
  "C-c C-t" #'bndos/task-review-mark
  "C-c C-d" #'bndos/task-review-resolve
  "C-c C-u" #'bndos/task-review-reopen
  "C-c C-r" #'bndos/task-review-refresh
  "C-c C-f" #'bndos/task-review-visit
  "C-c C-l" #'bndos/task-review-set-scope
  "C-c C-n" #'bndos/task-review-toggle-iteration
  "C-c C-b" #'bndos/task-review-set-base
  "C-c C-e" #'bndos/task-review-toggle-tree
  "C-c C-a" #'bndos/task-review-approve
  "C-c C-h" #'bndos/task-review-ai-review
  "]"       #'bndos/task-review-next-thread
  "["       #'bndos/task-review-previous-thread)

(defvar-keymap bndos-task-review-thread-section-map
  :parent magit-section-mode-map
  "RET"     #'bndos/task-review-visit
  "C-c C-c" #'bndos/task-review-comment
  "C-c C-t" #'bndos/task-review-mark
  "C-c C-d" #'bndos/task-review-resolve
  "C-c C-u" #'bndos/task-review-reopen)

(define-derived-mode bndos-task-review-mode hutch-mode "Task-Review"
  "Review of a task worktree: discussion threads and the task's diff.

\\{bndos-task-review-mode-map}"
  :interactive nil
  (setq-local hutch-render-header-functions (list #'bndos-task-review--insert-header))
  (setq-local hutch-render-footer-functions (list #'bndos-task-review--insert-threads
                                                  #'bndos-task-review--insert-diff))
  (setq-local hutch-worktree-scope-function #'bndos-task-review--hutch-scope)
  (setq-local revert-buffer-function (lambda (&rest _) (bndos/task-review-refresh)))
  (add-hook 'kill-buffer-hook #'bndos-task-review--cancel-processes nil t))

(defun bndos-task-review--cancel-processes ()
  (mapc #'bndos-proc-cancel bndos-task-review--processes)
  (setq bndos-task-review--processes nil))


;;; Opening

(defun bndos-task-review--buffer-name (record)
  (format "*task-review: %s/%s [%s]*"
          (or (bndos-task-get record :repo :project) "?")
          (or (bndos-task-get record :worktree :handle)
              (file-name-nondirectory (directory-file-name (bndos-task-get record :worktree :path))))
          (substring (plist-get record :key) 0 6)))

(defun bndos-task-review-switch-workspace (worktree)
  "Switch to WORKTREE's Doom/persp project workspace, creating it if needed.
Reuses the workspace Doom associates with that project root.  Without Doom
workspaces this only registers the project with Projectile."
  (when (fboundp 'projectile-add-known-project)
    (projectile-add-known-project (file-name-as-directory worktree)))
  (when (and (fboundp '+workspaces-switch-to-project-h) (bound-and-true-p persp-mode))
    (let ((+workspaces-switch-project-function #'ignore)
          (+workspaces-on-switch-project-behavior t))
      (+workspaces-switch-to-project-h (file-name-as-directory worktree)))))

(cl-defun bndos-task-review-open (key &key workspace (select t))
  "Open (or refresh) the review buffer for task KEY.  Return the buffer.
With WORKSPACE, first switch to the worktree's project workspace.  With
SELECT, display the buffer in the selected window and show the tree."
  (let* ((record (or (bndos-task-store-load key) (user-error "No task record for %s" key)))
         (worktree (file-name-as-directory (bndos-task-get record :worktree :path))))
    (unless (file-directory-p worktree)
      (user-error "Worktree %s no longer exists (its discussion is kept in %s)"
                  worktree (bndos-task-store-file key)))
    (when workspace
      (bndos-task-review-switch-workspace worktree))
    (let ((buffer (or (bndos-task-review-buffer key)
                      (with-current-buffer (hutch--setup-buffer (bndos-task-review--buffer-name record))
                        (bndos-task-review-mode)
                        (setq bndos-task-review-key key
                              bndos-task-review-directory worktree
                              default-directory worktree
                              hutch-review-title (format "Task review: %s"
                                                         (bndos-task-get record :worktree :handle)))
                        (current-buffer)))))
      (when select
        (switch-to-buffer buffer)
        (when (fboundp 'persp-add-buffer)
          (ignore-errors (persp-add-buffer buffer))))
      (with-current-buffer buffer
        (bndos-task-review-refresh :show-tree (and select bndos-task-review-show-tree)))
      buffer)))


;;; Refresh pipeline

(defun bndos-task-review--scope-range (scope base snapshot record)
  "Return (FROM . TO) for SCOPE, or a string explaining why it's unavailable."
  (let ((mb (plist-get base :merge-base))
        (head (or (plist-get snapshot :head) bndos-task-git-empty-tree))
        (tree (plist-get snapshot :tree))
        (index (plist-get snapshot :index-tree)))
    (pcase scope
      ('full (if mb (cons mb tree) "no usable base"))
      ('committed (if mb (cons mb head) "no usable base"))
      ('uncommitted (cons head tree))
      ('staged (cons head index))
      ('unstaged (cons index tree))
      ('iteration
       (if-let* ((last (car (last (plist-get record :iterations)))))
           (cons (plist-get last :tree) tree)
         "no iteration recorded yet")))))

(defun bndos-task-review--track (process)
  (when process (push process bndos-task-review--processes))
  process)

(cl-defun bndos-task-review-refresh (&key show-tree)
  "Asynchronously recompute and re-render the current review buffer."
  (let* ((buffer (current-buffer))
         (generation (cl-incf bndos-task-review--generation))
         (directory bndos-task-review-directory)
         (key bndos-task-review-key)
         (scope bndos-task-review-scope)
         (data nil))
    (bndos-task-review--cancel-processes)
    (cl-labels
        ((live-p () (and (buffer-live-p buffer)
                         (= generation (buffer-local-value 'bndos-task-review--generation buffer))))
         (step (form) (bndos-task-review--track form))
         (finish (&optional error)
           (when (live-p)
             (with-current-buffer buffer
               (setq bndos-task-review--processes nil
                     bndos-task-review--data
                     (append (list :error error :refreshed (current-time)) data))
               (bndos-task-review-render)
               (bndos-task-review--update-tree show-tree))))
         (fail (message) (finish message)))
      (with-current-buffer buffer
        (setq bndos-task-review--data (append (list :refreshing t) bndos-task-review--data))
        (bndos-task-review-render))
      (step
       (bndos-task-git-identity
        directory
        (lambda (identity)
          (when (live-p)
            (if (plist-get identity :error)
                (fail (plist-get identity :error))
              (setq data (plist-put data :identity identity))
              (progn
                (with-current-buffer buffer
                  (step
                   (bndos-task-git-resolve-base
                    directory
                    :branch (plist-get identity :branch)
                    :callback
                    (lambda (base)
                      (when (live-p)
                        (setq data (plist-put data :base base))
                        (with-current-buffer buffer
                          (step
                           (bndos-task-git-snapshot
                            directory
                            (lambda (snapshot)
                              (when (live-p)
                                (if (plist-get snapshot :error)
                                    (fail (plist-get snapshot :error))
                                  (setq data (plist-put data :snapshot snapshot))
                                  (bndos-task-review--ensure-baseline key snapshot)
                                  (with-current-buffer buffer
                                    (bndos-task-review--collect
                                     buffer key directory scope base snapshot data
                                     #'live-p #'step
                                     (lambda (more)
                                       (setq data (append more data))
                                       (finish))
                                     #'fail))))))))))))))))))))))

(defun bndos-task-review--collect (buffer key directory scope base snapshot data
                                          live-p step done fail)
  "Second half of the refresh: status, commit count, diff, re-anchoring."
  (ignore data)
  (let* ((record (bndos-task-store-load key))
         (range (bndos-task-review--scope-range scope base snapshot record))
         (pending 3)
         (out nil))
    (cl-flet ((join (plist)
                (setq out (append plist out))
                (when (and (funcall live-p) (zerop (cl-decf pending)))
                  (funcall done out))))
      (with-current-buffer buffer
        (funcall step (bndos-task-git-status
                       directory (lambda (status) (join (list :status status)))))
        (if-let* ((mb (plist-get base :merge-base)))
            (funcall step (bndos-task-git-count-commits
                           directory mb "HEAD" (lambda (n) (join (list :commits n)))))
          (join nil))
        (if (stringp range)
            (join (list :files nil :range-error range))
          (funcall step
                   (bndos-task-git-diff
                    directory (car range) (cdr range)
                    (lambda (files)
                      (if (plist-get files :error)
                          (funcall fail (plist-get files :error))
                        (bndos-task-review--reanchor key directory base snapshot)
                        (join (list :files files :from (car range) :to (cdr range))))))))))))

(defun bndos-task-review--ensure-baseline (key snapshot)
  "Record SNAPSHOT as KEY's first iteration if it has none."
  (bndos-task-store-update
   key (lambda (record)
         (if (plist-get record :iterations)
             record
           (plist-put record :iterations
                      (list (list :n 0 :tree (plist-get snapshot :tree)
                                  :head (plist-get snapshot :head)
                                  :at (bndos-task-now) :reason "review opened")))))))

(defun bndos-task-review-record-iteration (key snapshot reason &optional request)
  "Append SNAPSHOT to KEY's iterations, labelled REASON (and REQUEST id)."
  (bndos-task-store-update
   key (lambda (record)
         (let ((iterations (plist-get record :iterations)))
           (plist-put record :iterations
                      (append iterations
                              (list (list :n (length iterations)
                                          :tree (plist-get snapshot :tree)
                                          :head (plist-get snapshot :head)
                                          :at (bndos-task-now)
                                          :reason reason
                                          :request request))))))))


;;; Re-anchoring

(defun bndos-task-review-reanchor-anchor (anchor lines)
  "Locate ANCHOR in LINES (the file's current text on the anchor's side).
Return (:status ok|moved|stale :line N :reason STR).  Conservative: a line
that can't be found uniquely by its text and surrounding context is stale,
never silently moved."
  (let* ((text (plist-get anchor :text))
         (line (plist-get anchor :line))
         (before (plist-get anchor :before))
         (after (plist-get anchor :after))
         (vec (vconcat lines)))
    (cl-labels ((at (i) (and (>= i 0) (< i (length vec)) (aref vec i)))
                (score (i)
                  (+ (cl-loop for ctx in (reverse before) for k from 1
                              count (equal ctx (at (- i k))))
                     (cl-loop for ctx in after for k from 1
                              count (equal ctx (at (+ i k)))))))
      (cond
       ((null lines) (list :status 'stale :reason "file is gone"))
       ((null text) (list :status 'ok :line line))
       ((and line (equal (at (1- line)) text)
             (or (and (null before) (null after)) (> (score (1- line)) 0)))
        (list :status 'ok :line line))
       (t
        (let* ((candidates (cl-loop for i from 0 below (length vec)
                                    when (equal (aref vec i) text) collect i))
               (scored (mapcar (lambda (i) (cons i (score i))) candidates))
               (best (and scored (apply #'max (mapcar #'cdr scored))))
               (winners (seq-filter (lambda (c) (= (cdr c) best)) scored)))
          (cond
           ((null candidates) (list :status 'stale :reason "line no longer present"))
           ((and (= (length winners) 1)
                 (or (= (length candidates) 1) (> best 0)))
            (list :status 'moved :line (1+ (caar winners))))
           (t (list :status 'stale
                    :reason (format "ambiguous: %d matching lines" (length candidates)))))))))))

(defun bndos-task-review--file-lines (file)
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n"))))

(defun bndos-task-review--reanchor (key directory base snapshot)
  "Re-anchor KEY's line threads against the current worktree.
New-side anchors are matched against the worktree file.  Old-side anchors
are left alone unless the merge-base moved, in which case they're marked
stale for the reviewer to check.  Nothing is deleted."
  (bndos-task-store-update
   key
   (lambda (record)
     (let* ((changed nil)
            (threads
             (mapcar
              (lambda (thread)
                (let ((anchor (plist-get thread :anchor)))
                  (if (or (null anchor) (null (plist-get anchor :line)))
                      thread
                    (let ((result
                           (if (equal (plist-get anchor :side) "old")
                               (if (equal (plist-get anchor :merge-base) (plist-get base :merge-base))
                                   (list :status 'ok :line (plist-get anchor :line))
                                 (list :status 'stale :reason "the base moved"))
                             (bndos-task-review-reanchor-anchor
                              anchor (bndos-task-review--file-lines
                                      (file-name-concat directory (plist-get anchor :path)))))))
                      (pcase (plist-get result :status)
                        ('ok (if (plist-get thread :stale)
                                 (progn (setq changed t) (plist-put (copy-sequence thread) :stale nil))
                               thread))
                        ('moved
                         (setq changed t)
                         (let* ((delta (- (plist-get result :line) (plist-get anchor :line)))
                                (anchor (plist-put (copy-sequence anchor) :line (plist-get result :line))))
                           (when (plist-get anchor :end-line)
                             (setq anchor (plist-put anchor :end-line (+ delta (plist-get anchor :end-line)))))
                           (setq anchor (plist-put anchor :new-line (plist-get result :line)))
                           (plist-put (plist-put (copy-sequence thread) :anchor anchor) :stale nil)))
                        ('stale
                         (if (equal (bndos-task-get thread :stale :reason) (plist-get result :reason))
                             thread
                           (setq changed t)
                           (plist-put (copy-sequence thread) :stale
                                      (list :reason (plist-get result :reason)
                                            :at (bndos-task-now)
                                            :tree (plist-get snapshot :tree))))))))))
              (plist-get record :threads))))
       (if changed (plist-put record :threads threads) record)))))


;;; Rendering

(defun bndos-task-review-render ()
  "Re-render the current review buffer from its state."
  (hutch--render-buffer (current-buffer)))

(defun bndos-task-review--label (label)
  (propertize (format "%-9s " label) 'font-lock-face 'bndos-task-review-label))

(defun bndos-task-review--short (sha)
  (if (and sha (> (length sha) 8)) (substring sha 0 8) (or sha "?")))

(defun bndos-task-review--scope-info (scope)
  (alist-get scope bndos-task-review-scopes))

(defun bndos-task-review--insert-header ()
  (let* ((record (bndos-task-review-record))
         (data bndos-task-review--data)
         (identity (plist-get data :identity))
         (base (plist-get data :base))
         (snapshot (plist-get data :snapshot))
         (status (plist-get data :status))
         (scope-info (bndos-task-review--scope-info bndos-task-review-scope)))
    (magit-insert-section (bndos-task-header)
                          (insert (bndos-task-review--label "Task")
                                  (format "%s/%s" (or (bndos-task-get record :repo :project) "?")
                                          (or (bndos-task-get record :worktree :handle) "?"))
                                  (if-let* ((ticket (plist-get record :ticket))) (format "  ticket %s" ticket) "")
                                  "\n"
                                  (bndos-task-review--label "Worktree") (abbreviate-file-name bndos-task-review-directory) "\n"
                                  (bndos-task-review--label "Branch")
                                  (format "%s @ %s" (or (plist-get identity :branch) "(detached)")
                                          (bndos-task-review--short (plist-get identity :head)))
                                  "\n")
                          (insert (bndos-task-review--label "Base"))
                          (cond
                           ((null base) (insert "…"))
                           ((plist-get base :error)
                            (insert (propertize (format "%s — C-c C-b chooses a base" (plist-get base :message))
                                                'font-lock-face 'bndos-task-review-warning))
                            (when-let* ((candidates (plist-get base :candidates)))
                              (insert (format " (candidates: %s)" (string-join candidates ", ")))))
                           (t (insert (format "%s (%s) -> merge-base %s" (plist-get base :ref)
                                              "recorded parent"
                                              (bndos-task-review--short (plist-get base :merge-base))))
                              (when-let* ((n (plist-get data :commits)))
                                (insert (format ", %d commit%s since" n (if (= n 1) "" "s"))))))
                          (insert "\n" (bndos-task-review--label "Scope")
                                  (propertize (car scope-info) 'font-lock-face 'bold)
                                  ": " (cadr scope-info) "  (s: change, I: latest iteration)\n")
                          (when (plist-get data :from)
                            (insert (bndos-task-review--label "Compare")
                                    (format "%s -> %s" (bndos-task-review--short (plist-get data :from))
                                            (bndos-task-review--short (plist-get data :to)))
                                    "\n"))
                          (when status
                            (insert (bndos-task-review--label "Worktree")
                                    (if (plist-get status :error)
                                        (plist-get status :error)
                                      (format "%d staged, %d unstaged, %d untracked%s"
                                              (plist-get status :staged) (plist-get status :unstaged)
                                              (plist-get status :untracked)
                                              (if (> (plist-get status :conflicted) 0)
                                                  (format ", %d conflicted" (plist-get status :conflicted)) "")))
                                    "\n"))
                          (when snapshot
                            (insert (bndos-task-review--label "Snapshot")
                                    (format "tree %s, iteration %d" (bndos-task-review--short (plist-get snapshot :tree))
                                            (max 0 (1- (length (plist-get record :iterations)))))
                                    "\n"))
                          (insert (bndos-task-review--label "Agent")
                                  (if (fboundp 'bndos-task-agent-summary)
                                      (bndos-task-agent-summary record)
                                    "-")
                                  "\n")
                          (let ((threads (plist-get record :threads)))
                            (insert (bndos-task-review--label "Review")
                                    (bndos-task-get record :review :state)
                                    (format " — %s" (bndos-task-review--thread-counts threads))
                                    "\n"))
                          (when (plist-get data :refreshing)
                            (insert (propertize "Refreshing…\n" 'font-lock-face 'shadow)))
                          (when-let* ((err (plist-get data :error)))
                            (insert (propertize (format "Error: %s\n" err) 'font-lock-face 'bndos-task-review-warning)))
                          (when-let* ((err (plist-get data :range-error)))
                            (insert (propertize (format "Scope unavailable: %s\n" err) 'font-lock-face 'bndos-task-review-warning)))
                          (when hutch--scopes
                            (insert (propertize "AI findings below come from Hutch (gptel); they're separate from the discussion threads.\n"
                                                'font-lock-face 'shadow)))
                          (insert "\n"))))

(defun bndos-task-review--thread-counts (threads)
  (let ((counts (mapcar (lambda (state)
                          (cons state (seq-count (lambda (th) (equal (plist-get th :state) state)) threads)))
                        bndos-task-thread-states))
        (stale (seq-count (lambda (th) (plist-get th :stale)) threads)))
    (concat (mapconcat (pcase-lambda (`(,state . ,n)) (format "%d %s" n state))
                       (seq-filter (lambda (c) (> (cdr c) 0)) counts) ", ")
            (if (> stale 0) (format ", %d stale" stale) "")
            (if (null threads) "no threads" ""))))

(defun bndos-task-review--state-face (state)
  (pcase state
    ("open" 'bndos-task-review-thread-open)
    ("sent" 'bndos-task-review-thread-sent)
    ("addressed" 'bndos-task-review-thread-addressed)
    (_ 'bndos-task-review-thread-resolved)))

(defun bndos-task-review--anchor-label (anchor)
  (cond
   ((null anchor) "general")
   ((plist-get anchor :line)
    (format "%s:%s%s%s" (plist-get anchor :path) (plist-get anchor :line)
            (if-let* ((end (plist-get anchor :end-line))) (format "-%s" end) "")
            (if (equal (plist-get anchor :side) "old") " (old)" "")))
   (t (plist-get anchor :path))))

(defun bndos-task-review--first-line (text)
  (truncate-string-to-width (car (split-string (or text "") "\n")) 70 nil nil "…"))

(defun bndos-task-review--thread-heading (thread)
  (concat (propertize (format "[%s]" (plist-get thread :state))
                      'font-lock-face (bndos-task-review--state-face (plist-get thread :state)))
          (if (member (plist-get thread :id) bndos-task-review--marked) " *" "")
          (if (plist-get thread :stale)
              (propertize " [stale]" 'font-lock-face 'bndos-task-review-warning) "")
          " " (propertize (bndos-task-review--anchor-label (plist-get thread :anchor))
                          'font-lock-face 'magit-filename)
          " " (bndos-task-review--first-line (plist-get thread :body))
          (let ((n (length (plist-get thread :replies))))
            (if (> n 0) (propertize (format "  (%d)" n) 'font-lock-face 'shadow) ""))))

(defun bndos-task-review--insert-body (text indent)
  (dolist (line (split-string (or text "") "\n"))
    (insert indent line "\n")))

(defun bndos-task-review--insert-threads ()
  (let ((threads (plist-get (bndos-task-review-record) :threads)))
    (magit-insert-section (bndos-task-threads)
                          (magit-insert-heading
                           (propertize (format "Discussion (%d)" (length threads)) 'font-lock-face 'magit-section-heading))
                          (if (null threads)
                              (insert (propertize "  No threads yet.  c: comment on the line/region at point, a: ask the agent.\n"
                                                  'font-lock-face 'shadow))
                            (dolist (thread threads)
                              (magit-insert-section section (bndos-task-thread (plist-get thread :id)
                                                                               (equal (plist-get thread :state) "resolved"))
                                                    (oset section keymap 'bndos-task-review-thread-section-map)
                                                    (magit-insert-heading (concat "  " (bndos-task-review--thread-heading thread)))
                                                    (insert (propertize (format "    %s · %s · %s%s\n" (plist-get thread :id)
                                                                                (plist-get thread :author) (plist-get thread :created)
                                                                                (if (equal (plist-get thread :kind) "comment") ""
                                                                                  (format " · %s" (plist-get thread :kind))))
                                                                        'font-lock-face 'shadow))
                                                    (when-let* ((stale (plist-get thread :stale)))
                                                      (insert (propertize (format "    stale: %s (snapshot %s)\n" (plist-get stale :reason)
                                                                                  (bndos-task-review--short (plist-get stale :tree)))
                                                                          'font-lock-face 'bndos-task-review-warning)))
                                                    (bndos-task-review--insert-body (plist-get thread :body) "    ")
                                                    (dolist (reply (plist-get thread :replies))
                                                      (insert (propertize (format "    ↳ %s · %s%s\n" (plist-get reply :author) (plist-get reply :created)
                                                                                  (if (plist-get reply :status) (format " · %s" (plist-get reply :status)) ""))
                                                                          'font-lock-face 'shadow))
                                                      (bndos-task-review--insert-body (plist-get reply :body) "      ")
                                                      (when-let* ((files (plist-get reply :files)))
                                                        (insert (format "      files: %s\n" (string-join files ", "))))
                                                      (when-let* ((tests (plist-get reply :tests)))
                                                        (insert (format "      tests: %s\n" (string-join tests ", ")))))
                                                    (insert "\n"))))
                          (insert "\n"))))

(defun bndos-task-review--status-token (file)
  (if (plist-get file :binary) "B"
    (pcase (plist-get file :status)
      ("new file" "A") ("deleted" "D") ("renamed" "R") ("mode" "T") (_ "M"))))

(defun bndos-task-review--line-threads (threads path line)
  "Unresolved THREADS anchored to new-side LINE (or old-side, see LINE) of PATH."
  (seq-filter (lambda (thread)
                (let ((anchor (plist-get thread :anchor)))
                  (and anchor
                       (not (equal (plist-get thread :state) "resolved"))
                       (equal (plist-get anchor :path) path)
                       (equal (cons (plist-get anchor :side) (plist-get anchor :line)) line))))
              threads))

(defun bndos-task-review--side-texts (lines exclude)
  "Texts of the next two LINES that aren't of type EXCLUDE or meta."
  (mapcar (lambda (l) (plist-get l :text))
          (seq-take (seq-remove (lambda (l) (memq (plist-get l :type) (list exclude 'meta))) lines)
                    2)))

(defun bndos-task-review--insert-diff ()
  (let* ((data bndos-task-review--data)
         (files (plist-get data :files))
         (threads (plist-get (bndos-task-review-record) :threads)))
    (magit-insert-section (bndos-task-diff)
                          (magit-insert-heading
                           (propertize (format "Changes (%d files, +%d -%d)" (length files)
                                               (apply #'+ 0 (mapcar (lambda (f) (plist-get f :added)) files))
                                               (apply #'+ 0 (mapcar (lambda (f) (plist-get f :removed)) files)))
                                       'font-lock-face 'magit-section-heading))
                          (cond
                           ((plist-get data :refreshing) (insert "  …\n"))
                           ((null files) (insert (propertize "  No changes in this scope.\n" 'font-lock-face 'shadow))))
                          (dolist (file files)
                            (let ((path (plist-get file :path))
                                  (size (apply #'+ 0 (mapcar (lambda (h) (length (plist-get h :lines))) (plist-get file :hunks)))))
                              (magit-insert-section (bndos-task-file path (> size bndos-task-review-collapse-lines))
                                                    (magit-insert-heading
                                                     (propertize (format "%s %s%s (+%d -%d)%s"
                                                                         (bndos-task-review--status-token file)
                                                                         (if (equal (plist-get file :status) "renamed")
                                                                             (format "%s -> " (plist-get file :old-path)) "")
                                                                         path (plist-get file :added) (plist-get file :removed)
                                                                         (if (plist-get file :binary) " binary" ""))
                                                                 'font-lock-face 'magit-diff-file-heading))
                                                    (when (and (null (plist-get file :hunks)) (not (plist-get file :binary)))
                                                      (insert (propertize (format "  (%s, no content changes)\n" (plist-get file :status))
                                                                          'font-lock-face 'shadow)))
                                                    (dolist (hunk (plist-get file :hunks))
                                                      (magit-insert-section (bndos-task-hunk (cons path (plist-get hunk :header)))
                                                                            (magit-insert-heading
                                                                             (propertize (plist-get hunk :header) 'font-lock-face 'magit-diff-hunk-heading))
                                                                            (let ((before-new nil)
                                                                                  (before-old nil)
                                                                                  (lines (plist-get hunk :lines)))
                                                                              (while lines
                                                                                (let* ((line (pop lines))
                                                                                       (type (plist-get line :type))
                                                                                       (start (point))
                                                                                       (props (list :path path :old-path (plist-get file :old-path)
                                                                                                    :type type :old (plist-get line :old) :new (plist-get line :new)
                                                                                                    :text (plist-get line :text) :hunk (plist-get hunk :header)
                                                                                                    ;; Context per side, nearest first: a removed
                                                                                                    ;; line isn't part of the new file and vice versa.
                                                                                                    :before-new (seq-take before-new 2)
                                                                                                    :before-old (seq-take before-old 2)
                                                                                                    :after-new (bndos-task-review--side-texts lines 'removed)
                                                                                                    :after-old (bndos-task-review--side-texts lines 'added))))
                                                                                  (insert (pcase type ('added "+") ('removed "-") ('context " ") (_ ""))
                                                                                          (plist-get line :text) "\n")
                                                                                  (put-text-property start (point) 'bndos-task-line props)
                                                                                  (put-text-property start (point) 'font-lock-face
                                                                                                     (pcase type
                                                                                                       ('added 'magit-diff-added)
                                                                                                       ('removed 'magit-diff-removed)
                                                                                                       ('meta 'shadow)
                                                                                                       (_ 'magit-diff-context)))
                                                                                  (unless (memq type '(meta removed))
                                                                                    (push (plist-get line :text) before-new))
                                                                                  (unless (memq type '(meta added))
                                                                                    (push (plist-get line :text) before-old))
                                                                                  (dolist (thread (bndos-task-review--line-threads
                                                                                                   threads path
                                                                                                   (if (eq type 'removed)
                                                                                                       (cons "old" (plist-get line :old))
                                                                                                     (cons "new" (plist-get line :new)))))
                                                                                    (let ((tstart (point)))
                                                                                      (insert "  ┃ " (bndos-task-review--thread-heading thread) "\n")
                                                                                      (put-text-property tstart (point) 'bndos-task-thread (plist-get thread :id))
                                                                                      (add-face-text-property tstart (point) 'bndos-task-review-inline t)))))))))))
                          (insert "\n"))))


;;; Tree

(defun bndos-task-review--file-at-point ()
  (or (plist-get (get-text-property (point) 'bndos-task-line) :path)
      (let ((section (magit-current-section)))
        (while (and section (not (eq (oref section type) 'bndos-task-file)))
          (setq section (oref section parent)))
        (and section (oref section value)))))

(defun bndos-task-review--goto-file (file)
  (when-let* ((section (magit-get-section `((bndos-task-file . ,file) (bndos-task-diff) (review-root)))))
    (goto-char (oref section start))
    (when (oref section hidden) (magit-section-show section))
    (recenter 0)))

(defun bndos-task-review--update-tree (show)
  (let* ((files (plist-get bndos-task-review--data :files))
         (record (bndos-task-review-record)))
    (magit-treediff-follow
     (current-buffer)
     :title (format "%s/%s: %s" (bndos-task-get record :repo :project)
                    (bndos-task-get record :worktree :handle)
                    (car (bndos-task-review--scope-info bndos-task-review-scope)))
     :files (mapcar (lambda (file)
                      (list :file (plist-get file :path)
                            :status (plist-get file :status)
                            :summary (list :hunks (length (plist-get file :hunks))
                                           :added (plist-get file :added)
                                           :removed (plist-get file :removed))))
                    files)
     :goto-file #'bndos-task-review--goto-file
     :file-at-point #'bndos-task-review--file-at-point)
    (when (and show (get-buffer-window (current-buffer)))
      (with-selected-window (get-buffer-window (current-buffer))
        (magit-treediff-follow-show (current-buffer))))))

(defun bndos/task-review-toggle-tree ()
  "Show or hide the file tree following this review."
  (interactive)
  (magit-treediff-follow-toggle (current-buffer)))


;;; Commands: refresh, scope, base, visiting

(defun bndos-task-review--ensure ()
  (unless (derived-mode-p 'bndos-task-review-mode)
    (user-error "Not in a task review buffer")))

(defun bndos/task-review-refresh ()
  "Refresh the diff, threads and tree of this review."
  (interactive)
  (bndos-task-review--ensure)
  (bndos-task-review-refresh))

(defun bndos/task-review-set-scope (scope)
  "Review SCOPE of the task (see `bndos-task-review-scopes')."
  (interactive
   (list (let* ((choices (mapcar (pcase-lambda (`(,sym ,name ,desc))
                                   (cons (format "%-17s %s" name (propertize desc 'face 'shadow)) sym))
                                 bndos-task-review-scopes)))
           (alist-get (completing-read "Review scope: " choices nil t) choices nil nil #'equal))))
  (bndos-task-review--ensure)
  (setq bndos-task-review-scope scope)
  (bndos-task-review-refresh))

(defun bndos/task-review-toggle-iteration ()
  "Switch between the whole task and the latest iteration."
  (interactive)
  (bndos/task-review-set-scope (if (eq bndos-task-review-scope 'iteration) 'full 'iteration)))

(defun bndos-task-review--refs (directory)
  (with-temp-buffer
    (let ((default-directory directory))
      (when (zerop (process-file "git" nil t nil "for-each-ref" "--format=%(refname:short)"
                                 "refs/heads" "refs/remotes"))
        (split-string (buffer-string) "\n" t)))))

(defun bndos/task-review-set-base (ref)
  "Compare this task against parent REF, recorded in git config.
Writes branch.<branch>.bndos-base in the task's repository."
  (interactive
   (progn
     (bndos-task-review--ensure)
     (let ((base (plist-get bndos-task-review--data :base)))
       (list (completing-read (format "Parent of this task (now %s): " (or (plist-get base :ref) "none"))
                              (bndos-task-review--refs bndos-task-review-directory)
                              nil t nil nil (car (plist-get base :candidates)))))))
  (bndos-task-review--ensure)
  (let ((branch (bndos-task-get bndos-task-review--data :identity :branch))
        (key bndos-task-review-key)
        (buffer (current-buffer)))
    (unless branch (user-error "Detached HEAD: no branch to record a parent for"))
    (when (y-or-n-p (format "Record %s as the parent of %s (git config %s)? "
                            ref branch (bndos-task-git-base-key branch)))
      (bndos-task-git-set-base
       bndos-task-review-directory branch ref
       (lambda (err)
         (if err
             (message "Couldn't record the base: %s" err)
           (bndos-task-store-update key (lambda (r) (bndos-task-put r '(:base) (list :ref ref)))))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (bndos-task-review-refresh))))))))

(defun bndos/task-review-visit ()
  "Visit the file at point: the worktree file, or the base revision for removed lines."
  (interactive)
  (bndos-task-review--ensure)
  (let* ((thread-id (bndos-task-review--thread-at-point))
         (thread (and thread-id (bndos-task-thread (bndos-task-review-record) thread-id)))
         (line (get-text-property (point) 'bndos-task-line))
         (path (or (plist-get line :path) (bndos-task-get thread :anchor :path)
                   (bndos-task-review--file-at-point))))
    (cond
     ((and thread (not line) (bndos-task-get thread :anchor :path))
      (bndos-task-review--goto-anchor (plist-get thread :anchor)))
     ((null path) (user-error "Nothing to visit here"))
     ((and line (eq (plist-get line :type) 'removed))
      (let ((rev (plist-get bndos-task-review--data :from))
            (old (or (plist-get line :old-path) path)))
        (magit-find-file rev old)
        (goto-char (point-min))
        (forward-line (1- (plist-get line :old)))))
     (t
      (let ((file (file-name-concat bndos-task-review-directory path)))
        (unless (file-exists-p file) (user-error "%s doesn't exist in the worktree" path))
        (find-file-other-window file)
        (when-let* ((n (plist-get line :new)))
          (goto-char (point-min))
          (forward-line (1- n))))))))

(defun bndos-task-review--goto-anchor (anchor)
  (bndos-task-review--goto-file (plist-get anchor :path))
  (let ((side (if (equal (plist-get anchor :side) "old") :old :new))
        (target (plist-get anchor :line))
        found)
    (save-excursion
      (while (and (not found) (not (eobp)))
        (let ((props (get-text-property (point) 'bndos-task-line)))
          (when (and props (equal (plist-get props :path) (plist-get anchor :path))
                     (equal (plist-get props side) target))
            (setq found (point))))
        (forward-line 1)))
    (if found (goto-char found) (message "Anchor not in the current scope's diff"))))


;;; Threads at point and drafting

(defun bndos-task-review--thread-at-point ()
  (or (get-text-property (point) 'bndos-task-thread)
      (let ((section (magit-current-section)))
        (while (and section (not (eq (oref section type) 'bndos-task-thread)))
          (setq section (oref section parent)))
        (and section (oref section value)))))

(defun bndos-task-review--line-props-in (beg end)
  (let (lines)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (when-let* ((props (get-text-property (point) 'bndos-task-line)))
          (push props lines))
        (forward-line 1)))
    (nreverse lines)))

(defun bndos-task-review-anchor-at-point ()
  "Return an anchor plist for the region or the diff line/hunk/file at point.
The anchor records path, side, line mapping, the selected diff text and
the snapshot it was taken against."
  (let* ((data bndos-task-review--data)
         (lines (if (use-region-p)
                    (bndos-task-review--line-props-in (region-beginning) (region-end))
                  (when-let* ((props (get-text-property (point) 'bndos-task-line)))
                    (list props))))
         (common (list :scope (symbol-name bndos-task-review-scope)
                       :from (plist-get data :from) :to (plist-get data :to)
                       :tree (bndos-task-get data :snapshot :tree)
                       :merge-base (bndos-task-get data :base :merge-base))))
    (cond
     (lines
      (let* ((paths (delete-dups (mapcar (lambda (l) (plist-get l :path)) lines)))
             (first (car lines))
             (last (car (last lines)))
             (side (if (plist-get first :new) "new" "old"))
             (key (if (equal side "new") :new :old)))
        (when (cdr paths)
          (user-error "The selection spans several files; select within one"))
        (append
         (list :path (car paths) :old-path (plist-get first :old-path) :side side
               :line (plist-get first key)
               :end-line (let ((same (seq-filter (lambda (l) (plist-get l key)) lines)))
                           (and (cdr same) (plist-get (car (last same)) key)))
               :old-line (plist-get first :old) :new-line (plist-get first :new)
               :hunk (plist-get first :hunk) :text (plist-get first :text)
               :before (reverse (plist-get first (if (equal side "new") :before-new :before-old)))
               :after (plist-get last (if (equal side "new") :after-new :after-old))
               :selection (mapconcat (lambda (l)
                                       (concat (pcase (plist-get l :type)
                                                 ('added "+") ('removed "-") (_ " "))
                                               (plist-get l :text)))
                                     lines "\n"))
         common)))
     ((bndos-task-review--file-at-point)
      (append (list :path (bndos-task-review--file-at-point)) common))
     (t nil))))

(defvar-local bndos-task-draft--submit nil)

(defvar-keymap bndos-task-draft-mode-map
  "C-c C-c" #'bndos-task-draft-submit
  "C-c C-k" #'bndos-task-draft-cancel)

(define-derived-mode bndos-task-draft-mode text-mode "Task-Draft"
  "Compose a review comment, reply or question.
\\<bndos-task-draft-mode-map>\\[bndos-task-draft-submit] submits, \\[bndos-task-draft-cancel] discards.")

(defun bndos-task-draft (title context on-submit)
  "Pop up a draft buffer titled TITLE showing CONTEXT; call ON-SUBMIT with the text."
  (let ((buffer (generate-new-buffer (format "*task-draft: %s*" title))))
    (with-current-buffer buffer
      (bndos-task-draft-mode)
      (setq bndos-task-draft--submit on-submit)
      (setq header-line-format
            (substitute-command-keys
             (format "%s  —  \\<bndos-task-draft-mode-map>\\[bndos-task-draft-submit] submit, \\[bndos-task-draft-cancel] cancel"
                     title)))
      (when context
        (insert (propertize (concat (string-join (mapcar (lambda (l) (concat "│ " l))
                                                         (split-string context "\n"))
                                                 "\n")
                                    "\n\n")
                            'read-only t 'front-sticky t 'rear-nonsticky t
                            'font-lock-face 'shadow 'bndos-draft-context t))))
    (pop-to-buffer buffer '((display-buffer-below-selected) (window-height . 12)))
    buffer))

(defun bndos-task-draft--text ()
  (let ((start (or (next-single-property-change (point-min) 'bndos-draft-context)
                   (point-min))))
    (unless (get-text-property (point-min) 'bndos-draft-context)
      (setq start (point-min)))
    (string-trim (buffer-substring-no-properties start (point-max)))))

(defun bndos-task-draft-submit ()
  "Submit the draft."
  (interactive)
  (let ((text (bndos-task-draft--text))
        (fn bndos-task-draft--submit))
    (when (string-empty-p text) (user-error "Empty draft"))
    (funcall fn text)
    (quit-window t)))

(defun bndos-task-draft-cancel ()
  "Discard the draft."
  (interactive)
  (when (or (string-empty-p (bndos-task-draft--text))
            (y-or-n-p "Discard this draft? "))
    (quit-window t)))

(defun bndos-task-review--anchor-context (anchor)
  (when anchor
    (concat (bndos-task-review--anchor-label anchor)
            (when-let* ((sel (plist-get anchor :selection))) (concat "\n" sel)))))

(defun bndos-task-review--author ()
  (or (bound-and-true-p user-login-name) (user-login-name)))

(defun bndos-task-review--rerender (key)
  (when-let* ((buffer (bndos-task-review-buffer key)))
    (with-current-buffer buffer (bndos-task-review-render))))


;;; Commands: comments

(defun bndos/task-review-comment ()
  "Comment on the region, diff line, hunk or file at point.
On a discussion thread (without a region), reply to it instead."
  (interactive)
  (bndos-task-review--ensure)
  (if (and (not (use-region-p)) (bndos-task-review--thread-at-point))
      (bndos/task-review-reply)
    (bndos-task-review--comment)))

(defun bndos-task-review--comment ()
  (let ((anchor (bndos-task-review-anchor-at-point))
        (key bndos-task-review-key))
    (bndos-task-draft "comment" (bndos-task-review--anchor-context anchor)
                      (lambda (body)
                        (bndos-task-store-update
                         key (lambda (record)
                               (bndos-task-add-thread
                                (bndos-task-put record '(:review :state) "in-review")
                                (bndos-task-make-thread :kind "comment" :author (bndos-task-review--author)
                                                        :body body :anchor anchor))))
                        (bndos-task-review--rerender key)))))

(defun bndos-task-review--require-thread ()
  (or (bndos-task-review--thread-at-point) (user-error "No discussion thread at point")))

(defun bndos/task-review-reply ()
  "Reply to the thread at point."
  (interactive)
  (bndos-task-review--ensure)
  (let* ((id (bndos-task-review--require-thread))
         (key bndos-task-review-key)
         (thread (bndos-task-thread (bndos-task-review-record) id)))
    (bndos-task-draft (format "reply to %s" id) (plist-get thread :body)
                      (lambda (body)
                        (bndos-task-store-update
                         key (lambda (record)
                               (bndos-task-update-thread
                                record id (lambda (th)
                                            (bndos-task-thread-add-reply
                                             th (bndos-task-make-reply :author (bndos-task-review--author)
                                                                       :body body))))))
                        (bndos-task-review--rerender key)))))

(defun bndos-task-review--set-state (state verb)
  (bndos-task-review--ensure)
  (let ((id (bndos-task-review--require-thread))
        (key bndos-task-review-key))
    (bndos-task-store-update
     key (lambda (record)
           (bndos-task-update-thread
            record id (lambda (th) (bndos-task-thread-set-state th state (bndos-task-review--author) verb)))))
    (bndos-task-review-render)
    (message "%s %s" (capitalize verb) id)))

(defun bndos/task-review-resolve ()
  "Mark the thread at point resolved (you verified it)."
  (interactive)
  (bndos-task-review--set-state "resolved" "resolved"))

(defun bndos/task-review-reopen ()
  "Reopen the thread at point."
  (interactive)
  (bndos-task-review--set-state "open" "reopened"))

(defun bndos/task-review-mark ()
  "Toggle the thread at point in the set to submit with \\[bndos/task-review-submit]."
  (interactive)
  (bndos-task-review--ensure)
  (let ((id (bndos-task-review--require-thread)))
    (setq bndos-task-review--marked
          (if (member id bndos-task-review--marked)
              (delete id bndos-task-review--marked)
            (cons id bndos-task-review--marked)))
    (bndos-task-review-render)))

(defun bndos/task-review-next-thread ()
  "Move to the next inline discussion marker in the diff."
  (interactive)
  (bndos-task-review--ensure)
  (let ((here (point)))
    (end-of-line)
    (if-let* ((match (text-property-search-forward 'bndos-task-thread nil (lambda (_ v) v))))
        (goto-char (prop-match-beginning match))
      (goto-char here)
      (message "No later thread marker"))))

(defun bndos/task-review-previous-thread ()
  "Move to the previous inline discussion marker in the diff."
  (interactive)
  (bndos-task-review--ensure)
  (let ((here (point)))
    (beginning-of-line)
    (if-let* ((match (text-property-search-backward 'bndos-task-thread nil (lambda (_ v) v))))
        (goto-char (prop-match-beginning match))
      (goto-char here)
      (message "No earlier thread marker"))))

(defun bndos/task-review-approve ()
  "Record that you approve this task.  Publishing and merging stay separate."
  (interactive)
  (bndos-task-review--ensure)
  (let* ((record (bndos-task-review-record))
         (open (bndos-task-unresolved-count record)))
    (when (or (zerop open)
              (y-or-n-p (format "%d thread(s) are unresolved.  Approve anyway? " open)))
      (bndos-task-store-update bndos-task-review-key
                               (lambda (r) (bndos-task-put r '(:review)
                                                           (list :state "approved" :at (bndos-task-now)
                                                                 :tree (bndos-task-get bndos-task-review--data
                                                                                       :snapshot :tree)))))
      (bndos-task-review-render)
      (message "Approved.  Nothing was published or merged."))))


;;; Commands: talking to the agent

(defun bndos-task-review--submission-ids (all)
  "Threads to send: every open one with ALL, else marked ones, the one at
point, or (after asking) every open one."
  (let ((open (mapcar (lambda (th) (plist-get th :id))
                      (seq-filter (lambda (th) (equal (plist-get th :state) "open"))
                                  (plist-get (bndos-task-review-record) :threads)))))
    (cond
     (all (or open (user-error "No open threads")))
     (bndos-task-review--marked (reverse bndos-task-review--marked))
     ((bndos-task-review--thread-at-point) (list (bndos-task-review--thread-at-point)))
     ((null open) (user-error "No open threads to send"))
     ((y-or-n-p (format "Send all %d open thread(s)? " (length open))) open)
     (t (user-error "Put point on a thread or mark threads with C-c C-t")))))

(defun bndos/task-review-submit (&optional all)
  "Send threads to the implementing agent: marked ones, the one at point, or
every open thread (asked first when point isn't on a thread; prefix ALL
skips that).  You choose whether the agent may edit code."
  (interactive "P")
  (bndos-task-review--ensure)
  (require 'bndos-task-agent)
  (let* ((ids (bndos-task-review--submission-ids all))
         (mode (completing-read (format "Send %d thread(s) to %s: " (length ids) "the agent")
                                '("implement — the agent may edit files"
                                  "explain — discussion only, no edits")
                                nil t))
         (mode (if (string-prefix-p "implement" mode) "implement" "explain"))
         (delegate (and (equal mode "implement")
                        (y-or-n-p "Let the agent's \"addressed\" reports resolve these threads? "))))
    (bndos-task-agent-submit bndos-task-review-key ids mode
                             :delegate delegate
                             :snapshot (bndos-task-get bndos-task-review--data :snapshot)
                             :base (plist-get bndos-task-review--data :base))
    (setq bndos-task-review--marked nil)
    (bndos-task-review-render)))

(defun bndos/task-review-ask ()
  "Ask the agent about the region, line, hunk or file at point (no edits)."
  (interactive)
  (bndos-task-review--ensure)
  (require 'bndos-task-agent)
  (let ((anchor (bndos-task-review-anchor-at-point))
        (key bndos-task-review-key)
        (snapshot (bndos-task-get bndos-task-review--data :snapshot))
        (base (plist-get bndos-task-review--data :base)))
    (bndos-task-draft "question for the agent" (bndos-task-review--anchor-context anchor)
                      (lambda (body)
                        (let ((thread (bndos-task-make-thread :kind "question"
                                                              :author (bndos-task-review--author)
                                                              :body body :anchor anchor)))
                          (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r thread)))
                          (bndos-task-review--rerender key)
                          (bndos-task-agent-submit key (list (plist-get thread :id)) "explain"
                                                   :snapshot snapshot :base base))))))

(defun bndos/task-review-instruct ()
  "Send the agent a general instruction (it may edit files).  Kept as a thread."
  (interactive)
  (bndos-task-review--ensure)
  (require 'bndos-task-agent)
  (let ((key bndos-task-review-key)
        (snapshot (bndos-task-get bndos-task-review--data :snapshot))
        (base (plist-get bndos-task-review--data :base)))
    (bndos-task-draft "instruction for the agent" nil
                      (lambda (body)
                        (let ((thread (bndos-task-make-thread :kind "instruction"
                                                              :author (bndos-task-review--author)
                                                              :body body)))
                          (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r thread)))
                          (bndos-task-review--rerender key)
                          (bndos-task-agent-submit key (list (plist-get thread :id)) "implement"
                                                   :snapshot snapshot :base base))))))

(defun bndos/task-review-visit-agent ()
  "Show this task's agent: its agent-shell, resuming or handing off if needed."
  (interactive)
  (bndos-task-review--ensure)
  (require 'bndos-task-agent)
  (bndos-task-agent-open bndos-task-review-key))

(defun bndos/task-review-send-to-agent ()
  "Put the region, line or file at point in the agent-shell's input, unsent.
Plain conversation: nothing becomes a thread and no reply format is asked
for.  Write your message after it and send it from the agent-shell."
  (interactive)
  (bndos-task-review--ensure)
  (require 'bndos-task-agent)
  (let* ((anchor (or (bndos-task-review-anchor-at-point) (user-error "Nothing at point to send")))
         (selection (plist-get anchor :selection))
         (text (concat (format "In %s:\n" (bndos-task-review--anchor-label anchor))
                       (if selection (format "```diff\n%s\n```\n" selection) "")
                       "\n")))
    (bndos-task-agent-ensure
     bndos-task-review-key
     (lambda (buffer)
       (agent-shell-insert :text text :shell-buffer buffer)
       (pop-to-buffer buffer))
     :display t)))


;;; Optional Hutch (gptel) review of the whole task

(defun bndos-task-review--hutch-scope ()
  "Return a fresh Hutch `:worktree' scope for the current review buffer.
Synchronous, as Hutch's own scope collection is."
  (let* ((data bndos-task-review--data)
         (mb (bndos-task-get data :base :merge-base))
         (tree (bndos-task-review--snapshot-sync bndos-task-review-directory)))
    (when (and mb tree)
      (when-let* ((manifest (hutch--git-diff-numstat mb tree)))
        (hutch--make-scope :worktree tree mb manifest)))))

(defun bndos-task-review--snapshot-sync (directory)
  "Synchronous variant of `bndos-task-git-snapshot' returning the tree SHA."
  (let* ((default-directory directory)
         (index (car (process-lines "git" "rev-parse" "--path-format=absolute" "--git-path" "index")))
         (tmp (bndos-task-git--index-file)))
    (unwind-protect
        (let ((process-environment (cons (concat "GIT_INDEX_FILE=" tmp) process-environment)))
          (if (file-exists-p index) (copy-file index tmp t) (delete-file tmp))
          (unless (zerop (process-file "git" nil nil nil "add" "-A" "--" "."))
            (error "git add failed while snapshotting %s" directory))
          (car (process-lines "git" "write-tree")))
      (ignore-errors (delete-file tmp)))))

(defun bndos/task-review-ai-review ()
  "Run Hutch's LLM review (gptel, `hutch-backend') over the whole task.
Optional: manual review never needs it."
  (interactive)
  (bndos-task-review--ensure)
  (unless (bound-and-true-p hutch-backend)
    (user-error "Set `hutch-backend' to a gptel backend to use AI review"))
  (let ((scope (or (bndos-task-review--hutch-scope)
                   (user-error "No base or no changes to review"))))
    (hutch-review-run (current-buffer) (list scope))))

(provide 'bndos-task-review)
;;; bndos-task-review.el ends here
