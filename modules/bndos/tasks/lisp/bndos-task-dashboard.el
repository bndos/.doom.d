;;; bndos-task-dashboard.el --- Task dashboard and launcher -*- lexical-binding: t; -*-

;;; Commentary:

;; A tree of every worktree in the known projects, each branch under its
;; recorded parent (from `wt list --format json'), merged with `bndos-task status' (parent, session
;; and agent state, from git config and the Hydra daemon) and the durable
;; review records, plus a completing-read selector and the launcher.  It
;; refreshes on `g' (and after changes made from here), like dired.
;; Starting a task runs `bndos-task start', exactly as a leader agent does:
;; worktrunk creates the worktree, and the agent runs as a Hydra session
;; given the task prompt.  Nothing here commits, pushes, merges or deletes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'map)
(require 'magit-section)
(require 'bndos-proc)
(require 'bndos-task-store)
(require 'bndos-task-git)
(require 'bndos-task-wt)
(require 'bndos-task-review)
(require 'bndos-task-agent)

(declare-function projectile-project-root "ext:projectile")
(declare-function agent-shell-select-config "ext:agent-shell")

(defcustom bndos-task-projects nil
  "Repositories always shown in the dashboard, besides those seen before."
  :type '(repeat directory)
  :group 'bndos-task)

(defconst bndos-task-dashboard-buffer "*tasks*")


;;; Known projects

(defun bndos-task--projects-file ()
  (file-name-concat bndos-task-store-directory "projects.json"))

(defun bndos-task-known-projects ()
  "Repositories to list: configured, remembered and from task records."
  (let ((remembered (when (file-exists-p (bndos-task--projects-file))
                      (ignore-errors (bndos-task-store-decode
                                      (with-temp-buffer (insert-file-contents (bndos-task--projects-file))
                                                        (buffer-string)))))))
    (seq-filter #'file-directory-p
                (delete-dups
                 (mapcar (lambda (dir) (file-name-as-directory (file-truename dir)))
                         (append bndos-task-projects remembered
                                 (delq nil (mapcar (lambda (r) (bndos-task-get r :repo :main-path))
                                                   (bndos-task-store-all)))))))))

(defun bndos-task-remember-project (dir)
  "Add repository DIR to the remembered projects."
  (let* ((dir (file-name-as-directory (file-truename dir)))
         (all (bndos-task-known-projects)))
    (unless (member dir all)
      (bndos-task-store--write-atomically (bndos-task--projects-file)
                                          (bndos-task-store-encode (append all (list dir)))))))

(defun bndos-task--common-dir-sync (directory)
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (when (zerop (process-file "git" nil t nil "rev-parse" "--path-format=absolute" "--git-common-dir"))
        (file-truename (string-trim (buffer-string)))))))

(defun bndos-task--main-path (common-dir)
  (file-name-as-directory (file-name-directory (directory-file-name common-dir))))

(defun bndos-task--current-repo ()
  (when-let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                        (locate-dominating-file default-directory ".git")))
              (common (bndos-task--common-dir-sync root)))
    (bndos-task--main-path common)))

(defun bndos-task-ensure-record (common-dir worktree branch)
  "Return the key of WORKTREE's task record, creating or updating it."
  (let* ((worktree (file-name-as-directory (file-truename worktree)))
         (key (bndos-task-key common-dir worktree))
         (main (bndos-task--main-path common-dir))
         (record (bndos-task-store-ensure
                  :key key :repo common-dir :worktree worktree :branch branch
                  :project (file-name-nondirectory (directory-file-name main))
                  :handle (file-name-nondirectory (directory-file-name worktree)))))
    (unless (and (equal (bndos-task-get record :repo :main-path) main)
                 (equal (plist-get record :branch) branch))
      (bndos-task-store-save (plist-put (bndos-task-put record '(:repo :main-path) main) :branch branch)))
    key))


;;; Starting tasks

(cl-defun bndos-task-start (repo branch base &key prompt ticket agent mode model callback)
  "Create BRANCH from BASE in a new worktree of REPO and start its agent.
Runs `bndos-task start': PROMPT is sent to the agent, TICKET recorded; AGENT
is a Hydra agent id / agent-shell config identifier (default
`bndos-task-agent-default-config'); MODE and MODEL override the command's
per-agent defaults.  CALLBACK gets (:key KEY :worktree DIR) or (:error
MESSAGE)."
  (let ((callback (or callback #'ignore))
        (agent (format "%s" (or agent (map-elt (bndos-task-agent--config nil) :identifier)))))
    (bndos-task-cli-run
     (append (list "start" "--repo" repo "--base" base "--agent" agent)
             (and ticket (list "--ticket" ticket))
             (and mode (list "--mode" mode))
             (and model (list "--model" model))
             (and prompt (list "--prompt" prompt))
             (list branch))
     (lambda (out)
       (if (not (equal (plist-get out :status) "started"))
           (funcall callback (list :error (or (plist-get out :error) "bndos-task start failed")))
         (let* ((worktree (plist-get out :worktree))
                (common (bndos-task--common-dir-sync worktree))
                (key (bndos-task-ensure-record common worktree branch)))
           (bndos-task-remember-project (bndos-task--main-path common))
           (bndos-task-store-update
            key (lambda (r)
                  (bndos-task-put (bndos-task-put r '(:base) (list :ref base))
                                  '(:agent)
                                  (list :provider agent :session-id (plist-get out :session_id)
                                        :mode (plist-get out :requested_mode)
                                        :mode-actual (plist-get out :mode)
                                        :model-actual (plist-get out :model)
                                        :resume "new" :started (bndos-task-now)))))
           (when ticket (bndos-task-store-update key (lambda (r) (plist-put r :ticket ticket))))
           (dolist (warning (plist-get out :warnings))
             (display-warning 'bndos-task (format "%s: %s" branch warning)))
           (funcall callback (list :key key :worktree worktree))
           (bndos-task--refresh-dashboard-buffer)))))))

(defun bndos-task--agent-options (agent directory)
  "AGENT's models and modes from `bndos-task models' (it asks the agent once)."
  (message "Getting %s's models…" agent)
  (let ((out (bndos-task-cli-call (list "models" "--agent" agent) directory)))
    (message nil)
    (if (equal (plist-get out :status) "failed")
        (progn (message "Couldn't list %s's models: %s" agent (plist-get out :error)) nil)
      out)))

(defun bndos-task--read-option (prompt options default)
  "Read an id from OPTIONS (plists with :id :name :description).
Empty input returns nil, which leaves the choice to DEFAULT (a description).
Any other id may be typed as well."
  (let* ((table (mapcar (lambda (o) (cons (plist-get o :id) o)) options))
         (annotate (lambda (id)
                     (when-let* ((o (cdr (assoc id table))))
                       (propertize (concat "  " (string-join (delq nil (list (plist-get o :name)
                                                                              (plist-get o :description)))
                                                             " · "))
                                   'face 'completions-annotations))))
         (choice (completing-read (format-prompt prompt default)
                                  (lambda (string pred action)
                                    (if (eq action 'metadata)
                                        `(metadata (annotation-function . ,annotate)
                                                   (display-sort-function . identity))
                                      (complete-with-action action (mapcar #'car table) string pred))))))
    (unless (string-empty-p choice) choice)))

;;;###autoload
(defun bndos/task-start (repo branch base ticket config mode &optional model)
  "Start a task: a new worktree for BRANCH from BASE in REPO, with its agent.
You write the prompt in a draft buffer.  The agent runs as a Hydra session,
in its auto mode unless MODE says otherwise, with MODEL or its default;
both are picked from what the agent offers.  Open it from the dashboard."
  (interactive
   (let* ((repo (or (bndos-task--current-repo) (read-directory-name "Repository: ")))
          (default-directory repo)
          (branch (read-string "New branch: "))
          (current (or (ignore-errors (car (process-lines "git" "symbolic-ref" "-q" "--short" "HEAD"))) ""))
          (base (completing-read "Parent branch: " (bndos-task-review--refs repo) nil nil nil nil
                                 (unless (string-empty-p current) current)))
          (ticket (read-string "Ticket id (optional): "))
          (config (progn (require 'agent-shell) (agent-shell-select-config :prompt "Agent: ")))
          (options (bndos-task--agent-options (format "%s" (map-elt config :identifier)) repo))
          (mode (bndos-task--read-option "Session mode" (plist-get options :modes)
                                         (or (plist-get options :default_mode) "the agent's default")))
          (model (bndos-task--read-option "Model" (plist-get options :models)
                                          (format "the agent's default%s"
                                                  (if-let* ((m (plist-get options :current_model)))
                                                      (format ", %s" m) "")))))
     (list repo branch base (unless (string-empty-p ticket) ticket) config mode model)))
  (when (string-empty-p branch) (user-error "A branch name is required"))
  (bndos-task-draft
   (format "task prompt for %s" branch) nil
   (lambda (prompt)
     (message "Creating %s…" branch)
     (bndos-task-start repo branch base :prompt prompt :ticket ticket :mode mode :model model
                       :agent (and config (map-elt config :identifier))
                       :callback (lambda (result)
                                   (if (plist-get result :error)
                                       (message "Couldn't start %s: %s" branch (plist-get result :message))
                                     (message "Started %s" branch)))))))

;;;###autoload
(defun bndos/task-review-here ()
  "Review the worktree containing `default-directory'."
  (interactive)
  (let ((buffer (current-buffer)))
    (bndos-task-git-identity
     default-directory
     (lambda (identity)
       (if-let* ((err (plist-get identity :error)))
           (message "Not in a git worktree: %s" err)
         (let ((key (bndos-task-ensure-record (plist-get identity :common-dir)
                                              (plist-get identity :toplevel)
                                              (plist-get identity :branch))))
           (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
             (bndos-task-review-open key))))))))

;;;###autoload
(defun bndos/task-agent-here ()
  "Open the agent-shell of the task whose worktree contains `default-directory'.
Attaches to the session recorded for its branch (see `bndos-task status')."
  (interactive)
  (bndos-task-git-identity
   default-directory
   (lambda (identity)
     (if-let* ((err (plist-get identity :error)))
         (message "Not in a git worktree: %s" err)
       (let* ((toplevel (plist-get identity :toplevel))
              (branch (plist-get identity :branch))
              (key (bndos-task-ensure-record (plist-get identity :common-dir) toplevel branch)))
         (if-let* ((live (bndos-task-agent-buffer (bndos-task-store-load key))))
             (pop-to-buffer live)
           (bndos-task-cli-run
            (list "status" toplevel)
            (lambda (status)
              (cond
               ((equal (plist-get status :status) "failed")
                (message "No task here: %s" (plist-get status :error)))
               ((not (plist-get status :session_id))
                (message "No agent session is recorded for %s" (or branch toplevel)))
               (t (bndos-task-agent-update-from-status key status)
                  (bndos-task-agent-open key))))
            :directory toplevel)))))))


;;; Bringing in existing worktrees

;; A known project's worktrees are all listed, however they were made.  These
;; register a project, record a worktree's parent, and adopt a conversation
;; the agent had there before it was a task.

;;;###autoload
(defun bndos/task-add-project (directory)
  "List the worktrees of the repository at DIRECTORY in the dashboard."
  (interactive (list (read-directory-name "Repository: ")))
  (let ((common (or (bndos-task--common-dir-sync directory)
                    (user-error "%s isn't in a git repository" directory))))
    (bndos-task-remember-project (bndos-task--main-path common))
    (message "Added %s" (abbreviate-file-name (bndos-task--main-path common)))
    (bndos-task--refresh-dashboard-buffer)))

(defun bndos-task--key-here ()
  "The task at point in the dashboard, else of the worktree containing `default-directory'."
  (or (bndos-task--key-at-point-or-nil)
      (let* ((common (or (bndos-task--common-dir-sync default-directory)
                         (user-error "Not in a git worktree")))
             (toplevel (car (process-lines "git" "rev-parse" "--show-toplevel")))
             (branch (ignore-errors (car (process-lines "git" "symbolic-ref" "-q" "--short" "HEAD")))))
        (when (equal (file-name-as-directory (file-truename toplevel)) (bndos-task--main-path common))
          (user-error "This is the repository's main worktree, not a task"))
        (bndos-task-ensure-record common toplevel branch))))

;;;###autoload
(defun bndos/task-set-parent ()
  "Record the parent branch of the task at point (or of this worktree)."
  (interactive)
  (let* ((key (bndos-task--key-here))
         (record (bndos-task-store-load key))
         (worktree (bndos-task-get record :worktree :path))
         (branch (or (plist-get record :branch) (user-error "The worktree has no branch checked out")))
         (ref (completing-read (format "Parent of %s: " branch) (bndos-task-review--refs worktree) nil t)))
    (bndos-task-git-set-base
     worktree branch ref
     (lambda (err)
       (if err
           (message "Couldn't record the parent: %s" err)
         (bndos-task-store-update key (lambda (r) (bndos-task-put r '(:base) (list :ref ref))))
         (message "%s's parent is %s" branch ref)
         (bndos-task--refresh-dashboard-buffer))))))

(cl-defun bndos-task-adopt (key agent session &key replace callback)
  "Make the AGENT conversation SESSION task KEY's session (see `bndos-task adopt').
CALLBACK gets the command's result."
  (let ((worktree (bndos-task-get (bndos-task-store-load key) :worktree :path))
        (callback (or callback #'ignore)))
    (bndos-task-cli-run
     (append (list "adopt" worktree "--agent" agent "--session" session) (and replace '("--replace")))
     (lambda (out)
       (when (equal (plist-get out :status) "adopted")
         (bndos-task-store-update
          key (lambda (r) (bndos-task-put r '(:agent)
                                          (list :provider agent :session-id (plist-get out :session_id)
                                                :upstream-session-id session :mode-actual (plist-get out :mode)
                                                :resume "adopted" :started (bndos-task-now)))))
         (bndos-task--refresh-dashboard-buffer))
       (funcall callback out))
     :directory worktree)))

;;;###autoload
(defun bndos/task-adopt ()
  "Adopt an earlier agent conversation as the task's session.
For a worktree made before it was a task: lists the chosen agent's own
conversations in it, and the one you pick becomes the session that `v'
and leaders attach to (through Hydra).  Nothing is picked for you."
  (interactive)
  (require 'agent-shell)
  (let* ((key (bndos-task--key-here))
         (record (bndos-task-store-load key))
         (worktree (bndos-task-get record :worktree :path))
         (agent (format "%s" (map-elt (agent-shell-select-config :prompt "Whose conversation: ") :identifier)))
         (out (progn (message "Listing %s conversations in %s…" agent (abbreviate-file-name worktree))
                     (bndos-task-cli-call (list "sessions" worktree "--agent" agent) worktree)))
         (sessions (or (and (not (equal (plist-get out :status) "failed")) (plist-get out :sessions))
                       (user-error "%s" (or (plist-get out :error)
                                            (format "No %s conversations in %s" agent (abbreviate-file-name worktree))))))
         (candidates (mapcar (lambda (s)
                               (cons (format "%s  · %s" (or (plist-get s :title) "untitled")
                                             (or (plist-get s :updated_at) "?"))
                                     (plist-get s :id)))
                             sessions))
         (session (cdr (assoc (completing-read "Adopt conversation: " candidates nil t) candidates)))
         (current (plist-get out :session_id))
         (replace (and current
                       (or (y-or-n-p (format "This task already has session %s; replace it? " current))
                           (user-error "Kept %s" current)))))
    (bndos-task-adopt key agent session :replace replace
                      :callback (lambda (result)
                                  (if (equal (plist-get result :status) "adopted")
                                      (message "Adopted; v (or SPC g w a) opens that conversation")
                                    (message "Couldn't adopt: %s" (plist-get result :error)))))))


;;; Inventory

(defun bndos-task--inventory (callback)
  "Collect every task row and call CALLBACK with (ROWS PROBLEMS).
Each project's worktrees come from worktrunk (for their state) and
`bndos-task status' (parent, session, agent state), in parallel."
  (let* ((projects (bndos-task-known-projects))
         (pending (* 2 (length projects)))
         (entries nil)
         (statuses (make-hash-table :test #'equal))
         (problems nil))
    (cl-flet ((done ()
                (when (zerop (cl-decf pending))
                  (funcall callback (bndos-task--rows entries statuses) problems))))
      (if (null projects)
          (run-at-time 0 nil callback (bndos-task--rows nil statuses) nil)
        (dolist (project projects)
          (let ((common (bndos-task--common-dir-sync project)))
            (bndos-task-wt-list
             project
             (lambda (out)
               (if (bndos-task-wt-error-p out)
                   (push (format "%s: %s" (abbreviate-file-name project) (plist-get out :message)) problems)
                 (dolist (wt out)
                   (unless (plist-get wt :main)
                     (push (list :common-dir common :worktree wt) entries))))
               (done)))
            (bndos-task-cli-run
             '("status") (lambda (out)
                           (if (and (plistp out) (equal (plist-get out :status) "failed"))
                               (push (format "%s: %s" (abbreviate-file-name project) (plist-get out :error))
                                     problems)
                             (dolist (status out)
                               (puthash (file-name-as-directory (plist-get status :worktree)) status statuses)))
                           (done))
             :directory project)))))))

(defun bndos-task--rows (entries statuses)
  "Merge worktree ENTRIES and STATUSES (worktree -> status) with the store."
  (let ((seen nil) rows)
    (dolist (entry entries)
      (let* ((wt (plist-get entry :worktree))
             (key (bndos-task-ensure-record (plist-get entry :common-dir) (plist-get wt :path)
                                            (plist-get wt :branch)))
             (status (gethash (file-name-as-directory (file-truename (plist-get wt :path))) statuses)))
        (when status (bndos-task-agent-update-from-status key status))
        (bndos-task-agent-reconcile key)
        (push key seen)
        (push (list :key key :record (bndos-task-store-load key) :worktree wt
                    :base (if status (or (plist-get status :parent) "none") "?"))
              rows)))
    ;; Tasks whose worktree disappeared keep their discussion: show them.
    (dolist (record (bndos-task-store-all))
      (unless (member (plist-get record :key) seen)
        (when (or (plist-get record :threads) (bndos-task-get record :agent :session-id))
          (push (list :key (plist-get record :key) :record record :missing t) rows))))
    (nreverse rows)))


;;; Dashboard

;; One tree per project: each task sits under its recorded parent, so a
;; stack reads top to bottom (main, layer-a, then layer-b on it).  A task is
;; one short line (branch, agent state, what needs you); TAB shows the rest.

(defvar bndos-task--rows nil "Rows from the last inventory.")
(defvar bndos-task--problems nil "Projects whose worktrees couldn't be listed.")
(defvar-local bndos-task--refreshing nil)

(defvar-keymap bndos-task-dashboard-mode-map
  :parent magit-section-mode-map
  "RET"     #'bndos/task-dashboard-review
  "o"       #'bndos/task-dashboard-workspace
  "v"       #'bndos/task-dashboard-agent
  "i"       #'bndos/task-dashboard-instruct
  "+"       #'bndos/task-start
  "g"       #'bndos/task-dashboard-refresh
  "C-c C-o" #'bndos/task-dashboard-agent
  "C-c C-i" #'bndos/task-dashboard-instruct
  "C-c C-n" #'bndos/task-start
  "C-c C-r" #'bndos/task-dashboard-refresh
  "C-c C-a" #'bndos/task-adopt
  "C-c C-b" #'bndos/task-set-parent
  "C-c C-p" #'bndos/task-add-project)

(define-derived-mode bndos-task-dashboard-mode magit-section-mode "Tasks"
  "Dashboard of tasks, as a tree of branches under their parents.

\\{bndos-task-dashboard-mode-map}"
  :interactive nil
  (setq-local truncate-lines t))

(defun bndos-task--tree-label (row)
  (if (plist-get row :missing)
      "gone"
    (let ((changes (bndos-task-get (plist-get row :worktree) :changes)))
      (let ((flags (concat (if (plist-get changes :staged) "S" "")
                           (if (or (plist-get changes :modified) (plist-get changes :deleted)
                                   (plist-get changes :renamed))
                               "M" "")
                           (if (plist-get changes :untracked) "U" "")
                           (if (plist-get changes :conflicted) "!" ""))))
        (if (string-empty-p flags) "clean" flags)))))

(defun bndos-task--row-branch (row)
  (or (bndos-task-get row :record :branch) (bndos-task-get row :record :worktree :handle) "?"))

(defun bndos-task--row-parent (row)
  (let ((parent (or (plist-get row :base) (bndos-task-get row :record :base :ref))))
    (unless (member parent '(nil "none" "?")) parent)))

(defun bndos-task--state-label (record)
  "The agent's state for RECORD, short, with a glyph and face."
  (let ((finished (plist-get (gethash (plist-get record :key) bndos-task-agent--hydra) :done)))
    (pcase (bndos-task-agent-lifecycle record)
      ('busy (propertize "● working" 'font-lock-face 'success))
      ('blocked (propertize "⚠ needs permission" 'font-lock-face 'error))
      ((or 'idle 'detached 'stopped)
       (if finished (propertize "✓ done" 'font-lock-face 'success) (propertize "○ idle" 'font-lock-face 'shadow)))
      ('missing (propertize "✗ session gone" 'font-lock-face 'warning))
      (_ (propertize "· no agent" 'font-lock-face 'shadow)))))

(defun bndos-task--thread-counts (record)
  "(OPEN TO-VERIFY SENT RESOLVED) thread counts of RECORD."
  (let ((states (mapcar (lambda (th) (plist-get th :state)) (plist-get record :threads))))
    (mapcar (lambda (state) (seq-count (lambda (s) (equal s state)) states))
            '("open" "addressed" "sent" "resolved"))))

(defun bndos-task--attention (row)
  "What on ROW's task needs you, as short labels."
  (let* ((record (plist-get row :record))
         (counts (bndos-task--thread-counts record))
         (request (bndos-task-in-flight-request record)))
    (string-join
     (delq nil
           (list (and (plist-get row :missing) (propertize "worktree gone" 'font-lock-face 'error))
                 (and (equal (bndos-task-get record :agent :resume) "new-conversation")
                      (propertize "NEW conversation" 'font-lock-face 'warning))
                 (and request (propertize (format "request %s" (plist-get request :status)) 'font-lock-face 'warning))
                 (and (> (nth 1 counts) 0) (propertize (format "%d to verify" (nth 1 counts)) 'font-lock-face 'success))
                 (and (> (nth 0 counts) 0) (propertize (format "%d open" (nth 0 counts)) 'font-lock-face 'warning))
                 (and (equal (bndos-task-get record :review :state) "approved")
                      (propertize "✓ approved" 'font-lock-face 'success))))
     "  ")))

(defun bndos-task--details (row)
  "Lines describing everything about ROW's task."
  (let* ((record (plist-get row :record))
         (status (gethash (plist-get record :key) bndos-task-agent--hydra))
         (counts (bndos-task--thread-counts record))
         (threads (length (plist-get record :threads))))
    (seq-filter
     #'cdr
     (list (cons "parent" (or (bndos-task--row-parent row) "none recorded"))
           (cons "worktree" (abbreviate-file-name (or (bndos-task-get record :worktree :path) "?")))
           (cons "changes" (bndos-task--tree-label row))
           (cons "agent" (let ((agent (plist-get record :agent)))
                           (and (plist-get agent :session-id)
                                (string-join (delq nil (list (plist-get agent :provider)
                                                             (plist-get agent :mode-actual)
                                                             (plist-get agent :model-actual)))
                                             " · "))))
           (cons "session" (bndos-task-get record :agent :session-id))
           (cons "last turn" (plist-get status :last_turn_ended_at))
           (cons "review" (or (bndos-task-get record :review :state) "not reviewed"))
           (cons "threads" (and (> threads 0)
                                (string-join
                                 (delq nil (cl-mapcar (lambda (n label) (and (> n 0) (format "%d %s" n label)))
                                                      (list (nth 0 counts) (nth 2 counts) (nth 1 counts) (nth 3 counts))
                                                      '("open" "sent" "to verify" "resolved")))
                                 ", ")))
           (cons "ticket" (plist-get record :ticket))))))

(defun bndos-task--insert-task (row prefix continuation width)
  (let* ((key (plist-get row :key))
         (branch (bndos-task--row-branch row))
         (lead (concat (propertize prefix 'font-lock-face 'shadow)
                      (propertize branch 'font-lock-face '(bold magit-branch-local))))
         (attention (bndos-task--attention row)))
    (magit-insert-section (bndos-task key t)
      (magit-insert-heading
        (propertize (concat lead (make-string (max 2 (- width (string-width lead))) ?\s)
                            (bndos-task--state-label (plist-get row :record))
                            (if (string-empty-p attention) "" (concat "  " attention)))
                    'bndos-task-key key))
      (pcase-dolist (`(,label . ,value) (bndos-task--details row))
        (insert (propertize continuation 'font-lock-face 'shadow)
                (propertize (format "   %-10s" label) 'font-lock-face 'font-lock-comment-face)
                value "\n")))))

(defun bndos-task--insert-project (rows)
  "Insert the project section for ROWS (all from one repository)."
  (let* ((record (plist-get (car rows) :record))
         (by-branch (mapcar (lambda (r) (cons (bndos-task--row-branch r) r)) rows))
         (width 0)
         (seen nil))
    (cl-labels
        ((depth (row trail)
           ;; Tasks above ROW in its stack (cycles stop the count).
           (let ((parent (cdr (assoc (bndos-task--row-parent row) by-branch))))
             (if (or (null parent) (memq parent trail)) 0 (1+ (depth parent (cons row trail))))))
         (children (parent)
           (sort (seq-filter (lambda (r) (equal (bndos-task--row-parent r) parent)) rows)
                 (lambda (a b) (string< (bndos-task--row-branch a) (bndos-task--row-branch b)))))
         (insert-children (parent indent)
           (let ((kids (seq-remove (lambda (r) (memq r seen)) (children parent))))
             (while kids
               (let* ((row (pop kids))
                      (last (null kids))
                      (continuation (concat indent (if last "   " "│  "))))
                 (push row seen)
                 (bndos-task--insert-task row (concat indent (if last "└─ " "├─ ")) continuation width)
                 (insert-children (bndos-task--row-branch row) continuation))))))
      ;; One state column for the whole project: after the widest branch.
      (setq width (min 48 (+ 2 (apply #'max (mapcar (lambda (r) (+ 2 (* 3 (1+ (depth r nil)))
                                                                    (string-width (bndos-task--row-branch r))))
                                                    rows)))))
      (magit-insert-section (bndos-project (bndos-task-get record :repo :main-path))
        (magit-insert-heading
          (propertize (or (bndos-task-get record :repo :project) "?") 'font-lock-face 'magit-section-heading)
          "  " (propertize (abbreviate-file-name (or (bndos-task-get record :repo :main-path) "")) 'font-lock-face 'shadow))
        ;; Roots: parents that aren't themselves tasks here (main, a remote
        ;; branch...), then tasks with no recorded parent.
        (let ((bases (sort (delete-dups
                            (seq-remove (lambda (p) (assoc p by-branch))
                                        (delq nil (mapcar #'bndos-task--row-parent rows))))
                           #'string<)))
          (dolist (base bases)
            (insert "  " (propertize base 'font-lock-face 'magit-branch-remote) "\n")
            (insert-children base "  "))
          (when (children nil)
            (insert "  " (propertize "no recorded parent" 'font-lock-face 'shadow) "\n")
            (insert-children nil "  ")))
        ;; Anything left is in a parent cycle; show it rather than lose it.
        (dolist (row rows)
          (unless (memq row seen)
            (bndos-task--insert-task row "  ? " "    " width)))
        (insert "\n")))))

(defun bndos-task--expanded-keys ()
  "Keys of the task sections currently showing their details."
  (let (keys)
    (when (bound-and-true-p magit-root-section)
      (cl-labels ((walk (section)
                    (when (and (eq (oref section type) 'bndos-task) (not (oref section hidden)))
                      (push (oref section value) keys))
                    (mapc #'walk (oref section children))))
        (walk magit-root-section)))
    keys))

(defun bndos-task--print ()
  "Render `bndos-task--rows', keeping point and expanded tasks."
  (let ((inhibit-read-only t)
        (key (bndos-task--key-at-point-or-nil))
        (expanded (bndos-task--expanded-keys))
        (groups (seq-group-by (lambda (row) (or (bndos-task-get row :record :repo :main-path) "")) bndos-task--rows)))
    (erase-buffer)
    (magit-insert-section (bndos-task-root)
      (if (null groups)
          (insert "No tasks yet.  + (C-c C-n) starts one; tasks started by leaders show up on g r.\n")
        (dolist (group (sort groups (lambda (a b) (string< (car a) (car b)))))
          (bndos-task--insert-project (cdr group)))))
    ;; Applies each section's hidden state (details start folded).
    (magit-section-show magit-root-section)
    (dolist (k expanded)
      (when-let* ((section (bndos-task--section-for k)))
        (magit-section-show section)))
    (goto-char (or (and key (bndos-task--heading-position key)) (point-min)))
    (let ((blocked (seq-count (lambda (row) (eq (bndos-task-agent-lifecycle (plist-get row :record)) 'blocked))
                              bndos-task--rows)))
      (setq header-line-format
            (if bndos-task--problems
                (propertize (concat " " (string-join bndos-task--problems "; ")) 'face 'warning)
              (concat (format " %d tasks" (length bndos-task--rows))
                      (if (> blocked 0) (propertize (format " · %d need permission" blocked) 'face 'error) "")
                      " · RET review · TAB details · v agent · i instruct · + start · g r refresh · C-c C-a adopt"))))))

(defun bndos-task--heading-position (key)
  (save-excursion
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward 'bndos-task-key key #'equal)))
      (prop-match-beginning match))))

(defun bndos-task--section-for (key)
  (when-let* ((pos (bndos-task--heading-position key)))
    (save-excursion (goto-char pos) (magit-current-section))))

(defun bndos-task--key-at-point-or-nil ()
  (when (derived-mode-p 'bndos-task-dashboard-mode)
    (let ((section (magit-current-section)))
      (while (and section (not (eq (oref section type) 'bndos-task)))
        (setq section (oref section parent)))
      (and section (oref section value)))))

(defun bndos-task-dashboard-refresh ()
  "Refresh the dashboard asynchronously."
  (let ((buffer (current-buffer)))
    (unless bndos-task--refreshing
      (setq bndos-task--refreshing t)
      (bndos-task-wt-capabilities
       (lambda (caps)
         (if-let* ((problem (bndos-task-wt-capability-problem caps)))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq bndos-task--refreshing nil
                       header-line-format (propertize (concat " " problem) 'face 'error))))
           (bndos-task--inventory
            (lambda (rows problems)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (setq bndos-task--refreshing nil
                        bndos-task--rows rows
                        bndos-task--problems problems)
                  (bndos-task--print)))))))))))

(defun bndos/task-dashboard-refresh ()
  "Refresh the task dashboard."
  (interactive)
  (bndos-task-dashboard-refresh))

;;;###autoload
(defun bndos/task-dashboard ()
  "Show the tasks in the known projects."
  (interactive)
  (when-let* ((repo (bndos-task--current-repo)))
    (bndos-task-remember-project repo))
  (let ((buffer (get-buffer-create bndos-task-dashboard-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'bndos-task-dashboard-mode)
        (bndos-task-dashboard-mode))
      (when bndos-task--rows (bndos-task--print))
      (bndos-task-dashboard-refresh))
    (pop-to-buffer-same-window buffer)))

(defun bndos-task--refresh-dashboard-buffer ()
  (when-let* ((buffer (get-buffer bndos-task-dashboard-buffer)))
    (with-current-buffer buffer (bndos-task-dashboard-refresh))))

(defun bndos-task--agent-changed-h (_key)
  "Re-render the dashboard from the store when an agent's state changes."
  (when-let* ((buffer (get-buffer bndos-task-dashboard-buffer)))
    (with-current-buffer buffer
      (when bndos-task--rows
        (dolist (row bndos-task--rows)
          (plist-put row :record (bndos-task-store-load (plist-get row :key))))
        (bndos-task--print)))))
(add-hook 'bndos-task-agent-after-change-hook #'bndos-task--agent-changed-h)

(defun bndos-task--key-at-point ()
  (or (bndos-task--key-at-point-or-nil) (user-error "No task on this line")))

(defun bndos/task-dashboard-review ()
  "Open the task's review in its own workspace."
  (interactive)
  (bndos-task-review-open (bndos-task--key-at-point) :workspace t))

(defun bndos/task-dashboard-workspace ()
  "Switch to the task's workspace without opening the review."
  (interactive)
  (let ((record (bndos-task-store-load (bndos-task--key-at-point))))
    (bndos-task-review-switch-workspace (bndos-task-get record :worktree :path))
    (dired (bndos-task-get record :worktree :path))))

(defun bndos/task-dashboard-agent ()
  "Show the task's agent-shell, attaching to its session if needed."
  (interactive)
  (bndos-task-agent-open (bndos-task--key-at-point)))

(defun bndos-task-instruct (key)
  "Draft an instruction for task KEY's agent and send it."
  (let ((record (bndos-task-store-load key)))
    (bndos-task-draft
     (format "instruction for %s" (bndos-task-get record :worktree :handle)) nil
     (lambda (body)
       (let ((thread (bndos-task-make-thread :kind "instruction" :author (user-login-name) :body body))
             (worktree (bndos-task-get record :worktree :path)))
         (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r thread)))
         (bndos-task-git-identity
          worktree
          (lambda (identity)
            (bndos-task-git-resolve-base
             worktree :branch (plist-get identity :branch)
             :callback
             (lambda (base)
               (bndos-task-git-snapshot
                worktree
                (lambda (snapshot)
                  (condition-case err
                      (bndos-task-agent-submit key (list (plist-get thread :id)) "implement"
                                               :snapshot snapshot :base base)
                    (user-error (message "%s" (error-message-string err)))))))))))))))

(defun bndos/task-dashboard-instruct ()
  "Send an instruction to the task's agent (it may edit files)."
  (interactive)
  (bndos-task-instruct (bndos-task--key-at-point)))


;;; Quick selector

;;;###autoload
(defun bndos/task-select ()
  "Pick a task and open its review in its workspace."
  (interactive)
  (let* ((candidates (or (mapcar (lambda (record)
                                   (cons (format "%s/%s" (bndos-task-get record :repo :project)
                                                 (bndos-task-get record :worktree :handle))
                                         record))
                                 (seq-filter (lambda (r) (file-directory-p (or (bndos-task-get r :worktree :path) "")))
                                             (bndos-task-store-all)))
                         (user-error "No tasks recorded yet; open the dashboard first")))
         (annotate (lambda (name)
                     (let ((record (alist-get name candidates nil nil #'equal)))
                       (concat (propertize " " 'display '(space :align-to 32))
                               (propertize (format "%-22s %-12s %s"
                                                   (plist-get record :branch)
                                                   (or (bndos-task-get record :review :state) "")
                                                   (substring-no-properties (bndos-task-agent-summary record)))
                                           'face 'completions-annotations)))))
         (choice (completing-read "Task: "
                                  (lambda (string pred action)
                                    (if (eq action 'metadata)
                                        `(metadata (category . bndos-task)
                                                   (annotation-function . ,annotate))
                                      (complete-with-action action candidates string pred)))
                                  nil t)))
    (bndos-task-review-open (plist-get (alist-get choice candidates nil nil #'equal) :key)
                            :workspace t)))

(provide 'bndos-task-dashboard)
;;; bndos-task-dashboard.el ends here
