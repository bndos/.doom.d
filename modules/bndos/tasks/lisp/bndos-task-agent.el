;;; bndos-task-agent.el --- A task's agent-shell -*- lexical-binding: t; -*-

;;; Commentary:

;; A task's agent is a session in the Hydra ACP daemon, started by
;; bin/bndos-task (from Emacs or a leader agent) and recorded in git config
;; (branch.<branch>.bndos-session and -agent).  It runs whether or not Emacs
;; is open.  Opening the task attaches an agent-shell to that session through
;; Hydra's shim (`hydra-acp launch AGENT', resuming the recorded id): any
;; number of clients can be attached, and a permission request goes to all
;; of them, the first answer winning.  If the session can't be loaded the
;; shell starts a new one, labelled NEW and recorded, never passed off as a
;; resume.
;;
;; Responses.  Review requests ask the agent to end with one fenced block:
;;
;;   ```bndos-review
;;   {"protocol": "bndos-review/1", "task": KEY, "request": REQUEST-ID,
;;    "snapshot": TREE, "responses": [{"comment": ID, "status": STATUS,
;;    "body": TEXT, "files": [PATH...], "tests": [NAME...]}]}
;;   ```
;;
;; STATUS is "addressed" / "not-addressed" (implement requests only),
;; "answered" or "needs-info".  The block is validated against the task, the
;; request, its snapshot and the comment ids actually submitted; prose or a
;; finished turn alone never changes a thread's state.  It's read from the
;; turn's streamed message chunks.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'map)
(require 'bndos-proc)
(require 'bndos-task-store)
(require 'bndos-task-git)
(require 'bndos-task-review)

(declare-function agent-shell-start "ext:agent-shell")
(declare-function agent-shell-session-id "ext:agent-shell")
(declare-function agent-shell-status "ext:agent-shell")
(declare-function agent-shell-insert "ext:agent-shell")
(declare-function agent-shell-buffers "ext:agent-shell")
(declare-function agent-shell-subscribe-to "ext:agent-shell")
(declare-function agent-shell-unsubscribe "ext:agent-shell")
(declare-function agent-shell-config-option-value "ext:agent-shell")
(declare-function agent-shell--make-acp-client "ext:agent-shell")
(defvar agent-shell-cwd-function)
(defvar agent-shell-session-strategy)
(defvar agent-shell-agent-configs)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell--state)

(defconst bndos-task-agent--bin
  (expand-file-name "../bin/" (file-name-directory (or load-file-name buffer-file-name)))
  "This module's bin directory.")

(defcustom bndos-task-cli (expand-file-name "bndos-task" bndos-task-agent--bin)
  "The bndos-task command (start, send, status)."
  :type 'file
  :group 'bndos-task)

(defcustom bndos-task-hydra-command '("hydra-acp")
  "Command running Hydra's ACP shim, before its arguments."
  :type '(repeat string)
  :group 'bndos-task)

(defcustom bndos-task-agent-default-config 'claude-code
  "Identifier of the agent new tasks use when none is chosen.
It's both an agent-shell config identifier and a Hydra agent id.  nil means
`agent-shell-preferred-agent-config'."
  :type '(choice (const :tag "agent-shell's preferred config" nil) symbol)
  :group 'bndos-task)

(defcustom bndos-task-agent-submit-timeout 120
  "Seconds for a request to reach the agent (including attaching) before it fails."
  :type 'integer
  :group 'bndos-task)

(defconst bndos-task-agent-protocol "bndos-review/1")

(defvar bndos-task-agent--shells (make-hash-table :test #'equal)
  "Task key -> agent-shell buffer attached for it.")

(defvar bndos-task-agent--hydra (make-hash-table :test #'equal)
  "Task key -> its entry from the last `bndos-task status' (a plist).")

(defvar bndos-task-agent-after-change-hook nil
  "Run with a task key after its agent state or requests change.")


;;; Configs

(defun bndos-task-agent-configs ()
  "agent-shell's known agent configs, with maker functions realized."
  (require 'agent-shell)
  (mapcar (lambda (entry) (if (functionp entry) (funcall entry) entry))
          (if (functionp agent-shell-agent-configs)
              (funcall agent-shell-agent-configs)
            agent-shell-agent-configs)))

(defun bndos-task-agent--config (identifier)
  "The agent-shell config whose :identifier is IDENTIFIER (a symbol or string)."
  (let* ((id (or (and identifier (intern-soft (format "%s" identifier)))
                 bndos-task-agent-default-config
                 (bound-and-true-p agent-shell-preferred-agent-config)))
         (config (seq-find (lambda (c) (eq (map-elt c :identifier) id))
                           (bndos-task-agent-configs))))
    (or config (user-error "No agent-shell config with identifier %s" id))))

(defun bndos-task-agent--hydra-config (provider)
  "An agent-shell config reaching Hydra agent PROVIDER through Hydra's shim.
It looks like agent-shell's own config for PROVIDER (name, icon, prompt)
when there is one.  Mode and model aren't applied on attach: the session
keeps whatever it's in, and Hydra restores them when it revives it."
  (let ((config (copy-alist (or (ignore-errors (bndos-task-agent--config provider))
                                (bndos-task-agent--config nil)))))
    (setf (alist-get :client-maker config)
          (lambda (buffer)
            (agent-shell--make-acp-client
             :command (car bndos-task-hydra-command)
             :command-params (append (cdr bndos-task-hydra-command) (list "launch" provider))
             :environment-variables '("NO_UPDATE_NOTIFIER=1")
             :context-buffer buffer)))
    (setf (alist-get :default-session-mode-id config) nil
          (alist-get :default-model-id config) nil)
    config))

(defun bndos-task-agent--session-setting (buffer key)
  "KEY (:mode-id or :model-id) of BUFFER's live session.
Agents exposing it as a config option (Codex) are read through
`agent-shell-config-option-value'; otherwise agent-shell's session state,
for which it has no public getter."
  (or (ignore-errors
        (agent-shell-config-option-value :shell-buffer buffer
                                         :category (if (eq key :mode-id) "mode" "model")))
      (with-current-buffer buffer
        (and (boundp 'agent-shell--state)
             (map-nested-elt agent-shell--state (list :session key))))))


;;; The bndos-task command

(cl-defun bndos-task-cli-run (args callback &key directory)
  "Run `bndos-task' with ARGS and call CALLBACK with its parsed JSON output.
A failure is passed as (:status \"failed\" :error MESSAGE)."
  (bndos-proc-run
   bndos-task-cli args :directory directory
   :env (list (concat "BNDOS_TASK_STATE_DIR=" (expand-file-name bndos-task-store-directory))
              (concat "BNDOS_TASK_HYDRA=" (car bndos-task-hydra-command)))
   :callback (lambda (result)
               (let ((parsed (condition-case nil
                                 (list (bndos-task-store-decode (plist-get result :stdout)))
                               (error nil))))
                 (funcall callback
                          (if parsed
                              (car parsed)
                            (list :status "failed" :error (bndos-proc-error-message result))))))))

(defun bndos-task-agent-update-from-status (key status)
  "Remember task KEY's STATUS entry and copy what it records into the store.
The recorded session, agent, parent and ticket live in git config (where
`bndos-task start' puts them, with or without Emacs); the store follows."
  (puthash key status bndos-task-agent--hydra)
  (bndos-task-store-update
   key (lambda (record)
         (let ((agent (plist-get record :agent))
               (sid (plist-get status :session_id)))
           (when (and sid (not (equal sid (plist-get agent :session-id))))
             (setq agent (plist-put (plist-put agent :session-id sid) :resume nil)))
           (when-let* ((provider (plist-get status :agent)))
             (setq agent (plist-put agent :provider provider)))
           (when-let* ((model (plist-get status :model)))
             (setq agent (plist-put agent :model-actual model)))
           (let ((record (plist-put record :agent agent)))
             (when-let* ((ticket (plist-get status :ticket)))
               (setq record (plist-put record :ticket ticket)))
             (bndos-task-put record '(:base :ref) (plist-get status :parent)))))))


;;; Locating a task's shell

(defun bndos-task-agent--worktree (record)
  (file-name-as-directory (file-truename (bndos-task-get record :worktree :path))))

(defun bndos-task-agent-buffer (record)
  "Return RECORD's live agent-shell buffer, adopting one attached earlier.
A buffer is adopted only when its session id is RECORD's and its directory
is the task's worktree."
  (let* ((key (plist-get record :key))
         (known (gethash key bndos-task-agent--shells)))
    (if (buffer-live-p known)
        known
      (remhash key bndos-task-agent--shells)
      (when-let* (((featurep 'agent-shell))
                  (sid (bndos-task-get record :agent :session-id))
                  (worktree (bndos-task-agent--worktree record))
                  (buffer (seq-find (lambda (buffer)
                                      (and (equal (agent-shell-session-id :shell-buffer buffer) sid)
                                           (equal (file-name-as-directory
                                                   (file-truename (buffer-local-value 'default-directory buffer)))
                                                  worktree)))
                                    (agent-shell-buffers))))
        (bndos-task-agent--watch key buffer nil nil)
        buffer))))

(defun bndos-task-agent--short (id)
  (let ((id (and id (string-remove-prefix "hydra_session_" id))))
    (if (and id (> (length id) 8)) (substring id 0 8) (or id "?"))))

(defun bndos-task-agent-lifecycle (record)
  "RECORD's agent: `busy', `blocked', `idle', `detached', `stopped', `missing' or `none'.
An attached shell answers for itself; otherwise the last `bndos-task
status' does (refresh the dashboard to update it)."
  (let* ((buffer (bndos-task-agent-buffer record))
         (local (and buffer (pcase (agent-shell-status :shell-buffer buffer)
                              ('busy 'busy) ('blocked 'blocked) (_ 'idle))))
         (remote (pcase (plist-get (gethash (plist-get record :key) bndos-task-agent--hydra) :state)
                   ("working" 'busy) ("waiting" 'blocked) ("idle" 'detached)
                   ("stopped" 'stopped) ("missing" 'missing) ("none" 'none))))
    (cond
     ;; Another client's turn (a leader's prompt) doesn't make our shell busy.
     ((and (eq local 'idle) (memq remote '(busy blocked))) remote)
     (local)
     (remote)
     ((bndos-task-get record :agent :session-id) 'stopped)
     (t 'none))))

(defun bndos-task-agent-summary (record)
  "One-line description of RECORD's agent for headers and the dashboard."
  (let ((agent (plist-get record :agent)))
    (concat
     (pcase (bndos-task-agent-lifecycle record)
       ('busy "working")
       ('blocked "waiting for permission")
       ('idle "idle")
       ('detached "idle (not attached)")
       ('stopped "stopped (resumes on open)")
       ('missing (propertize "session gone" 'face 'warning))
       (_ "no agent"))
     (if-let* ((sid (plist-get agent :session-id)))
         (format " · %s %s" (or (plist-get agent :provider) "agent") (bndos-task-agent--short sid))
       "")
     (if-let* ((mode (plist-get agent :mode-actual)))
         (format " · %s" mode)
       "")
     (if (equal (plist-get agent :resume) "new-conversation")
         (propertize " · NEW conversation (resume failed)" 'face 'warning)
       "")
     (if-let* ((request (bndos-task-in-flight-request record)))
         (format " · request %s %s" (plist-get request :id) (plist-get request :status))
       ""))))


;;; Watching a shell

(defvar-local bndos-task-agent--watch nil
  "In an agent-shell buffer: (:key KEY :tokens TOKENS :expect SESSION-ID
:on-ready FNS :pending ID :request ID :chunks LIST).")

(defun bndos-task-agent--notify (key)
  (run-hook-with-args 'bndos-task-agent-after-change-hook key)
  (when-let* ((buffer (bndos-task-review-buffer key)))
    (with-current-buffer buffer (bndos-task-review-render))))

(defun bndos-task-agent--watch (key buffer expected on-ready)
  "Track agent-shell BUFFER for task KEY.
On init, verify EXPECTED (the session id being attached), then call ON-READY
with BUFFER."
  (puthash key buffer bndos-task-agent--shells)
  (with-current-buffer buffer
    (unless bndos-task-agent--watch
      (setq bndos-task-agent--watch
            (list :key key :expect expected :on-ready (and on-ready (list on-ready))
                  :tokens (list (agent-shell-subscribe-to
                                 :shell-buffer buffer
                                 :on-event (lambda (event)
                                             (bndos-task-agent--on-event buffer event)))))))))

(defun bndos-task-agent--on-event (buffer event)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((watch bndos-task-agent--watch)
             (key (plist-get watch :key))
             (data (map-elt event :data)))
        (when key
          (condition-case err
              (pcase (map-elt event :event)
                ('init-finished (bndos-task-agent--on-init key buffer))
                ('input-submitted (bndos-task-agent--on-input key (map-elt data :prompt)))
                ((or 'permission-request 'permission-response) (bndos-task-agent--notify key))
                ('agent-message-chunk
                 (when (plist-get watch :request)
                   (when-let* ((chunk (map-elt data :text-chunk)))
                     (setq bndos-task-agent--watch
                           (plist-put watch :chunks (cons chunk (plist-get watch :chunks)))))))
                ('turn-complete (bndos-task-agent--on-turn-complete key (map-elt data :stop-reason))
                                (bndos-task-agent--notify key))
                ('error (when (plist-get watch :request)
                          (bndos-task-agent--finish-request
                           key (plist-get watch :request) "failed"
                           (format "agent error: %s" (map-elt data :message)))))
                ('clean-up (bndos-task-agent--on-clean-up key buffer)))
            (error (message "bndos-task-agent: %s handling %s" (error-message-string err)
                            (map-elt event :event)))))))))

(defun bndos-task-agent--on-init (key buffer)
  "Record the session BUFFER attached for KEY, then run its ready callbacks."
  (let* ((watch bndos-task-agent--watch)
         (expected (plist-get watch :expect))
         (actual (agent-shell-session-id :shell-buffer buffer))
         (failed (and expected (not (equal expected actual))))
         (record (bndos-task-store-update
                  key (lambda (record)
                        (let ((agent (plist-get record :agent)))
                          (setq agent (plist-put agent :session-id actual))
                          (setq agent (plist-put agent :resume (cond (failed "new-conversation")
                                                                     (expected "resumed")
                                                                     (t "new"))))
                          (when failed
                            (setq agent (plist-put agent :previous-session-ids
                                                   (append (plist-get agent :previous-session-ids)
                                                           (list expected)))))
                          (setq agent (plist-put agent :mode-actual (bndos-task-agent--session-setting buffer :mode-id)))
                          (setq agent (plist-put agent :model-actual (bndos-task-agent--session-setting buffer :model-id)))
                          (plist-put record :agent agent))))))
    (if failed
        (progn
          ;; Leaders and `bndos-task status' find the session through git config.
          (when-let* ((branch (plist-get record :branch)))
            (bndos-task-git (bndos-task-agent--worktree record)
                            (list "config" (format "branch.%s.bndos-session" branch) actual)
                            #'ignore))
          (display-warning 'bndos-task
                           (format "Couldn't load session %s; started NEW conversation %s.  The earlier conversation isn't in it."
                                   expected (or actual "?"))))
      (when expected (message "Attached to session %s" (bndos-task-agent--short actual))))
    (let ((callbacks (plist-get watch :on-ready)))
      (setq bndos-task-agent--watch (plist-put watch :on-ready nil))
      (dolist (fn callbacks) (funcall fn buffer)))
    (bndos-task-agent--notify key)))

(defun bndos-task-agent--on-clean-up (key buffer)
  (remhash key bndos-task-agent--shells)
  (with-current-buffer buffer
    (when-let* ((request (or (plist-get bndos-task-agent--watch :request)
                             (plist-get bndos-task-agent--watch :pending))))
      (bndos-task-agent--finish-request key request "interrupted" "the agent-shell was closed"))
    (dolist (token (plist-get bndos-task-agent--watch :tokens))
      (ignore-errors (agent-shell-unsubscribe :subscription token)))
    (setq bndos-task-agent--watch nil))
  (bndos-task-agent--notify key))


;;; Attaching

(cl-defun bndos-task-agent--start-shell (key &key display on-ready)
  "Attach an agent-shell to task KEY's session, in its worktree.
Without a recorded session, start a new conversation with the task's agent.
Unless DISPLAY, the buffer starts without being shown.  ON-READY is called
with the buffer once its session is initialized."
  (require 'agent-shell)
  (let* ((record (bndos-task-store-load key))
         (session-id (bndos-task-get record :agent :session-id))
         (provider (or (bndos-task-get record :agent :provider)
                       (format "%s" (map-elt (bndos-task-agent--config nil) :identifier))))
         (config (bndos-task-agent--hydra-config provider))
         (worktree (bndos-task-agent--worktree record))
         (default-directory worktree)
         (agent-shell-cwd-function (lambda () worktree))
         ;; If the session can't be loaded, start a visibly new one rather than
         ;; prompting with, or silently loading, another conversation.
         (agent-shell-session-strategy 'new)
         (buffer (if display
                     (agent-shell-start :config config :session-id session-id)
                   (save-window-excursion
                     (agent-shell-start :config config :session-id session-id)))))
    (bndos-task-store-update key (lambda (r) (bndos-task-put r '(:agent :provider) provider)))
    (bndos-task-agent--watch key buffer session-id on-ready)
    buffer))

(cl-defun bndos-task-agent-ensure (key callback &key display)
  "Call CALLBACK with an agent-shell attached to task KEY's session.
An attached shell is used as is; otherwise one is attached (see
`bndos-task-agent--start-shell')."
  (let ((live (bndos-task-agent-buffer (bndos-task-store-load key))))
    (cond
     ((and live (with-current-buffer live (plist-get bndos-task-agent--watch :on-ready)))
      ;; Still initializing: run once it's ready.
      (with-current-buffer live
        (setq bndos-task-agent--watch
              (plist-put bndos-task-agent--watch :on-ready
                         (append (plist-get bndos-task-agent--watch :on-ready) (list callback))))))
     (live (funcall callback live))
     (t (bndos-task-agent--start-shell key :display display :on-ready callback)))))

(defun bndos-task-agent-open (key)
  "Show task KEY's agent-shell, attaching to its session if needed."
  (interactive (list (or (bound-and-true-p bndos-task-review-key) (user-error "No task here"))))
  (if-let* ((live (bndos-task-agent-buffer (bndos-task-store-load key))))
      (pop-to-buffer live)
    (pop-to-buffer (bndos-task-agent--start-shell key :display t))))


;;; Prompts

(defun bndos-task-agent--thread-prompt (thread)
  (let ((anchor (plist-get thread :anchor)))
    (concat
     (format "### %s (%s by %s)\n" (plist-get thread :id) (plist-get thread :kind) (plist-get thread :author))
     (cond
      ((null anchor) "")
      ((plist-get anchor :line)
       (format "Location: %s line %s%s on the %s side%s\n"
               (plist-get anchor :path) (plist-get anchor :line)
               (if-let* ((end (plist-get anchor :end-line))) (format "-%s" end) "")
               (if (equal (plist-get anchor :side) "old") "old (base)" "new (worktree)")
               (if (plist-get thread :stale) " (anchor may be stale: the code moved)" "")))
      (t (format "File: %s\n" (plist-get anchor :path))))
     (if-let* ((hunk (plist-get anchor :hunk))) (format "Hunk: %s\n" hunk) "")
     (if-let* ((sel (plist-get anchor :selection)))
         (format "Selected diff:\n```diff\n%s\n```\n" sel) "")
     (format "\n%s\n" (plist-get thread :body))
     (let ((replies (seq-take (reverse (seq-filter (lambda (r) (not (equal (plist-get r :kind) "state")))
                                                   (plist-get thread :replies)))
                              4)))
       (if replies
           (concat "\nEarlier in this thread (newest first):\n"
                   (mapconcat (lambda (r) (format "- %s: %s" (plist-get r :author) (plist-get r :body)))
                              replies "\n")
                   "\n")
         "")))))

(defun bndos-task-agent-prompt (record request threads)
  "Build the prompt text for REQUEST about THREADS of RECORD."
  (let ((implement (equal (plist-get request :mode) "implement")))
    (concat
     (format "[bndos-review request %s · task %s · snapshot %s]\n\n"
             (plist-get request :id) (plist-get record :key) (plist-get request :snapshot))
     (format "Review of your work in %s (branch %s, compared against %s at merge-base %s).\n"
             (bndos-task-get record :worktree :path) (plist-get record :branch)
             (or (plist-get request :base-ref) "?")
             (bndos-task-agent--short (plist-get request :merge-base)))
     (if implement
         "Mode: IMPLEMENT. You may edit files in this worktree to address the items below. Do not commit, push, open pull requests, merge, or delete branches or worktrees unless an item explicitly asks for it.\n"
       "Mode: DISCUSS ONLY. Answer the questions; do NOT modify any files, run formatters, or make commits.\n")
     "\n"
     (mapconcat #'bndos-task-agent--thread-prompt threads "\n")
     "\n---\n"
     "End your reply with exactly one fenced block in this format (valid JSON, one entry per item above):\n\n"
     "```bndos-review\n"
     (bndos-task-store-encode
      (list :protocol bndos-task-agent-protocol :task (plist-get record :key)
            :request (plist-get request :id) :snapshot (plist-get request :snapshot)
            :responses (mapcar (lambda (th)
                                 (list :comment (plist-get th :id) :status "STATUS" :body "..."
                                       :files '("path") :tests '("test")))
                               threads)))
     "\n```\n\n"
     (format "status is one of: %s.  "
             (if implement "addressed | not-addressed | answered | needs-info" "answered | needs-info"))
     (if implement
         "Use \"addressed\" only for items you changed code for; list the files you changed and the tests you ran.  "
       "")
     "Only use the comment ids listed above.\n")))


;;; Submitting

(cl-defun bndos-task-agent-submit (key ids mode &key delegate snapshot base)
  "Send threads IDS of task KEY to its agent in MODE (\"implement\" or \"explain\").
The agent is resumed first if it isn't running.  DELEGATE lets an
\"addressed\" report resolve a thread.  SNAPSHOT and BASE describe what the
reviewer was looking at."
  (let* ((record (bndos-task-store-load key))
         (threads (mapcar (lambda (id) (or (bndos-task-thread record id) (user-error "No thread %s" id))) ids))
         (live (bndos-task-agent-buffer record)))
    (when-let* ((pending (bndos-task-in-flight-request record)))
      (user-error "Request %s is still %s; wait for it (or for its turn to end)"
                  (plist-get pending :id) (plist-get pending :status)))
    (when-let* ((sent (seq-find (lambda (th) (member (plist-get th :state) '("sent" "resolved"))) threads)))
      (user-error "Thread %s is %s" (plist-get sent :id) (plist-get sent :state)))
    (unless (plist-get snapshot :tree)
      (user-error "No snapshot yet; refresh the review first"))
    (when (and live (not (eq (agent-shell-status :shell-buffer live) 'ready)))
      (user-error "The task's agent is busy; wait for its turn to end"))
    (let* ((request (list :id (bndos-task-new-id "r") :mode mode :delegate (and delegate t)
                          :comments ids :snapshot (plist-get snapshot :tree)
                          :base-ref (plist-get base :ref) :merge-base (plist-get base :merge-base)
                          :status "sending" :created (bndos-task-now)))
           (id (plist-get request :id))
           (text (bndos-task-agent-prompt record request threads)))
      (bndos-task-agent--record-request key request)
      (when (equal mode "implement")
        (bndos-task-review-record-iteration key snapshot (format "before request %s" id) id))
      (run-at-time bndos-task-agent-submit-timeout nil
                   (lambda ()
                     (when (equal (plist-get (bndos-task-request (bndos-task-store-load key) id) :status) "sending")
                       (bndos-task-agent--finish-request key id "failed" "the agent never accepted the prompt"))))
      (bndos-task-agent-ensure
       key (lambda (buffer)
             (if (not (eq (agent-shell-status :shell-buffer buffer) 'ready))
                 (bndos-task-agent--finish-request key id "failed" "the agent was busy")
               (with-current-buffer buffer
                 (setq bndos-task-agent--watch
                       (plist-put (plist-put bndos-task-agent--watch :pending id) :chunks nil)))
               (agent-shell-insert :text text :submit t :no-focus t :shell-buffer buffer))))
      (bndos-task-agent--notify key)
      request)))

(defun bndos-task-agent--record-request (key request)
  (bndos-task-store-update
   key (lambda (record)
         (let ((record (plist-put record :requests (append (plist-get record :requests) (list request)))))
           (dolist (id (plist-get request :comments) record)
             (setq record (bndos-task-update-thread
                           record id
                           (lambda (th)
                             (plist-put (bndos-task-thread-set-state th "sent")
                                        :submissions (append (plist-get th :submissions)
                                                             (list (list :request (plist-get request :id)
                                                                         :mode (plist-get request :mode)
                                                                         :at (bndos-task-now)))))))))))))

(defun bndos-task-agent--on-input (key prompt)
  "Start collecting the reply when PROMPT is our pending request."
  (let ((pending (plist-get bndos-task-agent--watch :pending)))
    (when (and pending prompt (string-search (format "[bndos-review request %s " pending) prompt))
      (setq bndos-task-agent--watch
            (plist-put (plist-put (plist-put bndos-task-agent--watch :pending nil) :request pending)
                       :chunks nil))
      (bndos-task-store-update key (lambda (r) (bndos-task-update-request
                                                r pending (lambda (q) (plist-put q :status "in-progress")))))
      (bndos-task-agent--notify key))))


;;; Responses

(defun bndos-task-agent-extract-blocks (text)
  "Return the contents of every ```bndos-review fenced block in TEXT."
  (let (blocks (start 0))
    (while (string-match "^```bndos-review[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n```" text start)
      (push (match-string 1 text) blocks)
      (setq start (match-end 0)))
    (nreverse blocks)))

(defun bndos-task-agent-validate-response (object record request)
  "Validate parsed response OBJECT for REQUEST of RECORD.
Return (:ok RESPONSES) or (:error REASON)."
  (let* ((allowed (if (equal (plist-get request :mode) "implement")
                      '("addressed" "not-addressed" "answered" "needs-info")
                    '("answered" "needs-info")))
         (responses (plist-get object :responses))
         (ids (mapcar (lambda (r) (plist-get r :comment)) responses))
         (unknown (seq-find (lambda (id) (not (member id (plist-get request :comments)))) ids))
         (bad-status (seq-find (lambda (r) (not (member (plist-get r :status) allowed))) responses)))
    (cond
     ((not (equal (plist-get object :protocol) bndos-task-agent-protocol))
      (list :error (format "protocol %S, expected %S" (plist-get object :protocol) bndos-task-agent-protocol)))
     ((not (equal (plist-get object :task) (plist-get record :key)))
      (list :error (format "response is for task %S" (plist-get object :task))))
     ((not (equal (plist-get object :request) (plist-get request :id)))
      (list :error (format "response is for request %S" (plist-get object :request))))
     ((not (equal (plist-get object :snapshot) (plist-get request :snapshot)))
      (list :error (format "response is for snapshot %S" (plist-get object :snapshot))))
     ((not (listp responses)) (list :error "responses is not a list"))
     ((/= (length ids) (length (delete-dups (copy-sequence ids))))
      (list :error "a comment id appears more than once"))
     (unknown (list :error (format "unknown comment id %s" unknown)))
     (bad-status (list :error (format "status %S isn't allowed for a %s request"
                                      (plist-get bad-status :status) (plist-get request :mode))))
     ((seq-find (lambda (r) (not (stringp (plist-get r :body)))) responses)
      (list :error "a response has no body"))
     ((seq-find (lambda (r) (seq-remove #'stringp (append (plist-get r :files) (plist-get r :tests))))
                responses)
      (list :error "files and tests must be lists of strings"))
     (t (list :ok responses)))))

(defun bndos-task-agent-apply-response (record request responses)
  "Return RECORD with validated RESPONSES to REQUEST applied to its threads."
  (let* ((author (format "agent:%s" (or (bndos-task-get record :agent :provider) "agent")))
         (id (plist-get request :id))
         (answered nil))
    (dolist (response responses)
      (let ((cid (plist-get response :comment))
            (status (plist-get response :status)))
        (push cid answered)
        (setq record
              (bndos-task-update-thread
               record cid
               (lambda (th)
                 (let ((th (bndos-task-thread-add-reply
                            th (bndos-task-make-reply :author author :kind "agent-response"
                                                      :status status :body (plist-get response :body)
                                                      :files (plist-get response :files)
                                                      :tests (plist-get response :tests)
                                                      :request id))))
                   (cond
                    ((and (equal status "addressed") (plist-get request :delegate))
                     (bndos-task-thread-set-state th "resolved" "system"
                                                  (format "resolved on the agent's report (delegated in %s)" id)))
                    ((equal status "addressed") (bndos-task-thread-set-state th "addressed"))
                    (t (bndos-task-thread-set-state th "open")))))))))
    (dolist (cid (plist-get request :comments))
      (unless (member cid answered)
        (setq record (bndos-task-update-thread
                      record cid (lambda (th) (bndos-task-thread-set-state
                                               th "open" "system" (format "no answer in %s" id)))))))
    (bndos-task-update-request
     record id (lambda (q)
                 (plist-put (plist-put q :status (if (seq-every-p (lambda (c) (member c answered))
                                                                  (plist-get request :comments))
                                                     "answered" "partial"))
                            :completed (bndos-task-now))))))

(defun bndos-task-agent--finish-request (key id status note &optional raw)
  "End request ID of KEY in STATUS without a valid response; reopen its threads."
  (bndos-task-store-update
   key (lambda (record)
         (let ((request (bndos-task-request record id)))
           (if (not (member (plist-get request :status) '("sending" "in-progress")))
               record
             (dolist (cid (plist-get request :comments))
               (setq record (bndos-task-update-thread
                             record cid (lambda (th)
                                          (if (equal (plist-get th :state) "sent")
                                              (bndos-task-thread-set-state th "open" "system"
                                                                           (format "%s: %s" id note))
                                            th)))))
             (bndos-task-update-request record id (lambda (q) (plist-put (plist-put (plist-put q :status status)
                                                                                    :note note)
                                                                         :raw raw)))))))
  (when-let* ((buffer (gethash key bndos-task-agent--shells)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (member id (list (plist-get bndos-task-agent--watch :request)
                               (plist-get bndos-task-agent--watch :pending)))
          (setq bndos-task-agent--watch
                (plist-put (plist-put bndos-task-agent--watch :request nil) :pending nil))))))
  (message "Request %s %s: %s" id status note)
  (bndos-task-agent--notify key))

(defun bndos-task-agent-accept-text (key id text)
  "Validate and apply the response for request ID of KEY found in TEXT.
Return the resulting request status."
  (let* ((record (bndos-task-store-load key))
         (request (bndos-task-request record id))
         (results (mapcar (lambda (block)
                            (condition-case err
                                (bndos-task-agent-validate-response (bndos-task-store-decode block) record request)
                              (error (list :error (format "invalid JSON: %s" (error-message-string err))))))
                          (bndos-task-agent-extract-blocks text)))
         (ok (seq-find (lambda (r) (plist-get r :ok)) results)))
    (cond
     (ok
      (bndos-task-store-save (bndos-task-agent-apply-response record request (plist-get ok :ok)))
      (bndos-task-agent--notify key)
      (plist-get (bndos-task-request (bndos-task-store-load key) id) :status))
     (results
      (bndos-task-agent--finish-request key id "invalid-response" (plist-get (car results) :error)
                                        (truncate-string-to-width text 4000))
      "invalid-response")
     (t
      (bndos-task-agent--finish-request key id "no-response" "the reply had no bndos-review block"
                                        (truncate-string-to-width text 4000))
      "no-response"))))

(defun bndos-task-agent--on-turn-complete (key stop-reason)
  (let* ((watch bndos-task-agent--watch)
         (id (plist-get watch :request))
         (text (apply #'concat (reverse (plist-get watch :chunks)))))
    (setq bndos-task-agent--watch (plist-put (plist-put watch :request nil) :chunks nil))
    (bndos-task-store-update key (lambda (r) (bndos-task-put r '(:agent :last-turn)
                                                             (list :stop-reason (format "%s" stop-reason)
                                                                   :at (bndos-task-now)
                                                                   :epoch (truncate (float-time))))))
    (when id
      (if (member (format "%s" stop-reason) '("cancelled" "canceled"))
          (bndos-task-agent--finish-request key id "cancelled" "the turn was cancelled")
        (bndos-task-agent-accept-text key id text)
        (bndos-task-agent--after-request key id)))))

(defun bndos-task-agent--after-request (key id)
  "Refresh KEY's review; flag edits made during a discussion-only request."
  (let* ((record (bndos-task-store-load key))
         (request (bndos-task-request record id)))
    (bndos-task-git-snapshot
     (bndos-task-agent--worktree record)
     (lambda (snapshot)
       (when (and (equal (plist-get request :mode) "explain")
                  (plist-get snapshot :tree)
                  (not (equal (plist-get snapshot :tree) (plist-get request :snapshot))))
         (bndos-task-store-update
          key (lambda (r)
                (let ((r (bndos-task-update-request r id (lambda (q) (plist-put q :edited-during-discussion t)))))
                  (dolist (cid (plist-get request :comments) r)
                    (setq r (bndos-task-update-thread
                             r cid (lambda (th)
                                     (bndos-task-thread-add-reply
                                      th (bndos-task-make-reply
                                          :author "system" :kind "warning"
                                          :body (format "The worktree changed during discussion-only request %s." id))))))))))
         (display-warning 'bndos-task (format "The agent changed files during discussion-only request %s" id)))
       (when-let* ((buffer (bndos-task-review-buffer key)))
         (with-current-buffer buffer (bndos-task-review-refresh)))))))

(defun bndos-task-agent-reconcile (key)
  "Fail KEY's requests whose shell is gone (e.g. after a restart).
A reply arriving while no shell of ours is attached can't be read."
  (let ((record (bndos-task-store-load key)))
    (unless (bndos-task-agent-buffer record)
      (dolist (request (plist-get record :requests))
        (when (member (plist-get request :status) '("sending" "in-progress"))
          (bndos-task-agent--finish-request key (plist-get request :id) "interrupted"
                                            "the agent-shell isn't running anymore"))))))

(provide 'bndos-task-agent)
;;; bndos-task-agent.el ends here
