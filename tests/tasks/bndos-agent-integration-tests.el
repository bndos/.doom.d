;;; bndos-agent-integration-tests.el --- Real Hydra, agent-shell and worktrunk against a stub agent -*- lexical-binding: t; -*-

;; These drive bin/bndos-task, the installed agent-shell and worktrunk end to
;; end against tests/tasks/stub-acp-agent.py running behind a private Hydra
;; daemon (set up by run-tests.sh): starting a task, attaching to it, modes,
;; a permission request waiting for Emacs, a failed resume, a review round
;; trip and a leader's start / send / wait.  No model calls.

(require 'test-helper)
(require 'agent-shell)
(require 'bndos-task-review)
(require 'bndos-task-agent)
(require 'bndos-task-dashboard)
(require 'bndos-review-tests)

(defun bndos-test-hydra-p ()
  (and (getenv "BNDOS_TEST_HYDRA") (getenv "HYDRA_ACP_HOME") (bndos-test-wt-available-p)))

(defun bndos-test-stub-config ()
  "An agent-shell config for the stub; Hydra knows it as agent bndos-stub."
  (agent-shell-make-agent-config
   :identifier 'bndos-stub
   :mode-line-name "Stub" :buffer-name "Stub"
   :shell-prompt "Stub> " :shell-prompt-regexp "Stub> "
   :client-maker #'ignore))

(defmacro bndos-test-with-hydra (&rest body)
  "Run BODY with a private store, the stub as the only agent and the test daemon."
  (declare (indent 0))
  `(progn
     (skip-unless (bndos-test-hydra-p))
     (bndos-test-with-store
       (let ((agent-shell-agent-configs (list (bndos-test-stub-config)))
             (bndos-task-agent-default-config 'bndos-stub)
             (bndos-task-hydra-command (list (getenv "BNDOS_TEST_HYDRA")))
             (bndos-task-agent--shells (make-hash-table :test #'equal))
             (bndos-task-agent--hydra (make-hash-table :test #'equal))
             (agent-shell-show-welcome-message nil)
             (agent-shell-transcript-file-path-function nil)
             (agent-shell-file-completion-enabled nil)
             (agent-shell-permission-responder-function nil))
         ,@body))))

(defun bndos-test-kill-shell (buffer)
  (when (buffer-live-p buffer)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun bndos-test-stub-messages (method)
  "Params of every METHOD message the stub agents received, in order."
  (let ((log (getenv "BNDOS_TEST_STUB_LOG")))
    (when (file-exists-p log)
      (delq nil (mapcar (lambda (line)
                          (let ((msg (json-parse-string line :object-type 'plist)))
                            (when (equal (plist-get msg :method) method)
                              (plist-get msg :params))))
                        (split-string (with-temp-buffer (insert-file-contents log) (buffer-string)) "\n" t))))))

(defun bndos-test-stub-prompts ()
  (mapcar (lambda (params) (mapconcat (lambda (b) (or (plist-get b :text) "")) (plist-get params :prompt) ""))
          (bndos-test-stub-messages "session/prompt")))

(defun bndos-test-start (repo branch &rest keys)
  "Start BRANCH from main in REPO through `bndos-task-start'; return its result."
  (bndos-test-await (lambda (cb) (apply #'bndos-task-start repo branch "main" :callback cb keys)) 60))

(defun bndos-test-attach (key)
  (let (shell)
    (bndos-task-agent-ensure key (lambda (b) (setq shell b)))
    (bndos-proc-wait (lambda () shell) 30)
    (or shell (error "No shell attached for %s" key))))

(ert-deftest bndos-integration-start-runs-the-agent-in-its-mode-and-emacs-attaches ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      (let* ((result (bndos-test-start repo "w1" :prompt "build the thing" :mode "auto" :model "large"
                                       :ticket "gt-1"))
             (key (plist-get result :key))
             (wt (plist-get result :worktree)))
        (should key)
        (should (equal (bndos-test-git wt "config" "branch.w1.bndos-base") "main"))
        (bndos-proc-wait (lambda () (member "build the thing" (bndos-test-stub-prompts))) 20)
        (should (member "build the thing" (bndos-test-stub-prompts)))
        (let* ((record (bndos-task-store-load key))
               (sid (bndos-task-get record :agent :session-id)))
          (should (string-prefix-p "hydra_session_" sid))
          (should (equal (bndos-test-git wt "config" "branch.w1.bndos-session") sid))
          (should (equal (bndos-task-get record :agent :mode-actual) "auto"))
          (should (equal (plist-get record :ticket) "gt-1"))
          ;; Opening the task attaches to that same session, still in its mode.
          (let ((shell (bndos-test-attach key)))
            (unwind-protect
                (let ((agent (plist-get (bndos-task-store-load key) :agent)))
                  (should (equal (plist-get agent :resume) "resumed"))
                  (should (equal (plist-get agent :session-id) sid))
                  (should (equal (plist-get agent :mode-actual) "auto"))
                  (should (eq (bndos-task-agent-buffer (bndos-task-store-load key)) shell))
                  ;; Attaching didn't set the mode again.
                  (should (= (seq-count (lambda (p) (equal (plist-get p :modeId) "auto"))
                                        (bndos-test-stub-messages "session/set_mode"))
                             1)))
              (bndos-test-kill-shell shell)))
          ;; The dashboard sees it through `bndos-task status'.
          (let ((bndos-task-projects (list repo)))
            (pcase-let ((`(,rows ,problems) (bndos-test-await #'bndos-task--inventory 60)))
              (should-not problems)
              (let ((row (seq-find (lambda (r) (equal (plist-get r :key) key)) rows)))
                (should (equal (plist-get row :base) "main"))
                (should (memq (bndos-task-agent-lifecycle (plist-get row :record)) '(detached busy)))))))))))

(ert-deftest bndos-integration-agent-here-attaches-from-inside-the-worktree ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      ;; Started by a leader: Emacs has never seen this task.
      (should (eql (car (bndos-test-cli repo "start" "--base" "main" "--agent" "bndos-stub" "here")) 0))
      (let* ((wt (file-name-as-directory (file-truename (concat (directory-file-name repo) ".here"))))
             (sid (bndos-test-git repo "config" "branch.here.bndos-session"))
             (default-directory (expand-file-name "src/" wt)))
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
          (bndos/task-agent-here)
          (let (shell)
            (bndos-proc-wait (lambda ()
                               (setq shell (seq-find #'buffer-live-p (hash-table-values bndos-task-agent--shells)))
                               (and shell (equal (agent-shell-session-id :shell-buffer shell) sid)))
                             30)
            (unwind-protect
                (should (equal (agent-shell-session-id :shell-buffer shell) sid))
              (bndos-test-kill-shell shell))))))))

(ert-deftest bndos-integration-adopts-a-conversation-from-a-hand-made-worktree ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      ;; A worktree made by hand, with an agent conversation from before.
      (let* ((wt (bndos-test-worktree repo "old" "main"))
             (key (bndos-test-task-in wt))
             (listed (bndos-task-cli-call (list "sessions" wt "--agent" "bndos-stub") wt)))
        (should (equal (mapcar (lambda (s) (plist-get s :id)) (plist-get listed :sessions)) '("past-1" "past-2")))
        (should (equal (plist-get (car (plist-get listed :sessions)) :title) "earlier work"))
        (let* ((out (bndos-test-await (lambda (cb) (bndos-task-adopt key "bndos-stub" "past-1" :callback cb)) 30))
               (sid (plist-get out :session_id)))
          (should (equal (plist-get out :status) "adopted"))
          (should (equal (bndos-test-git wt "config" "branch.old.bndos-session") sid))
          (should (equal (bndos-task-get (bndos-task-store-load key) :agent :resume) "adopted"))
          ;; Hydra loaded that very conversation into the agent.
          (should (member "past-1" (mapcar (lambda (p) (plist-get p :sessionId))
                                           (bndos-test-stub-messages "session/load"))))
          ;; Emacs attaches to it like any task's session.
          (let ((shell (bndos-test-attach key)))
            (unwind-protect
                (progn
                  (should (equal (agent-shell-session-id :shell-buffer shell) sid))
                  (should (equal (bndos-task-get (bndos-task-store-load key) :agent :resume) "resumed")))
              (bndos-test-kill-shell shell)))
          ;; It isn't silently replaced.
          (let ((again (bndos-task-cli-call (list "adopt" wt "--agent" "bndos-stub" "--session" "past-2") wt)))
            (should (equal (plist-get again :status) "failed"))
            (should (string-match-p "already has session" (plist-get again :error)))))))))

(defun bndos-test-task-in (wt)
  (bndos-task-ensure-record (bndos-test-git wt "rev-parse" "--path-format=absolute" "--git-common-dir")
                            wt (bndos-test-git wt "branch" "--show-current")))

(defun bndos-test-ai-review (status &optional pick-model resume)
  "Run (or with RESUME, resume) the AI review here with the stub; wait for STATUS.
PICK-MODEL is the model to choose in the reviewer's picker; nil quits it.
The mocks stay in place while waiting: the picker opens once the session is up."
  (let (offered)
    (cl-letf (((symbol-function 'agent-shell-select-config)
               (cl-function (lambda (&rest _) (bndos-task-agent--hydra-config "bndos-stub"))))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) resume))
              ((symbol-function 'completing-read)
               (lambda (prompt choices &rest _)
                 (push (cons prompt choices) offered)
                 (or (and pick-model (seq-find (lambda (c) (string-match-p pick-model c)) choices))
                     (signal 'quit nil)))))
      (bndos/task-review-ai-review)
      (bndos-proc-wait (lambda () (equal (plist-get bndos-task-review--ai :status) status)) 30))
    (should (equal (plist-get bndos-task-review--ai :status) status))
    offered))

(defun bndos-test-review-task (repo branch &rest keys)
  "Start BRANCH (KEYS as for `bndos-task-start') with a commit to review.
Return (KEY WORKTREE REVIEW-BUFFER)."
  (let* ((result (apply #'bndos-test-start repo branch keys))
         (key (plist-get result :key))
         (wt (plist-get result :worktree)))
    (bndos-test-commit wt "work" "w.txt" "w\n")
    (let ((bndos-task-review-show-tree nil))
      (bndos-task-review-open key))
    (let ((review (bndos-task-review-buffer key)))
      (bndos-proc-wait (lambda () (with-current-buffer review (plist-get bndos-task-review--data :files))) 20)
      (list key wt review))))

(defun bndos-test-stub-methods ()
  "Methods of every message the stub agents received, in order."
  (let ((log (getenv "BNDOS_TEST_STUB_LOG")))
    (mapcar (lambda (line) (cons (plist-get (json-parse-string line :object-type 'plist) :method) line))
            (split-string (with-temp-buffer (insert-file-contents log) (buffer-string)) "\n" t))))

(ert-deftest bndos-integration-ai-review-model-comes-from-the-reviewers-session ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      (pcase-let ((`(,_key ,_wt ,review) (bndos-test-review-task repo "pick")))
        (with-current-buffer review
          (let* ((before (length (bndos-test-stub-methods)))  ; the log is shared by all tests
                 (offered (bndos-test-ai-review "done" "large"))
                 (choices (cdr (seq-find (lambda (o) (string-match-p "model" (car o))) offered)))
                 (methods (nthcdr before (bndos-test-stub-methods)))
                 (set-model (seq-position methods "large"
                                          (lambda (m model) (and (equal (car m) "session/set_model")
                                                                 (string-match-p model (cdr m))))))
                 (review-prompt (seq-position methods "hutch-review"
                                              (lambda (m text) (and (equal (car m) "session/prompt")
                                                                    (string-match-p text (cdr m)))))))
            ;; The session's own models, as agent-shell lists them.
            (should (seq-some (lambda (c) (string-match-p "small" c)) choices))
            (should (seq-some (lambda (c) (string-match-p "large" c)) choices))
            ;; Chosen before the review prompt went out.
            (should set-model)
            (should (< set-model review-prompt))
            (bndos-test-kill-shell (plist-get bndos-task-review--ai :buffer))))))))

(ert-deftest bndos-integration-ai-review-gets-instructions-and-the-tasks-brief ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      (pcase-let ((`(,_key ,_wt ,review) (bndos-test-review-task repo "brief" :prompt "Build the slug helper.")))
        (with-current-buffer review
          (let* ((bndos-task-review-ai-instructions "Use the review skill.")
                 (before (length (bndos-test-stub-methods))))
            (bndos-test-ai-review "done")
            (let ((prompt (cdr (seq-find (lambda (m) (and (equal (car m) "session/prompt")
                                                          (string-match-p "hutch-review" (cdr m))))
                                         (nthcdr before (bndos-test-stub-methods))))))
              (should (string-match-p "Use the review skill" prompt))
              (should (string-match-p "> Build the slug helper" prompt)))
            (bndos-test-kill-shell (plist-get bndos-task-review--ai :buffer))))))))

(ert-deftest bndos-integration-interrupted-ai-review-pauses-and-resumes ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      (pcase-let ((`(,key ,_wt ,review) (bndos-test-review-task repo "pause")))
        (with-current-buffer review
          ;; The reviewer's first turn ends interrupted (as C-c C-c in its shell does).
          (with-temp-file (getenv "BNDOS_TEST_STUB_CANCEL_ONCE"))
          (bndos-test-ai-review "paused")
          (should (string-match-p "cancelled" (plist-get bndos-task-review--ai :message)))
          (should (buffer-live-p (plist-get bndos-task-review--ai :buffer)))
          (should-not (plist-get (bndos-task-store-load key) :threads))
          (let ((reviewer (plist-get bndos-task-review--ai :buffer)))
            ;; C-c C-h resumes the same reviewer instead of starting another.
            (bndos-test-ai-review "done" nil t)
            (should (eq (plist-get bndos-task-review--ai :buffer) reviewer))
            (should (= (length (plist-get (bndos-task-store-load key) :threads)) 1))
            (bndos-test-kill-shell reviewer)))))))

(ert-deftest bndos-integration-models-are-saved-and-listed ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      (bndos-test-start repo "mdl")
      (let ((saved (cdr (bndos-test-cli repo "models" "--agent" "bndos-stub"))))
        (should (equal (plist-get saved :source) "saved"))
        (should (equal (mapcar (lambda (m) (plist-get m :id)) (plist-get saved :models)) '("small" "large")))
        (should (equal (plist-get saved :current_model) "small"))
        (should (member "auto" (mapcar (lambda (m) (plist-get m :id)) (plist-get saved :modes)))))
      ;; Asked again, straight from the agent's adapter.
      (let ((fresh (cdr (bndos-test-cli repo "models" "--agent" "bndos-stub" "--refresh"))))
        (should (equal (plist-get fresh :source) "agent"))
        (should (equal (length (plist-get fresh :models)) 2))))))

(ert-deftest bndos-integration-models-from-config-options-are-listed-and-set ()
  ;; Claude reports its models as a (grouped) "model" config option.
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      (let ((options (cdr (bndos-test-cli repo "models" "--agent" "bndos-stub-options"))))
        (should (equal (mapcar (lambda (m) (plist-get m :id)) (plist-get options :models)) '("small" "large")))
        (should (equal (plist-get options :current_model) "small")))
      (let ((before (length (bndos-test-stub-methods))))
        (bndos-test-start repo "cfg" :agent "bndos-stub-options" :model "large")
        (should (seq-some (lambda (m) (and (equal (car m) "session/set_config_option")
                                           (string-match-p "\"large\"" (cdr m))))
                          (nthcdr before (bndos-test-stub-methods))))))))

(ert-deftest bndos-integration-ai-findings-become-threads-the-worker-can-answer ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      (let* ((result (bndos-test-start repo "rv"))
             (key (plist-get result :key))
             (wt (plist-get result :worktree))
             (worker (bndos-task-get (bndos-task-store-load key) :agent :session-id)))
        (bndos-test-commit wt "work" "w.txt" "w\n" "v.txt" "v\n")
        (let ((bndos-task-review-show-tree nil))
          (bndos-task-review-open key))
        (let ((review (bndos-task-review-buffer key)))
          (bndos-proc-wait (lambda () (with-current-buffer review (plist-get bndos-task-review--data :files))) 20)
          (with-current-buffer review
            (bndos-test-ai-review "done"))
          (let* ((threads (plist-get (bndos-task-store-load key) :threads))
                 (finding (car threads)))
            ;; One thread per finding (the lgtm file adds none), by ai:AGENT, on the file.
            (should (= (length threads) 1))
            (should (equal (plist-get finding :author) "ai:bndos-stub"))
            (should (equal (plist-get finding :kind) "finding"))
            (should (equal (plist-get finding :title) "stub finding"))
            (should (equal (bndos-task-get finding :anchor :line) 1))
            (should (member (bndos-task-get finding :anchor :path) '("v.txt" "w.txt")))
            ;; It's shown in the diff like any thread and reached with ]t.
            (with-current-buffer review
              (goto-char (point-min))
              (bndos/task-review-next-thread)
              (should (equal (bndos-task-review--thread-at-point) (plist-get finding :id)))
              (should (string-match-p "stub finding" (buffer-substring (line-beginning-position) (line-end-position)))))
            ;; And it goes to the worker (not the reviewer) like your comments.
            (let ((snapshot (bndos-test-await (lambda (cb) (bndos-task-git-snapshot wt cb)))))
              (bndos-task-agent-submit key (list (plist-get finding :id)) "implement"
                                       :snapshot snapshot :base '(:ref "main")))
            (bndos-proc-wait (lambda () (equal (plist-get (bndos-task-thread (bndos-task-store-load key) (plist-get finding :id)) :state)
                                               "addressed"))
                             30)
            (should (equal (plist-get (bndos-task-thread (bndos-task-store-load key) (plist-get finding :id)) :state)
                           "addressed"))
            (should (equal (bndos-test-git wt "config" "branch.rv.bndos-session") worker)))
          ;; The reviewer was a separate conversation, still open.
          (let ((reviewer (seq-find (lambda (b) (and (not (memq b (hash-table-values bndos-task-agent--shells)))
                                                     (string-match-p "Stub" (buffer-name b))))
                                    (agent-shell-buffers))))
            (unwind-protect
                (progn
                  (should reviewer)
                  (should-not (equal (agent-shell-session-id :shell-buffer reviewer) worker)))
              (bndos-test-kill-shell reviewer)
              (bndos-test-kill-shell (bndos-task-agent-buffer (bndos-task-store-load key)))
              (kill-buffer review))))))))

(ert-deftest bndos-integration-a-task-opened-before-any-refresh-attaches-to-its-session ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      ;; A leader started it; Emacs only knows the worktree (as after
      ;; SPC g w r, without a dashboard refresh in between).
      (should (eql (car (bndos-test-cli repo "start" "--base" "main" "--agent" "bndos-stub" "led")) 0))
      (let* ((wt (file-name-as-directory (file-truename (concat (directory-file-name repo) ".led"))))
             (sid (bndos-test-git repo "config" "branch.led.bndos-session"))
             (key (bndos-test-task-in wt)))
        (should-not (bndos-task-get (bndos-task-store-load key) :agent :session-id))
        (let ((shell (bndos-test-attach key)))
          (unwind-protect
              (progn
                (should (equal (agent-shell-session-id :shell-buffer shell) sid))
                (should (equal (bndos-task-get (bndos-task-store-load key) :agent :session-id) sid))
                (should (equal (bndos-task-get (bndos-task-store-load key) :agent :provider) "bndos-stub")))
            (bndos-test-kill-shell shell)))))))

(ert-deftest bndos-integration-a-conversation-emacs-starts-is-recorded-in-git ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      ;; A hand-made worktree with no session anywhere: opening its agent
      ;; starts one, and leaders / the dashboard must see the same one.
      (let* ((wt (bndos-test-worktree repo "fresh" "main"))
             (key (bndos-test-task-in wt))
             (shell (bndos-test-attach key)))
        (unwind-protect
            (let ((sid (agent-shell-session-id :shell-buffer shell)))
              (should sid)
              (bndos-proc-wait (lambda () (equal (ignore-errors (bndos-test-git wt "config" "branch.fresh.bndos-session")) sid)) 10)
              (should (equal (bndos-test-git wt "config" "branch.fresh.bndos-session") sid))
              (should (equal (bndos-test-git wt "config" "branch.fresh.bndos-agent") "bndos-stub"))
              (should (equal (plist-get (cdr (bndos-test-cli wt "status" wt)) :session_id) sid)))
          (bndos-test-kill-shell shell))))))

(ert-deftest bndos-integration-branches-with-slashes-keep-their-session ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      (let ((started (cdr (bndos-test-cli repo "start" "--base" "main" "--agent" "bndos-stub" "feat/Slash.1"))))
        (should (equal (plist-get started :status) "started"))
        (let* ((wt (file-name-as-directory (plist-get started :worktree)))
               (status (cdr (bndos-test-cli repo "status" "feat/Slash.1")))
               (key (bndos-test-task-in wt)))
          (should (equal (plist-get status :session_id) (plist-get started :session_id)))
          (should (equal (plist-get status :parent) "main"))
          (bndos-task-agent-sync-from-git key)
          (should (equal (bndos-task-get (bndos-task-store-load key) :agent :session-id)
                         (plist-get started :session_id))))))))

(ert-deftest bndos-integration-refused-mode-is-reported-not-assumed ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      (let ((warned nil))
        (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) (push args warned))))
          (let* ((result (bndos-test-start repo "m2" :prompt "go yolo" :mode "yolo"))
                 (agent (plist-get (bndos-task-store-load (plist-get result :key)) :agent)))
            (should (equal (plist-get agent :mode) "yolo"))
            (should (equal (plist-get agent :mode-actual) "default"))
            (should (string-match-p "mode yolo refused" (format "%s" warned)))
            ;; The agent still got its prompt.
            (bndos-proc-wait (lambda () (member "go yolo" (bndos-test-stub-prompts))) 20)
            (should (member "go yolo" (bndos-test-stub-prompts)))))))))

(ert-deftest bndos-integration-permission-request-waits-for-emacs ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      (let* ((result (bndos-test-start repo "p1" :prompt "PERMIT write stub.txt"))
             (key (plist-get result :key))
             (wt (plist-get result :worktree)))
        ;; Nobody else is attached: the request stays open instead of being refused.
        (let ((status (bndos-test-cli wt "status" "--wait" "waiting" "--timeout" "20" "p1")))
          (should (eql (car status) 0))
          (should (eql (plist-get (cdr status) :attached_clients) 1)))
        (let* ((asked nil)
               (agent-shell-permission-responder-function
                (lambda (permission)
                  (setq asked (map-elt (map-elt permission :tool-call) :title))
                  (agent-shell-permission-allow-always permission)))
               (shell (bndos-test-attach key)))
          (unwind-protect
              (progn
                (bndos-proc-wait (lambda () asked) 20)
                (should (equal asked "Write stub.txt"))
                (let ((done (bndos-test-cli wt "status" "--wait" "done" "--timeout" "20" "p1")))
                  (should (eql (car done) 0))
                  (should (eq (plist-get (cdr done) :done) t))))
            (bndos-test-kill-shell shell)))))))

(ert-deftest bndos-integration-failed-resume-is-labelled-new ()
  (bndos-test-with-hydra
    (let* ((wt (file-name-as-directory (file-truename (make-temp-file "wt-" t))))
           (sid "hydra_session_doesnotexist0000"))
      (bndos-task-store-save (list :key "kn" :worktree (list :path wt)
                                   :agent (list :provider "bndos-stub" :session-id sid)))
      (cl-letf (((symbol-function 'display-warning) #'ignore))
        (let ((shell (bndos-test-attach "kn")))
          (unwind-protect
              (let ((agent (plist-get (bndos-task-store-load "kn") :agent)))
                (should (equal (plist-get agent :resume) "new-conversation"))
                (should (plist-get agent :session-id))
                (should-not (equal (plist-get agent :session-id) sid))
                (should (equal (plist-get agent :previous-session-ids) (list sid))))
            (bndos-test-kill-shell shell)
            (delete-directory wt t)))))))

(ert-deftest bndos-integration-review-round-trip-attaches-to-the-session ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      ;; No prompt: the session exists but nothing is attached.
      (let* ((result (bndos-test-start repo "it"))
             (key (plist-get result :key))
             (wt (plist-get result :worktree)))
        (bndos-test-commit wt "work" "w.txt" "w\n")
        (let* ((snapshot (bndos-test-await (lambda (cb) (bndos-task-git-snapshot wt cb))))
               (thread (bndos-task-make-thread :kind "comment" :author "me" :body "rename this"
                                               :anchor (list :path "w.txt" :side "new" :line 1 :text "w"))))
          (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r thread)))
          (bndos-task-agent-submit key (list (plist-get thread :id)) "implement"
                                   :snapshot snapshot :base '(:ref "main"))
          (bndos-proc-wait (lambda ()
                             (equal (plist-get (bndos-task-thread (bndos-task-store-load key) (plist-get thread :id))
                                               :state)
                                    "addressed"))
                           30)
          (let* ((record (bndos-task-store-load key))
                 (th (bndos-task-thread record (plist-get thread :id))))
            (unwind-protect
                (progn
                  (should (equal (bndos-task-get record :agent :resume) "resumed"))
                  (should (equal (plist-get th :state) "addressed"))
                  (should (equal (plist-get (car (last (plist-get th :replies))) :body)
                                 (format "stub reply to %s" (plist-get thread :id))))
                  (should (equal (plist-get (car (plist-get record :requests)) :status) "answered")))
              (bndos-test-kill-shell (bndos-task-agent-buffer record)))))))))

(ert-deftest bndos-integration-leader-starts-sends-and-waits-without-emacs ()
  (bndos-test-with-hydra
    (bndos-test-with-repo repo
      ;; Everything a leader agent does, from the command line.
      (let ((started (bndos-test-cli repo "start" "--base" "main" "--agent" "bndos-stub"
                                     "--prompt" "leader task one" "layer-a")))
        (should (eql (car started) 0))
        (should (equal (plist-get (cdr started) :status) "started"))
        (should (equal (plist-get (cdr started) :mode) "default"))
        (let ((done (bndos-test-cli repo "status" "--wait" "done" "--timeout" "30" "layer-a")))
          (should (eql (car done) 0))
          (should (equal (plist-get (cdr done) :state) "idle")))
        ;; A stacked layer on top of it.
        (should (eql (car (bndos-test-cli repo "start" "--base" "layer-a" "--agent" "bndos-stub" "layer-b")) 0))
        (should (equal (plist-get (cdr (bndos-test-cli repo "status" "layer-b")) :parent) "layer-a"))
        (let ((sent (bndos-test-cli repo "send" "layer-a" "--prompt" "leader follow-up" "--wait")))
          (should (eql (car sent) 0))
          (should (equal (plist-get (cdr sent) :stop_reason) "end_turn")))
        (should (member "leader follow-up" (bndos-test-stub-prompts)))
        (should (= (length (cdr (bndos-test-cli repo "status"))) 2))))))

(provide 'bndos-agent-integration-tests)
;;; bndos-agent-integration-tests.el ends here
