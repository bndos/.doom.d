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
