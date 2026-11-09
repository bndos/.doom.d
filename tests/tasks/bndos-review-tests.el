;;; bndos-review-tests.el --- Review buffers, agent routing, dashboard, the CLI -*- lexical-binding: t; -*-

(require 'test-helper)
(require 'bndos-task-review)
(require 'bndos-task-agent)
(require 'bndos-task-dashboard)

(defun bndos-test-task (repo branch base)
  "Create a worktree task and its record; return (KEY . WORKTREE)."
  (let* ((wt (bndos-test-worktree repo branch base))
         (common (bndos-test-git wt "rev-parse" "--path-format=absolute" "--git-common-dir"))
         (key (bndos-task-key common wt)))
    (bndos-task-store-ensure :key key :repo common :worktree wt :branch branch
                             :project "repo" :handle branch)
    (bndos-task-store-update key (lambda (r) (bndos-task-put r '(:repo :main-path) repo)))
    (cons key wt)))

(defun bndos-test-open-review (key)
  (let ((buffer (bndos-task-review-open key :select nil)))
    (bndos-proc-wait (lambda () (with-current-buffer buffer
                                  (and (plist-get bndos-task-review--data :refreshed)
                                       (not bndos-task-review--processes))))
                     20)
    buffer))

(defun bndos-test-goto-line-text (buffer text)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (search-forward text nil t))
        (when (get-text-property (line-beginning-position) 'bndos-task-line)
          (setq found t)))
      (should found)
      (beginning-of-line))))


;;; Review buffers

(ert-deftest bndos-review-multiple-reviews-are-isolated ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((a (bndos-test-task repo "alpha" "main"))
             (b (bndos-test-task repo "beta" "main")))
        (bndos-test-commit (cdr a) "a" "alpha.txt" "alpha\n")
        (bndos-test-commit (cdr b) "b" "beta.txt" "beta\n")
        (let ((ba (bndos-test-open-review (car a)))
              (bb (bndos-test-open-review (car b))))
          (unwind-protect
              (progn
                (should-not (eq ba bb))
                (should (equal (bndos-task-review-buffer (car a)) ba))
                (with-current-buffer ba
                  (should (equal (mapcar (lambda (f) (plist-get f :path)) (plist-get bndos-task-review--data :files))
                                 '("alpha.txt")))
                  (should (string-match-p "main (recorded parent)" (buffer-string))))
                (with-current-buffer bb
                  (should (equal (mapcar (lambda (f) (plist-get f :path)) (plist-get bndos-task-review--data :files))
                                 '("beta.txt"))))
                ;; A comment in one review never shows up in the other.
                (bndos-test-goto-line-text ba "+alpha")
                (with-current-buffer ba
                  (let ((anchor (bndos-task-review-anchor-at-point)))
                    (should (equal (plist-get anchor :path) "alpha.txt"))
                    (should (equal (plist-get anchor :side) "new"))
                    (should (equal (plist-get anchor :line) 1))
                    (bndos-task-store-update (car a) (lambda (r) (bndos-task-add-thread
                                                                  r (bndos-task-make-thread :kind "comment" :author "me"
                                                                                            :body "why?" :anchor anchor))))))
                (should (= 1 (length (plist-get (bndos-task-store-load (car a)) :threads))))
                (should (null (plist-get (bndos-task-store-load (car b)) :threads)))
                ;; Stale callbacks: a refresh started then superseded doesn't clobber.
                (with-current-buffer ba
                  (let ((gen bndos-task-review--generation))
                    (bndos-task-review-refresh)
                    (bndos-task-review-refresh)
                    (should (= bndos-task-review--generation (+ gen 2))))))
            (kill-buffer ba) (kill-buffer bb)))))))

(ert-deftest bndos-review-comments-persist-and-go-stale-when-code-moves ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((task (bndos-test-task repo "gamma" "main"))
             (key (car task)) (wt (cdr task)))
        (bndos-test-commit wt "g" "g.txt" "a\nb\ntarget\nc\n")
        (let ((buffer (bndos-test-open-review key)))
          (unwind-protect
              (progn
                (bndos-test-goto-line-text buffer "+target")
                (with-current-buffer buffer
                  (let ((anchor (bndos-task-review-anchor-at-point)))
                    (should (equal (plist-get anchor :before) '("a" "b")))
                    (should (equal (plist-get anchor :after) '("c")))
                    (bndos-task-store-update key (lambda (r) (bndos-task-add-thread
                                                              r (bndos-task-make-thread :kind "comment" :author "me"
                                                                                        :body "check" :anchor anchor))))))
                ;; Reload from disk: the discussion survives a restart.
                (clrhash bndos-task-store--cache)
                (should (equal (plist-get (car (plist-get (bndos-task-store-load key) :threads)) :body) "check"))
                ;; Code moves down two lines: anchor follows.
                (bndos-test-write wt "g.txt" "new1\nnew2\na\nb\ntarget\nc\n")
                (with-current-buffer buffer (bndos-task-review-refresh))
                (bndos-test-open-review key)
                (let ((thread (car (plist-get (bndos-task-store-load key) :threads))))
                  (should (equal (bndos-task-get thread :anchor :line) 5))
                  (should-not (plist-get thread :stale)))
                ;; The line disappears: thread kept, marked stale.
                (bndos-test-write wt "g.txt" "new1\nnew2\na\nb\nc\n")
                (bndos-test-open-review key)
                (let ((thread (car (plist-get (bndos-task-store-load key) :threads))))
                  (should (equal (plist-get thread :body) "check"))
                  (should (equal (bndos-task-get thread :stale :reason) "line no longer present")))
                (with-current-buffer buffer
                  (should (string-match-p "\\[stale\\]" (buffer-string)))))
            (kill-buffer buffer)))))))

(ert-deftest bndos-review-iteration-scope-shows-only-the-latest-revision ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((task (bndos-test-task repo "delta" "main"))
             (key (car task)) (wt (cdr task)))
        (bndos-test-commit wt "first" "one.txt" "1\n")
        (let ((buffer (bndos-test-open-review key)))
          (unwind-protect
              (progn
                (should (= 1 (length (plist-get (bndos-task-store-load key) :iterations))))
                (bndos-test-write wt "two.txt" "2\n")
                (with-current-buffer buffer
                  (setq bndos-task-review-scope 'iteration))
                (bndos-test-open-review key)
                (with-current-buffer buffer
                  (should (equal (mapcar (lambda (f) (plist-get f :path)) (plist-get bndos-task-review--data :files))
                                 '("two.txt")))
                  (setq bndos-task-review-scope 'full))
                (bndos-test-open-review key)
                (with-current-buffer buffer
                  (should (equal (mapcar (lambda (f) (plist-get f :path)) (plist-get bndos-task-review--data :files))
                                 '("one.txt" "two.txt")))))
            (kill-buffer buffer)))))))


;;; Responses

(defun bndos-test-request-record ()
  (list :key "task1"
        :threads (list (list :id "c-1" :state "sent" :replies nil)
                       (list :id "c-2" :state "sent" :replies nil))
        :requests (list (list :id "r-1" :mode "implement" :comments '("c-1" "c-2")
                              :snapshot "tree1" :status "in-progress"))))

(defun bndos-test-response (&rest overrides)
  (let ((base (list :protocol "bndos-review/1" :task "task1" :request "r-1" :snapshot "tree1"
                    :responses (list (list :comment "c-1" :status "addressed" :body "done"
                                           :files '("a.el") :tests '("make test"))))))
    (while overrides (setq base (plist-put base (pop overrides) (pop overrides))))
    base))

(ert-deftest bndos-agent-response-validation ()
  (let* ((record (bndos-test-request-record))
         (request (bndos-task-request record "r-1")))
    (should (plist-get (bndos-task-agent-validate-response (bndos-test-response) record request) :ok))
    (dolist (bad `((:task "other") (:request "r-9") (:snapshot "tree2") (:protocol "v0")
                   (:responses ((:comment "c-9" :status "addressed" :body "x")))
                   (:responses ((:comment "c-1" :status "answered" :body "x")
                                (:comment "c-1" :status "answered" :body "y")))
                   (:responses ((:comment "c-1" :status "fixed!" :body "x")))
                   (:responses ((:comment "c-1" :status "answered")))
                   (:responses ((:comment "c-1" :status "answered" :body "x" :files (1))))))
      (should (plist-get (bndos-task-agent-validate-response (apply #'bndos-test-response bad) record request)
                         :error)))
    ;; A discussion-only request can't claim code was changed.
    (let ((explain (plist-put (copy-sequence request) :mode "explain")))
      (should (plist-get (bndos-task-agent-validate-response (bndos-test-response) record explain) :error)))))

(ert-deftest bndos-agent-response-blocks-are-found-and-applied ()
  (let* ((text (concat "I fixed it.\n\n```bndos-review\n"
                       (json-serialize (bndos-task-store--to-json (bndos-test-response)))
                       "\n```\n"))
         (blocks (bndos-task-agent-extract-blocks text)))
    (should (= 1 (length blocks)))
    (let* ((record (bndos-test-request-record))
           (request (bndos-task-request record "r-1"))
           (ok (plist-get (bndos-task-agent-validate-response (bndos-task-store-decode (car blocks)) record request) :ok))
           (applied (bndos-task-agent-apply-response record request ok)))
      (should (equal (plist-get (bndos-task-thread applied "c-1") :state) "addressed"))
      ;; Not mentioned: reopened with a note, and the request is partial.
      (should (equal (plist-get (bndos-task-thread applied "c-2") :state) "open"))
      (should (equal (plist-get (bndos-task-request applied "r-1") :status) "partial"))
      (let ((reply (car (plist-get (bndos-task-thread applied "c-1") :replies))))
        (should (equal (plist-get reply :files) '("a.el")))
        (should (equal (plist-get reply :request) "r-1")))
      ;; Delegated: an addressed report resolves.
      (let ((applied (bndos-task-agent-apply-response record (plist-put (copy-sequence request) :delegate t) ok)))
        (should (equal (plist-get (bndos-task-thread applied "c-1") :state) "resolved"))))))


;;; Routing to the exact agent

(defmacro bndos-test-with-fake-shells (spec &rest body)
  "SPEC is ((KEY BUFFER-VAR STATUS SESSION) ...).  Stub agent-shell's API."
  (declare (indent 1))
  `(let ((bndos-task-agent--shells (make-hash-table :test #'equal))
         (inserted nil))
     (let ,(mapcar (lambda (s) `(,(nth 1 s) (generate-new-buffer " fake-shell"))) spec)
       (unwind-protect
           (cl-letf* (((symbol-function 'agent-shell-status)
                       (cl-function (lambda (&key shell-buffer)
                                      (buffer-local-value 'bndos-test--status shell-buffer))))
                      ((symbol-function 'agent-shell-session-id)
                       (cl-function (lambda (&key shell-buffer)
                                      (buffer-local-value 'bndos-test--session shell-buffer))))
                      ((symbol-function 'agent-shell-insert)
                       (cl-function (lambda (&key text shell-buffer &allow-other-keys)
                                      (push (cons shell-buffer text) inserted)))))
             ,@(mapcar (lambda (s)
                         `(with-current-buffer ,(nth 1 s)
                            (setq-local bndos-test--status ,(nth 2 s))
                            (setq-local bndos-test--session ,(nth 3 s))
                            (setq-local bndos-task-agent--watch (list :key ,(nth 0 s)))
                            (puthash ,(nth 0 s) ,(nth 1 s) bndos-task-agent--shells)))
                       spec)
             ,@body)
         ,@(mapcar (lambda (s) `(kill-buffer ,(nth 1 s))) spec)))))

(defvar-local bndos-test--status nil)
(defvar-local bndos-test--session nil)

(defun bndos-test-add-thread (key body)
  (let ((thread (bndos-task-make-thread :kind "comment" :author "me" :body body)))
    (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r thread)))
    (plist-get thread :id)))

(ert-deftest bndos-agent-routes-to-the-tasks-own-shell-and-refuses-unsafe-sends ()
  (bndos-test-with-store
    (dolist (key '("k-a" "k-b" "k-c"))
      (bndos-task-store-save (list :key key :worktree (list :path temporary-file-directory)
                                   :agent (list :provider "claude-code"))))
    (bndos-test-with-fake-shells (("k-a" shell-a 'ready "sa") ("k-b" shell-b 'ready "sb") ("k-c" shell-c 'busy "sc"))
      (let* ((ca (bndos-test-add-thread "k-a" "for a"))
             (cb (bndos-test-add-thread "k-b" "for b"))
             (cc (bndos-test-add-thread "k-c" "for c"))
             (snapshot '(:tree "t1" :head "h1")))
        ;; Most-recently-used shell is irrelevant: each task's own buffer gets it.
        (bndos-task-agent-submit "k-b" (list cb) "explain" :snapshot snapshot :base '(:ref "main"))
        (bndos-task-agent-submit "k-a" (list ca) "implement" :snapshot snapshot :base '(:ref "main"))
        (should (equal (mapcar #'car inserted) (list shell-a shell-b)))
        (should (string-search ca (cdr (assq shell-a inserted))))
        (should-not (string-search cb (cdr (assq shell-a inserted))))
        (should (string-search "DISCUSS ONLY" (cdr (assq shell-b inserted))))
        (should (string-search "Mode: IMPLEMENT" (cdr (assq shell-a inserted))))
        ;; Busy shell, in-flight request, already-sent thread: all refused.
        (should-error (bndos-task-agent-submit "k-c" (list cc) "explain" :snapshot snapshot) :type 'user-error)
        (should-error (bndos-task-agent-submit "k-a" (list ca) "explain" :snapshot snapshot) :type 'user-error)
        (should (equal (plist-get (bndos-task-thread (bndos-task-store-load "k-a") ca) :state) "sent"))
        ;; The turn: input echo, streamed chunks, completion.
        (let* ((request (bndos-task-in-flight-request (bndos-task-store-load "k-a")))
               (rid (plist-get request :id))
               (answer (bndos-task-store--to-json
                        (list :protocol "bndos-review/1" :task "k-a" :request rid :snapshot "t1"
                              :responses (list (list :comment ca :status "addressed" :body "fixed"))))))
          (cl-letf (((symbol-function 'bndos-task-agent--after-request) #'ignore))
            (bndos-task-agent--on-event shell-a `((:event . input-submitted)
                                                  (:data . ((:prompt . ,(cdr (assq shell-a inserted)))))))
            (should (equal (plist-get (bndos-task-request (bndos-task-store-load "k-a") rid) :status) "in-progress"))
            (bndos-task-agent--on-event shell-a '((:event . agent-message-chunk) (:data . ((:text-chunk . "Done.\n```bndos-")))))
            (bndos-task-agent--on-event shell-a `((:event . agent-message-chunk)
                                                  (:data . ((:text-chunk . ,(concat "review\n" (json-serialize answer) "\n```"))))))
            (bndos-task-agent--on-event shell-a '((:event . turn-complete) (:data . ((:stop-reason . "end_turn"))))))
          (should (equal (plist-get (bndos-task-thread (bndos-task-store-load "k-a") ca) :state) "addressed"))
          (should (equal (plist-get (bndos-task-request (bndos-task-store-load "k-a") rid) :status) "answered"))
          ;; Task b's thread is untouched by a's answer.
          (should (equal (plist-get (bndos-task-thread (bndos-task-store-load "k-b") cb) :state) "sent")))
        ;; A cancelled turn reopens the threads, it doesn't resolve them.
        (let ((rid (plist-get (bndos-task-in-flight-request (bndos-task-store-load "k-b")) :id)))
          (bndos-task-agent--on-event shell-b `((:event . input-submitted)
                                                (:data . ((:prompt . ,(cdr (assq shell-b inserted)))))))
          (bndos-task-agent--on-event shell-b '((:event . turn-complete) (:data . ((:stop-reason . "cancelled")))))
          (should (equal (plist-get (bndos-task-request (bndos-task-store-load "k-b") rid) :status) "cancelled"))
          (should (equal (plist-get (bndos-task-thread (bndos-task-store-load "k-b") cb) :state) "open")))))))

(ert-deftest bndos-agent-prose-without-a-block-changes-nothing ()
  (bndos-test-with-store
    (bndos-task-store-save (list :key "k" :worktree (list :path temporary-file-directory)
                                 :agent (list :provider "claude-code")))
    (bndos-test-with-fake-shells (("k" shell 'ready "s"))
      (let ((id (bndos-test-add-thread "k" "please fix")))
        (bndos-task-agent-submit "k" (list id) "implement" :snapshot '(:tree "t") :base nil)
        (cl-letf (((symbol-function 'bndos-task-agent--after-request) #'ignore))
          (bndos-task-agent--on-event shell `((:event . input-submitted) (:data . ((:prompt . ,(cdar inserted))))))
          (bndos-task-agent--on-event shell '((:event . agent-message-chunk) (:data . ((:text-chunk . "All fixed, trust me!")))))
          (bndos-task-agent--on-event shell '((:event . turn-complete) (:data . ((:stop-reason . "end_turn"))))))
        (let ((record (bndos-task-store-load "k")))
          (should (equal (plist-get (bndos-task-thread record id) :state) "open"))
          (should (equal (plist-get (car (plist-get record :requests)) :status) "no-response"))
          (should (string-search "trust me" (plist-get (car (plist-get record :requests)) :raw))))))))

;;; Session verification

(ert-deftest bndos-agent-init-labels-a-failed-resume-as-new ()
  (bndos-test-with-store
    (bndos-task-store-save (list :key "kv" :worktree (list :path temporary-file-directory)
                                 :agent (list :provider "claude-code" :session-id "old-session")))
    (let ((buffer (generate-new-buffer " shell"))
          (ready nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-session-id) (cl-function (lambda (&key _shell-buffer) "other-session")))
                    ((symbol-function 'display-warning) #'ignore))
            (with-current-buffer buffer
              (setq-local bndos-task-agent--watch
                          (list :key "kv" :expect "old-session" :on-ready (list (lambda (b) (setq ready b))))))
            (with-current-buffer buffer (bndos-task-agent--on-init "kv" buffer))
            (let ((agent (plist-get (bndos-task-store-load "kv") :agent)))
              (should (equal (plist-get agent :resume) "new-conversation"))
              (should (equal (plist-get agent :session-id) "other-session"))
              (should (equal (plist-get agent :previous-session-ids) '("old-session")))
              (should (eq ready buffer))
              (should (string-match-p "NEW conversation" (bndos-task-agent-summary (bndos-task-store-load "kv"))))))
        (kill-buffer buffer)))))


;;; Dashboard

(ert-deftest bndos-dashboard-rows-include-plain-and-missing-worktrees ()
  (skip-unless (bndos-test-wt-available-p))
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((made (bndos-test-await (lambda (cb) (bndos-task-wt-create repo "eps" "main" cb))))
             (bndos-task-projects (list repo))
             (gone-key "gone-task"))
        (bndos-task-store-save (list :key gone-key :worktree (list :path "/nonexistent/wt" :handle "zeta")
                                     :repo (list :project "repo")
                                     :threads (list (bndos-task-make-thread :kind "comment" :author "me"
                                                                            :body "keep me"))))
        (bndos-test-write (plist-get made :path) "x.txt" "x\n")
        (pcase-let ((`(,rows ,problems) (bndos-test-await #'bndos-task--inventory)))
          (should-not problems)
          (let ((eps (seq-find (lambda (r) (equal (bndos-task-get r :record :branch) "eps")) rows))
                (gone (seq-find (lambda (r) (equal (plist-get r :key) gone-key)) rows)))
            (should eps)
            (should (equal (bndos-task--tree-label eps) "U"))
            (should (equal (bndos-task-get eps :record :repo :main-path) repo))
            (should (plist-get gone :missing))
            (should-not (seq-find (lambda (r) (equal (bndos-task-get r :worktree :path) repo)) rows))))))))

;;; Review navigation: the tree, keeping your place, M-RET

(defmacro bndos-test-with-open-review (spec &rest body)
  "SPEC is (REVIEW WORKTREE): a task review over four changed files, shown."
  (declare (indent 1))
  `(bndos-test-with-store
     (bndos-test-with-repo repo
       (let* ((,(nth 1 spec) (bndos-test-worktree repo "nav" "main"))
              (key (bndos-task-ensure-record
                    (bndos-test-git ,(nth 1 spec) "rev-parse" "--path-format=absolute" "--git-common-dir")
                    ,(nth 1 spec) "nav"))
              (bndos-task-review-show-tree nil))
         (dolist (f '("a.txt" "b.txt" "c.txt" "d/e.txt"))
           (bndos-test-write ,(nth 1 spec) f (mapconcat (lambda (n) (format "%s line %d" f n)) (number-sequence 1 40) "\n")))
         (bndos-test-commit ,(nth 1 spec) "four files" "z.txt" "z\n")
         (bndos-task-review-open key)
         (let ((,(car spec) (bndos-task-review-buffer key)))
           (bndos-proc-wait (lambda () (with-current-buffer ,(car spec) (plist-get bndos-task-review--data :files))) 20)
           (switch-to-buffer ,(car spec))
           (delete-other-windows)
           (unwind-protect (progn ,@body)
             (with-current-buffer ,(car spec) (magit-treediff-follow-close (current-buffer)))
             (kill-buffer ,(car spec))))))))

(defun bndos-test-goto-diff-line (text)
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(ert-deftest bndos-review-tree-window-follows-the-review ()
  (skip-unless (locate-library "treemacs"))
  (let ((magit-treediff-tree-backend 'treemacs))
    (bndos-test-with-open-review (review _wt)
      (bndos-task-review--update-tree t)
      (let ((tree-window (seq-find (lambda (w) (not (eq (window-buffer w) review))) (window-list))))
        (should tree-window)
        (dolist (file '("b.txt" "d/e.txt" "a.txt" "z.txt"))
          (with-current-buffer review
            (bndos-task-review--goto-file file)
            (magit-treediff-follow-sync))
          ;; The tree's window, not just its buffer, is on the file.
          (should (equal (with-current-buffer (window-buffer tree-window)
                           (save-excursion (goto-char (window-point tree-window))
                                           (magit-treediff--tree-file-at-point)))
                         file)))))))

(ert-deftest bndos-review-rerender-keeps-the-line-and-column ()
  (bndos-test-with-open-review (review _wt)
    (bndos-test-goto-diff-line "+c.txt line 17")
    (forward-char 6)
    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (column (current-column)))
      (bndos-task-review-render)
      (should (equal (buffer-substring-no-properties (line-beginning-position) (line-end-position)) line))
      (should (= (current-column) column)))))

(ert-deftest bndos-review-files-reopen-where-you-left-them ()
  (bndos-test-with-open-review (review _wt)
    (bndos-test-goto-diff-line "+b.txt line 23")
    (bndos-task-review--remember-position)
    (bndos-task-review--goto-file "d/e.txt")
    (should (equal (bndos-task-review--file-at-point) "d/e.txt"))
    (bndos-task-review--goto-file "b.txt")
    (should (string-match-p "b\\.txt line 23" (buffer-substring (line-beginning-position) (line-end-position))))
    ;; A file never visited opens at its top.
    (bndos-task-review--goto-file "c.txt")
    (should (eq (oref (magit-current-section) type) 'bndos-task-file))))

(ert-deftest bndos-review-m-ret-visits-the-worktree-line-in-another-window ()
  (bndos-test-with-open-review (review wt)
    (bndos-test-goto-diff-line "+a.txt line 12")
    (bndos/task-review-visit-worktree-other-window)
    (should (equal (buffer-file-name) (expand-file-name "a.txt" wt)))
    (should (= (line-number-at-pos) 12))
    (should (eq (window-buffer (next-window)) review))))

(ert-deftest bndos-review-threads-render-as-blocks-and-are-jumpable ()
  (bndos-test-with-open-review (review wt)
    (let* ((anchor (bndos-task-review--line-anchor wt "b.txt" 5 nil))
           (mine (bndos-task-make-thread :kind "comment" :author "me"
                                         :body "A long comment that should wrap nicely across lines instead of running off the right edge of the window forever and ever."
                                         :anchor anchor))
           (ai (plist-put (plist-put (bndos-task-make-thread :kind "finding" :author "ai:codex"
                                                              :body "Off by one\n\nThe loop stops early."
                                                              :anchor (bndos-task-review--line-anchor wt "c.txt" 3 nil))
                                     :title "Off by one")
                          :suggestion '(:old "c.txt line 3" :new "c.txt line three")))
           (done (plist-put (bndos-task-make-thread :kind "comment" :author "me" :body "old news" :anchor anchor)
                            :state "resolved")))
      (bndos-task-store-update key (lambda (r) (seq-reduce #'bndos-task-add-thread (list mine ai done) r)))
      (bndos-task-review-render)
      ;; Inline under their lines, framed, wrapped, with the suggestion.
      (goto-char (point-min))
      (bndos/task-review-next-thread)
      (should (equal (bndos-task-review--thread-at-point) (plist-get mine :id)))
      (should (looking-at-p " *╭ ● open  me"))
      (let ((block (buffer-substring-no-properties
                    (point) (next-single-property-change (point) 'bndos-task-thread))))
        (should (seq-every-p (lambda (l) (<= (string-width l) 120)) (split-string block "\n")))
        (should (string-match-p "instead of running\n *│ off the right edge" block)))
      (bndos/task-review-next-thread)
      (should (equal (bndos-task-review--thread-at-point) (plist-get ai :id)))
      (let ((block (buffer-substring-no-properties
                    (point) (next-single-property-change (point) 'bndos-task-thread))))
        (should (string-match-p "ai:codex · finding.*Off by one" block))
        (should (string-match-p "^ *│ -c.txt line 3$" block))
        (should (string-match-p "^ *│ \\+c.txt line three$" block)))
      ;; Resolved threads aren't inline, but the list reaches them.
      (should-not (save-excursion (bndos/task-review-next-thread)
                                  (equal (bndos-task-review--thread-at-point) (plist-get done :id))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (let ((all (all-completions "" table)))
                     ;; Open ones first, resolved last.
                     (should (string-match-p "^○ resolved" (car (last all))))
                     (car (last all))))))
        (bndos/task-review-jump-to-thread))
      (should (equal (bndos-task-review--thread-at-point) (plist-get done :id))))))

(ert-deftest bndos-review-suggestions-apply-to-the-worktree ()
  (bndos-test-with-open-review (review wt)
    (let ((ai (plist-put (bndos-task-make-thread :kind "finding" :author "ai:codex" :body "fix"
                                                 :anchor (bndos-task-review--line-anchor wt "a.txt" 2 nil))
                         :suggestion '(:old "a.txt line 2\n" :new "a.txt line two\n"))))
      (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r ai)))
      (bndos-task-review-render)
      (bndos-task-review--goto-thread (plist-get ai :id))
      (bndos/task-review-apply-suggestion)
      (should (string-match-p "^a.txt line two$" (with-temp-buffer (insert-file-contents (expand-file-name "a.txt" wt))
                                                                   (buffer-string))))
      (should (equal (plist-get (bndos-task-thread (bndos-task-store-load key) (plist-get ai :id)) :state) "resolved"))
      ;; A suggestion that no longer matches is refused, not guessed at.
      (let ((stale (plist-put (bndos-task-make-thread :kind "finding" :author "ai:codex" :body "x"
                                                      :anchor (list :path "a.txt"))
                              :suggestion '(:old "not in the file" :new "y"))))
        (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r stale)))
        (bndos-task-review-render)
        (bndos-task-review--goto-thread (plist-get stale :id))
        (should-error (bndos/task-review-apply-suggestion) :type 'user-error)))))

(ert-deftest bndos-review-code-is-coloured-by-its-mode ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((wt (bndos-test-worktree repo "col" "main"))
             (key (bndos-task-ensure-record (bndos-test-git wt "rev-parse" "--path-format=absolute" "--git-common-dir") wt "col"))
             (bndos-task-review-show-tree nil))
        (bndos-test-write wt "x.el" "(defun my-fn ()\n  \"Doc.\"\n  (let ((a 1)) a))\n")
        (bndos-test-commit wt "elisp" "y.txt" "y\n")
        (bndos-task-review-open key)
        (let ((review (bndos-task-review-buffer key)))
          (bndos-proc-wait (lambda () (with-current-buffer review (plist-get bndos-task-review--data :files))) 20)
          (with-current-buffer review
            (bndos-test-goto-diff-line "+(defun my-fn")
            (forward-char 2)
            (let ((faces (ensure-list (get-text-property (point) 'font-lock-face))))
              ;; The keyword's own colour, with the added-line face behind it.
              (should (memq 'font-lock-keyword-face faces))
              (should (eq (car (last faces)) 'magit-diff-added))))
          (kill-buffer review))))))

(ert-deftest bndos-review-agent-reply-blocks-parse-into-findings ()
  (require 'hutch-agent-shell)
  (should-not (hutch-agent-shell-extract "no block here"))
  (should (equal (hutch-agent-shell-extract "x\n```hutch-review\n{\"findings\": [{\"file\": \"a\", \"lgtm\": true}]}\n```\n")
                 '(:ok ((:file "a" :lgtm t)))))
  ;; The last block wins (an agent may quote the format first).
  (should (equal (car (hutch-agent-shell-extract
                       "```hutch-review\n{\"findings\": []}\n```\nthen\n```hutch-review\n{\"oops\": 1}\n```\n"))
                 :error))
  (should (eq (car (hutch-agent-shell-extract "```hutch-review\n{not json\n```\n")) :error)))


;;; Dashboard tree

(ert-deftest bndos-dashboard-shows-stacks-as-a-tree-with-details-on-demand ()
  (bndos-test-with-store
    (let ((bndos-task-agent--hydra (make-hash-table :test #'equal))
          (bndos-task-agent--shells (make-hash-table :test #'equal)))
      (cl-flet ((row (key branch base)
                  (bndos-task-store-save
                   (list :key key :branch branch :repo (list :main-path "/r/" :project "proj")
                         :worktree (list :path (concat "/r." branch "/") :handle branch)
                         :agent (list :provider "claude-code" :session-id (concat "hydra_session_" key))))
                  (list :key key :record (bndos-task-store-load key) :base base :worktree nil)))
        (puthash "ka" '(:state "idle" :done t) bndos-task-agent--hydra)
        (puthash "kb" '(:state "waiting") bndos-task-agent--hydra)
        (let ((bndos-task--rows (list (row "ka" "layer-a" "main") (row "kb" "layer-b" "layer-a")
                                      (row "kc" "fix" "main")))
              (bndos-task--problems nil))
          (with-temp-buffer
            (bndos-task-dashboard-mode)
            (bndos-task--print)
            (let ((lines (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
              (should (equal (nth 0 lines) "proj  /r/"))
              (should (equal (nth 1 lines) "  main"))
              (should (string-match-p "^  ├─ fix +○ idle" (nth 2 lines)))
              ;; Details sit between headings, hidden until TAB.
              (should (string-match-p "^  └─ layer-a +✓ done" (seq-find (lambda (l) (string-match-p "layer-a +" l)) lines)))
              (should (seq-find (lambda (l) (string-match-p "^     └─ layer-b +⚠ needs permission" l)) lines)))
            (should (oref (bndos-task--section-for "kb") hidden))
            ;; Really folded on screen, not just marked.
            (goto-char (bndos-task--heading-position "kb"))
            (forward-line 1)
            (should (invisible-p (point)))
            (should (string-match-p "1 need permission" (format "%s" header-line-format)))
            ;; Point and expanded details survive a refresh.
            (goto-char (bndos-task--heading-position "kb"))
            (magit-section-show (magit-current-section))
            (should (equal (bndos-task--key-at-point) "kb"))
            (forward-line 1)
            (should (equal (bndos-task--key-at-point) "kb"))
            (should (looking-at-p " +parent +layer-a"))
            (goto-char (bndos-task--heading-position "kb"))
            (bndos-task--print)
            (should (equal (bndos-task--key-at-point) "kb"))
            (should-not (oref (bndos-task--section-for "kb") hidden))
            (should (oref (bndos-task--section-for "ka") hidden))))))))


;;; Sending a diff region to the agent

(ert-deftest bndos-review-send-to-agent-fills-the-input-without-a-thread ()
  (bndos-test-with-store
    (bndos-task-store-save (list :key "kd" :worktree (list :path temporary-file-directory)
                                 :agent (list :provider "claude-code" :session-id "s1")))
    (bndos-test-with-fake-shells (("kd" shell 'ready "s1"))
      (with-temp-buffer
        (setq-local bndos-task-review-key "kd")
        (cl-letf (((symbol-function 'bndos-task-review--ensure) #'ignore)
                  ((symbol-function 'bndos-task-review-anchor-at-point)
                   (lambda () (list :path "src/a.txt" :side "new" :line 2 :end-line 3
                                    :selection "+two\n+three")))
                  ((symbol-function 'pop-to-buffer) #'ignore))
          (bndos/task-review-send-to-agent)))
      (should (equal inserted
                     `((,shell . "In src/a.txt:2-3:\n```diff\n+two\n+three\n```\n\n"))))
      (should-not (plist-get (bndos-task-store-load "kd") :threads)))))


;;; Hydra sessions

(ert-deftest bndos-agent-hydra-config-goes-through-the-shim ()
  (let* ((agent-shell-agent-configs
          (list (list (cons :identifier 'claude-code) (cons :buffer-name "Claude")
                      (cons :default-session-mode-id (lambda () "plan"))
                      (cons :client-maker #'ignore))))
         (bndos-task-hydra-command '("/opt/hydra" "--x"))
         (config (bndos-task-agent--hydra-config "claude-code"))
         (made nil))
    (should (equal (alist-get :buffer-name config) "Claude"))
    ;; Attaching never resets the session's own mode or model.
    (should-not (alist-get :default-session-mode-id config))
    (should-not (alist-get :default-model-id config))
    (cl-letf (((symbol-function 'agent-shell--make-acp-client)
               (cl-function (lambda (&key command command-params &allow-other-keys)
                              (setq made (cons command command-params))))))
      (funcall (alist-get :client-maker config) nil))
    (should (equal made '("/opt/hydra" "--x" "launch" "claude-code")))
    ;; agent-shell's own config is untouched.
    (should (functionp (alist-get :default-session-mode-id (car agent-shell-agent-configs))))))

(ert-deftest bndos-agent-lifecycle-merges-the-daemon-state ()
  (bndos-test-with-store
    (let ((bndos-task-agent--hydra (make-hash-table :test #'equal)))
      (bndos-task-store-save (list :key "kh" :worktree (list :path temporary-file-directory)
                                   :agent (list :provider "codex" :session-id "hydra_session_abcdefghij")))
      (cl-flet ((with-state (state) (puthash "kh" (list :state state) bndos-task-agent--hydra)
                  (bndos-task-agent-lifecycle (bndos-task-store-load "kh"))))
        (should (eq (with-state "idle") 'detached))
        (should (eq (with-state "stopped") 'stopped))
        (should (eq (with-state "missing") 'missing))
        (should (eq (with-state "waiting") 'blocked))
        (bndos-test-with-fake-shells (("kh" shell 'ready "hydra_session_abcdefghij"))
          ;; An attached shell answers for itself...
          (should (eq (with-state "idle") 'idle))
          ;; ...unless another client's turn is running.
          (should (eq (with-state "working") 'busy))
          (with-current-buffer shell (setq-local bndos-test--status 'blocked))
          (should (eq (with-state "idle") 'blocked))))
      (should (string-match-p "codex abcdefgh" (bndos-task-agent-summary (bndos-task-store-load "kh")))))))

(ert-deftest bndos-agent-status-entries-update-the-record ()
  (bndos-test-with-store
    (let ((bndos-task-agent--hydra (make-hash-table :test #'equal)))
      (bndos-task-store-save (list :key "ku" :agent (list :provider "claude-code" :session-id "s1" :resume "resumed")))
      (bndos-task-agent-update-from-status
       "ku" (list :session_id "s2" :agent "codex" :parent "layer-a" :ticket "gt-9" :model "large" :state "idle"))
      (let ((record (bndos-task-store-load "ku")))
        (should (equal (bndos-task-get record :agent :session-id) "s2"))
        (should-not (bndos-task-get record :agent :resume))
        (should (equal (bndos-task-get record :agent :provider) "codex"))
        (should (equal (bndos-task-get record :agent :model-actual) "large"))
        (should (equal (bndos-task-get record :base :ref) "layer-a"))
        (should (equal (plist-get record :ticket) "gt-9"))))))


;;; bin/bndos-task without sessions (no Hydra involved)

(defun bndos-test-cli (directory &rest args)
  "Run bin/bndos-task ARGS in DIRECTORY; return (EXIT . PARSED-STDOUT)."
  (let ((r (bndos-test-await
            (lambda (cb) (bndos-proc-run bndos-task-cli args :directory directory
                                         :env (cons (concat "BNDOS_TASK_STATE_DIR=" bndos-task-store-directory)
                                                    (when-let* ((hydra (getenv "BNDOS_TEST_HYDRA")))
                                                      (list (concat "BNDOS_TASK_HYDRA=" hydra))))
                                         :callback cb))
            60)))
    (cons (plist-get r :exit)
          (ignore-errors (json-parse-string (plist-get r :stdout) :object-type 'plist :array-type 'list
                                            :null-object nil :false-object nil)))))

(ert-deftest bndos-cli-status-lists-worktrees-with-their-parents ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((wt (bndos-test-worktree repo "layer-a" "main"))
             (common (bndos-test-git wt "rev-parse" "--path-format=absolute" "--git-common-dir"))
             (key (bndos-task-ensure-record common wt "layer-a")))
        (bndos-test-git repo "config" "branch.layer-a.bndos-ticket" "gt-3")
        (bndos-task-store-update key (lambda (r) (bndos-task-add-thread
                                                  r (bndos-task-make-thread :kind "comment" :author "me" :body "x"))))
        (let ((all (bndos-test-cli repo "status")))
          (should (eql (car all) 0))
          (should (= (length (cdr all)) 1)))
        (let ((one (cdr (bndos-test-cli repo "status" "layer-a"))))
          (should (equal (plist-get one :parent) "main"))
          (should (equal (plist-get one :ticket) "gt-3"))
          (should (equal (plist-get one :state) "none"))
          (should (equal (file-name-as-directory (plist-get one :worktree)) wt))
          ;; The review state comes from Emacs's record for the worktree.
          (should (eql (plist-get one :unresolved_threads) 1)))
        ;; By worktree directory too, from anywhere.
        (should (eql (car (bndos-test-cli temporary-file-directory "status" wt)) 0))
        (should (eql (car (bndos-test-cli repo "status" "nope")) 1))
        (should (eql (car (bndos-test-cli repo "status" "--wait" "bogus" "layer-a")) 1))
        (should (eql (car (bndos-test-cli repo "status" "--wait" "done" "--timeout" "1" "layer-a")) 2))))))

(provide 'bndos-review-tests)
;;; bndos-review-tests.el ends here
