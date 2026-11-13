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
  "Open task KEY's review (diff-review loads it synchronously); return its buffer."
  (bndos-task-review-open key :select nil))

(defun bndos-test-review-files (review)
  "REVIEW's files, sorted."
  (with-current-buffer review (sort (mapcar (lambda (f) (plist-get f :path)) diff-review--files) #'string<)))

(defun bndos-test-open-file (review path)
  "Show PATH of REVIEW in its file buffer; return that buffer."
  (with-current-buffer review
    (diff-review--open (or (diff-review--file-index path) (error "%s isn't in the review" path)))))

(defmacro bndos-test-with-review (spec setup &rest body)
  "SPEC is (KEY WT REVIEW): a task \"rv\" from main whose worktree SETUP fills
\(a function of the worktree), opened in diff-review."
  (declare (indent 2))
  `(bndos-test-with-store
     (bndos-test-with-repo repo
       (let* ((task (bndos-test-task repo "rv" "main"))
              (,(nth 0 spec) (car task))
              (,(nth 1 spec) (cdr task)))
         (funcall ,setup ,(nth 1 spec))
         (let ((,(nth 2 spec) (bndos-test-open-review ,(nth 0 spec))))
           (unwind-protect (progn ,@body)
             (when (buffer-live-p ,(nth 2 spec)) (kill-buffer ,(nth 2 spec)))))))))

(defun bndos-test-say (body)
  "Answer diff-review's comment prompt with BODY."
  (lambda (&rest _) body))


;;; The review is diff-review's

(ert-deftest bndos-review-opens-the-tasks-review-file-in-diff-review ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\n")
        (bndos-test-write wt "new.txt" "untracked\n"))
    (let* ((common (bndos-test-git wt "rev-parse" "--path-format=absolute" "--git-common-dir"))
           (file (expand-file-name "review/rv.json" common))
           (data (bndos-task-review-file-read file)))
      (should (file-exists-p file))
      (should (equal (gethash "task" data) key))
      (should (equal (gethash "root" data) (file-name-as-directory wt)))
      (should (equal (gethash "base" data) "main"))
      (with-current-buffer review
        (should (derived-mode-p 'diff-review-mode))
        (should bndos-task-review-mode)
        (should (string-match-p "Task +repo/rv" (buffer-string)))
        (should (string-match-p "Scope +Whole task" (buffer-string))))
      ;; The whole task: committed and untracked alike.
      (should (equal (bndos-test-review-files review) '("new.txt" "src/a.txt"))))))

(ert-deftest bndos-review-scopes-show-their-ranges ()
  (bndos-test-with-review (key wt review)
      (lambda (wt)
        (bndos-test-commit wt "committed" "c.txt" "c\n" "src/a.txt" "one\nTWO\nthree\n")
        (bndos-test-write wt "s.txt" "staged\n")
        (bndos-test-git wt "add" "s.txt")
        (bndos-test-write wt "src/a.txt" "one\nTWO\nTHREE\n")
        (bndos-test-write wt "u.txt" "untracked\n"))
    (cl-flet ((files-in (scope)
                (with-current-buffer review (bndos/task-review-set-scope scope))
                (bndos-test-review-files review)))
      (should (equal (files-in 'full) '("c.txt" "s.txt" "src/a.txt" "u.txt")))
      (should (equal (files-in 'committed) '("c.txt" "src/a.txt")))
      (should (equal (files-in 'uncommitted) '("s.txt" "src/a.txt" "u.txt")))
      (should (equal (files-in 'staged) '("s.txt")))
      ;; A file of a tree under review is shown read-only, from git.
      (with-current-buffer (bndos-test-open-file review "s.txt") (should buffer-read-only))
      (should (equal (files-in 'unstaged) '("src/a.txt" "u.txt")))
      ;; The latest iteration: what changed since the last snapshot only.
      (bndos-task-review-record-iteration key (bndos-task-git-snapshot-sync wt) "test")
      (bndos-test-write wt "c.txt" "c changed\n")
      (should (equal (files-in 'iteration) '("c.txt")))
      (with-current-buffer review
        (should (string-match-p "Scope +Latest iteration" (buffer-string)))))))

(ert-deftest bndos-review-comments-on-lines-changes-and-files ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nfour\n"))
    ;; main has one / two / three / four: two became TWO, three went.
    (let ((file (bndos-test-open-file review "src/a.txt")))
      (with-current-buffer file
        (goto-char (diff-review--line-pos 2))
        (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "on the new line")))
          (diff-review-comment))
        (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "about the change")))
          (diff-review-comment-hunk))
        (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "about the file")))
          (diff-review-comment-file)))
      (let* ((threads (plist-get (bndos-task-store-load key) :threads))
             (by (lambda (body) (seq-find (lambda (th) (equal (plist-get th :body) body)) threads))))
        (should (= (length threads) 3))
        ;; Anchors: a line, a change, a file; the lines keep the base's blob.
        (should (equal (bndos-task-get (funcall by "on the new line") :anchor :line) 2))
        (should (equal (bndos-task-get (funcall by "on the new line") :anchor :base-blob)
                       (bndos-test-git wt "rev-parse" "main:src/a.txt")))
        (should (equal (bndos-task-get (funcall by "about the change") :anchor :line) 2))
        (let ((whole (plist-get (funcall by "about the file") :anchor)))
          (should (equal (plist-get whole :path) "src/a.txt"))
          (should-not (plist-get whole :line)))
        (should (seq-every-p (lambda (th) (equal (plist-get th :state) "open")) threads)))
      ;; Drawn: the file's thread above everything, labelled with its state.
      (with-current-buffer file
        (let* ((carriers (seq-filter (lambda (o) (overlay-get o 'diff-review-anchors)) diff-review--overlays))
               (strings (mapcar (lambda (o) (substring-no-properties (or (overlay-get o 'before-string) ""))) carriers)))
          (should (seq-some (lambda (s) (string-match-p "about the file.*● open\\|● open.*about the file" s)) strings)))))))

(defmacro bndos-test-with-diff-view (spec &rest body)
  "SPEC is (KEY WT REVIEW FILE): main's src/a.txt is one / two / three, the
task's one / TWO / four, shown in FILE's window."
  (declare (indent 1))
  `(bndos-test-with-review (,(nth 0 spec) ,(nth 1 spec) ,(nth 2 spec))
       (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nfour\n"))
     (let ((,(nth 3 spec) (bndos-test-open-file ,(nth 2 spec) "src/a.txt")))
       (switch-to-buffer ,(nth 3 spec))
       (delete-other-windows)
       ,@body)))

(defun bndos-test-view-lines ()
  "This diff view's lines as (SIDE LINE TEXT)."
  (save-excursion
    (goto-char (point-min))
    (cl-loop until (eobp)
             collect (list (get-text-property (point) 'diff-review-side) (get-text-property (point) 'diff-review-line)
                           (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             do (forward-line 1))))

(defun bndos-test-view-goto (text)
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (re-search-forward (concat "^" (regexp-quote text) "$")))
  (beginning-of-line))

(ert-deftest bndos-review-diff-view-has-both-sides-as-lines ()
  (bndos-test-with-diff-view (key wt review file)
    (goto-char (diff-review--line-pos 2))
    (with-current-buffer (diff-review-toggle-view)
      (should (derived-mode-p 'diff-review-view-mode))
      (should (eq (window-buffer) (current-buffer)))
      ;; Removed lines are lines, where the file draws them.
      (should (equal (bndos-test-view-lines)
                     '(("new" 1 "one") ("old" 2 "two") ("old" 3 "three") ("new" 2 "TWO") ("new" 3 "four"))))
      (bndos-test-view-goto "two")
      (should (equal (substring-no-properties (get-text-property (point) 'line-prefix)) "-"))
      (bndos-test-view-goto "TWO")
      (should (get-text-property (point) 'diff-review-added)))))

(ert-deftest bndos-review-diff-view-and-file-map-onto-each-other ()
  (bndos-test-with-diff-view (key wt review file)
    ;; The file's line 2, column 1 is TWO's in the view.
    (goto-char (+ (diff-review--line-pos 2) 1))
    (diff-review-toggle-view)
    (should (equal (buffer-substring-no-properties (line-beginning-position) (line-end-position)) "TWO"))
    (should (= (current-column) 1))
    ;; Back from a removed line: the line it's drawn above.
    (bndos-test-view-goto "three")
    (diff-review-toggle-view)
    (should (eq (current-buffer) file))
    (should (= (line-number-at-pos) 2))
    ;; A region goes along, the removed lines above its first line with it.
    (goto-char (diff-review--line-pos 2))
    (push-mark (point) t t)
    (goto-char (save-excursion (goto-char (diff-review--line-pos 3)) (line-end-position)))
    (activate-mark)
    (diff-review-toggle-view)
    (should (use-region-p))
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end)) "two\nthree\nTWO\nfour"))))

(ert-deftest bndos-review-diff-view-comments-are-exact ()
  (bndos-test-with-diff-view (key wt review file)
    (with-current-buffer (diff-review-toggle-view)
      ;; three and TWO: one removed line and one added.
      (bndos-test-view-goto "three")
      (push-mark (point) t t)
      (bndos-test-view-goto "TWO")
      (end-of-line)
      (activate-mark)
      (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "this swap")))
        (diff-review-view-comment))
      ;; Only two, a removed line.
      (bndos-test-view-goto "two")
      (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "why two")))
        (diff-review-view-comment)))
    (let* ((threads (plist-get (bndos-task-store-load key) :threads))
           (by (lambda (body) (seq-find (lambda (th) (equal (plist-get th :body) body)) threads)))
           (quote (lambda (body) (bndos-task-get (bndos-task-thread-with-selection (funcall by body) wt)
                                                  :anchor :selection))))
      (should (equal (bndos-task-get (funcall by "this swap") :anchor :line) 2))
      (should (equal (bndos-task-get (funcall by "this swap") :anchor :removed) '((3 3))))
      (should (equal (funcall quote "this swap") "-three\n+TWO"))
      (should (equal (bndos-task-get (funcall by "why two") :anchor :side) "old"))
      (should (equal (bndos-task-get (funcall by "why two") :anchor :line) 2))
      (should (equal (funcall quote "why two") "-two")))
    ;; Both threads show in both views.
    (let ((ids (mapcar (lambda (th) (plist-get th :id)) (plist-get (bndos-task-store-load key) :threads)))
          (shown (lambda (overlays) (sort (delete-dups (delq nil (mapcar (lambda (o) (and (overlay-buffer o) (gethash "id" (or (overlay-get o 'diff-review-comment) (make-hash-table))))) overlays))) #'string<))))
      (should (equal (funcall shown (with-current-buffer file diff-review--overlays)) (sort (copy-sequence ids) #'string<)))
      (should (equal (funcall shown (with-current-buffer (buffer-local-value 'diff-review--view file) diff-review--view-overlays))
                     (sort (copy-sequence ids) #'string<))))))

(ert-deftest bndos-review-diff-view-comments-on-the-whole-change ()
  (bndos-test-with-diff-view (key wt review file)
    (with-current-buffer (diff-review-toggle-view)
      (bndos-test-view-goto "three")
      (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "the change")))
        (diff-review-view-comment-change)))
    (let ((thread (car (plist-get (bndos-task-store-load key) :threads))))
      (should (equal (bndos-task-get thread :anchor :line) 2))
      (should (equal (bndos-task-get thread :anchor :end-line) 3))
      (should (equal (bndos-task-get (bndos-task-thread-with-selection thread wt) :anchor :selection)
                     "-two\n-three\n+TWO\n+four")))))

(ert-deftest bndos-review-open-threads-keep-the-diff-colours ()
  (let ((diff-review-thread-marker "▎")
        (diff-review-tint-open-thread nil))
    (bndos-test-with-diff-view (key wt review file)
      (goto-char (diff-review--line-pos 2))
      (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "a note")))
        (diff-review-comment-in-view))
      (should (= (buffer-local-value 'left-margin-width file) 1))
      (let ((margin-mark-p (lambda (string)
                           (equal (car-safe (get-text-property 0 'display string)) '(margin left-margin)))))
        (with-current-buffer (buffer-local-value 'diff-review--view file)
          (bndos-test-view-goto "TWO")
          (diff-review--view-track)
          (let ((thread (seq-find (lambda (o) (overlay-get o 'diff-review-comment)) diff-review--view-overlays)))
            (should (overlay-get thread 'diff-review-open))
            (should-not (overlay-get thread 'face)))
          (should (= left-margin-width 1))
          (should (seq-some (lambda (o) (and (overlay-get o 'before-string)
                                             (funcall margin-mark-p (overlay-get o 'before-string))))
                            diff-review--view-overlays)))
        (with-current-buffer file
          (let ((thread (seq-find (lambda (o) (overlay-get o 'diff-review-comment)) diff-review--overlays)))
            (diff-review--redraw-thread thread t)
            (should-not (overlay-get thread 'face)))
          (should (seq-some (lambda (o) (and (overlay-get o 'before-string)
                                             (funcall margin-mark-p (overlay-get o 'before-string))))
                            diff-review--overlays)))))))

(ert-deftest bndos-review-threads-are-walked-and-listed-across-files-and-views ()
  (bndos-test-with-diff-view (key wt review file)
    (bndos-test-write wt "b.txt" "b\n")
    (with-current-buffer review (revert-buffer t t t))
    (let ((say (lambda (body) (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say body)))
                                (diff-review-comment)))))
      (with-current-buffer (bndos-test-open-file review "b.txt")
        (funcall say "on b"))
      (with-current-buffer (bndos-test-open-file review "src/a.txt")
        (goto-char (diff-review--line-pos 2))
        (funcall say "on a")))
    (let ((at (lambda () (with-current-buffer (window-buffer)
                           (list major-mode
                                 (or diff-review--file-path
                                     (buffer-local-value 'diff-review--file-path diff-review--view-file))
                                 (seq-some (lambda (o) (and (overlay-get o 'diff-review-comment)
                                                            (gethash "body" (overlay-get o 'diff-review-comment))))
                                           (overlays-in (line-beginning-position) (1+ (line-end-position)))))))))
      ;; From a's diff view to b's thread, which opens in the view too.
      (with-current-buffer (bndos-test-open-file review "src/a.txt")
        (with-current-buffer (diff-review-toggle-view)
          (goto-char (point-min))
          (diff-review-next-comment)
          (should (equal (funcall at) '(diff-review-view-mode "src/a.txt" "on a")))
          (diff-review-next-comment)))
      (should (equal (funcall at) '(diff-review-view-mode "b.txt" "on b")))
      (with-current-buffer (window-buffer) (diff-review-previous-comment))
      (should (equal (funcall at) '(diff-review-view-mode "src/a.txt" "on a")))
      ;; From the overview, and through the list.
      (with-current-buffer review
        (goto-char (point-min))
        (diff-review-next-comment))
      ;; The tree's first file: directories come first.
      (should (equal (cadr (funcall at)) "src/a.txt"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (seq-find (lambda (c) (string-match-p "on b" c)) (all-completions "" table)))))
        (with-current-buffer review (diff-review-list-comments)))
      (should (equal (funcall at) '(diff-review-view-mode "b.txt" "on b"))))))

(ert-deftest bndos-review-drawn-rows-end-the-line-above ()
  ;; Removed lines and threads hang off the newline above, so the line
  ;; below starts its own row and keeps its number.
  (bndos-test-with-diff-view (key wt review file)
    (goto-char (diff-review--line-pos 2))
    (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "a note")))
      (diff-review-comment))
    (let* ((lifts (seq-filter (lambda (o) (overlay-get o 'diff-review-rows-of)) diff-review--overlays))
           (at (lambda (line) (seq-find (lambda (o) (= (overlay-end o) (diff-review--line-pos line))) lifts))))
      ;; Above line 2: the removed lines two and three.
      (should (string-match-p "\\`\n.*two\n.*three\\'"
                              (substring-no-properties (overlay-get (funcall at 2) 'before-string))))
      (should (= (overlay-start (funcall at 2)) (1- (diff-review--line-pos 2))))
      ;; Under line 2: the thread.
      (should (string-match-p "a note" (overlay-get (funcall at 3) 'before-string)))
      (should-not (seq-some (lambda (o) (let ((b (overlay-get o 'before-string)))
                                          (and b (not (overlay-get o 'diff-review-rows-of))
                                               (string-match-p "\n" b))))
                            diff-review--overlays)))))

(ert-deftest bndos-review-a-file-toggles-to-plain-and-back ()
  (let ((diff-review-thread-marker "▐"))
    (bndos-test-with-diff-view (key wt review file)
      (goto-char (diff-review--line-pos 2))
      (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "a note")))
        (diff-review-comment))
      (should diff-review--overlays)
      (diff-review-toggle-plain)
      (should-not diff-review--overlays)
      (should (= left-margin-width 0))
      ;; Still the plain file after the review redraws.
      (with-current-buffer review (revert-buffer t t t))
      (should-not diff-review--overlays)
      (should diff-review-file-mode)
      (diff-review-toggle-plain)
      (should (seq-some (lambda (o) (overlay-get o 'diff-review-comment)) diff-review--overlays))
      (should (= left-margin-width 1)))))

(ert-deftest bndos-review-markdown-bodies-keep-lists-and-code ()
  (skip-unless (require 'markdown-mode nil t))
  (let ((lines (diff-review--body-lines "Done:\n\n- one thing\n- another thing\n\n```elisp\n(defun f (x)\n  x)\n```\n\nRan `make`." 40)))
    (should (member "- one thing" (mapcar (lambda (l) (replace-regexp-in-string "^[•●◦*] " "- " l)) lines)))
    (should (member "(defun f (x)" lines))
    (should (member "  x)" lines))
    (should-not (seq-some (lambda (l) (string-match-p "```\\|`make`" l)) lines))))

(ert-deftest bndos-review-the-thread-panel-follows-and-replies ()
  (skip-unless (require 'markdown-mode nil t))
  (bndos-test-with-diff-view (key wt review file)
    (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "first")))
      (goto-char (diff-review--line-pos 1)) (diff-review-comment))
    (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "second")))
      (goto-char (diff-review--line-pos 3)) (diff-review-comment))
    (goto-char (diff-review--line-pos 1))
    (diff-review-thread-panel)
    (let ((panel (get-buffer "*review thread*")))
      (unwind-protect
          (progn
            (should (get-buffer-window panel))
            (with-current-buffer panel (should (string-match-p "first" (buffer-string))))
            ;; Inline, threads are one row while the panel shows them.
            (diff-review--track-threads)
            (should-not (seq-some (lambda (o) (overlay-get o 'diff-review-open)) (diff-review--thread-overlays)))
            ;; It follows point.
            (goto-char (diff-review--line-pos 3))
            (diff-review--track-threads)
            (with-current-buffer panel (should (string-match-p "second" (buffer-string))))
            ;; A reply from it lands on its thread and shows.
            (with-current-buffer panel
              (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "a reply")))
                (diff-review-panel-reply))
              (should (string-match-p "a reply" (buffer-string))))
            (let ((thread (seq-find (lambda (th) (equal (plist-get th :body) "second"))
                                    (plist-get (bndos-task-store-load key) :threads))))
              (should (equal (plist-get (car (plist-get thread :replies)) :body) "a reply"))))
        (when (get-buffer-window panel) (delete-window (get-buffer-window panel)))
        (kill-buffer panel)))))

(defmacro bndos-test-with-two-files (spec &rest body)
  "SPEC is (KEY WT REVIEW): a task that changed src/a.txt and added b.txt."
  (declare (indent 1))
  `(bndos-test-with-review (,(nth 0 spec) ,(nth 1 spec) ,(nth 2 spec))
       (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nfour\n" "b.txt" "b\n"))
     (switch-to-buffer ,(nth 2 spec))
     (delete-other-windows)
     ,@body))

(ert-deftest bndos-review-a-file-entered-later-has-its-own-hunks ()
  (bndos-test-with-two-files (key wt review)
    (bndos-test-open-file review "src/a.txt")   ; the review's current file
    (let ((b (find-file-noselect (expand-file-name "b.txt" wt))))
      (with-current-buffer review (diff-review--enter b "b.txt"))
      (with-current-buffer b
        (should (equal (mapcar (lambda (h) (list (plist-get h :line) (plist-get h :count))) diff-review--hunks)
                       '((1 1))))))))

(ert-deftest bndos-review-a-file-visited-any-way-joins-its-review ()
  (bndos-test-with-two-files (key wt review)
    (let ((file (expand-file-name "b.txt" wt)))
      (let ((diff-review-claim-visits nil))
        (find-file file)
        (diff-review--follow)
        (should-not diff-review-file-mode))
      (kill-buffer (current-buffer))
      (let ((diff-review-claim-visits t))
        (find-file file)
        (diff-review--follow)
        (should diff-review-file-mode)
        (should (eq diff-review--review review))
        (should (equal diff-review--file-path "b.txt"))
        (should (diff-review--sidebar-window (selected-window)))))))

(ert-deftest bndos-review-adopt-takes-open-files-over ()
  (bndos-test-with-two-files (key wt review)
    (let ((b (find-file-noselect (expand-file-name "b.txt" wt)))
          (a (find-file-noselect (expand-file-name "src/a.txt" wt))))
      (should-not (buffer-local-value 'diff-review-file-mode b))
      (with-current-buffer review (diff-review-adopt))
      (should (buffer-local-value 'diff-review-file-mode a))
      (should (eq (buffer-local-value 'diff-review--review b) review)))))

(ert-deftest bndos-review-the-tree-comes-back-from-anywhere-in-the-worktree ()
  (bndos-test-with-two-files (key wt review)
    ;; With the review open but its tree off.
    (with-current-buffer review (setq diff-review--sidebar-enabled nil))
    (find-file (expand-file-name "b.txt" wt))
    (diff-review--follow)
    (should-not (diff-review--sidebar-window (selected-window)))
    (bndos/task-review-tree)
    (should diff-review-file-mode)
    (should (diff-review--sidebar-window (selected-window)))
    ;; With no review open: the task's opens unseen.
    (kill-buffer review)
    (let ((file (find-file (expand-file-name "src/a.txt" wt))))
      (should-not diff-review-file-mode)
      (bndos/task-review-tree)
      (bndos-proc-wait (lambda () (buffer-local-value 'diff-review-file-mode file)) 10)
      (should (buffer-local-value 'diff-review-file-mode file))
      (should (eq (window-buffer) file))
      (should (diff-review--sidebar-window (selected-window))))))

(ert-deftest bndos-review-refine-finds-the-changed-words ()
  (pcase-let ((`(,old . ,new) (diff-review--refine "title: Page 3\n" "title: Page three\n")))
    (should (equal (mapcar (lambda (s) (substring "title: Page 3\n" (car s) (cdr s))) old) '("3")))
    (should (equal (mapcar (lambda (s) (substring "title: Page three\n" (car s) (cdr s))) new) '("three")))))

(ert-deftest bndos-review-refined-words-in-both-views ()
  (let ((diff-review-refine t))
    (bndos-test-with-review (key wt review)
        (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\ntwo words\nthree\n"))
      ;; main has one / two / three: line 2 gained a word.
      (let ((file (bndos-test-open-file review "src/a.txt")))
        (with-current-buffer file
          (let ((added (seq-find (lambda (o) (eq (overlay-get o 'face) 'diff-review-refine-added))
                                 diff-review--overlays)))
            (should added)
            (should (string-match-p "words" (buffer-substring (overlay-start added) (overlay-end added))))))
        (switch-to-buffer file)
        (with-current-buffer (with-current-buffer file (diff-review-toggle-view))
          (should (seq-find (lambda (o) (eq (overlay-get o 'face) 'diff-review-refine-added))
                            diff-review--view-overlays)))))
    ;; Off: nothing refined.
    (let ((diff-review-refine nil))
      (bndos-test-with-review (key wt review)
          (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\ntwo words\nthree\n"))
        (with-current-buffer (bndos-test-open-file review "src/a.txt")
          (should-not (seq-find (lambda (o) (eq (overlay-get o 'face) 'diff-review-refine-added))
                                diff-review--overlays)))))))

(defun bndos-test-gh-stub (dir pr threads)
  "A gh in DIR answering `pr view' with PR and `api graphql' with THREADS (plists)."
  (let ((script (expand-file-name "gh" dir))
        (pr-file (expand-file-name "pr.json" dir))
        (gql-file (expand-file-name "gql.json" dir)))
    (with-temp-file pr-file (insert (json-serialize pr)))
    (with-temp-file gql-file
      (insert (json-serialize
               (list :data (list :repository
                                 (list :pullRequest
                                       (list :reviewThreads
                                             (list :pageInfo (list :hasNextPage :false :endCursor :null)
                                                   :nodes (vconcat threads)))))))))
    (with-temp-file script
      (insert (format "#!/bin/sh\ncase \"$1\" in pr) cat '%s';; api) cat '%s';; esac\n" pr-file gql-file)))
    (set-file-modes script #o755)
    script))

(defun bndos-test-gh-thread (id side line commit body &rest keys)
  (list :id id :isResolved (if (plist-get keys :resolved) t :false) :path "src/a.txt" :diffSide side
        :originalLine line :originalStartLine :null
        :comments (list :nodes (vconcat (mapcar (lambda (b)
                                                   (list :url (concat "https://github.com/me/repo/pull/7#" id)
                                                         :body b :createdAt "2026-11-12T10:00:00Z"
                                                         :author (list :login "octo") :originalCommit (list :oid commit)))
                                                 (cons body (plist-get keys :replies)))))))

(ert-deftest bndos-review-github-threads-show-read-only ()
  (require 'bndos-review-github)
  (bndos-test-with-two-files (key wt review)
    (let* ((stubs (make-temp-file "bndos-gh-" t))
           (head (bndos-test-git wt "rev-parse" "HEAD"))
           (bndos-review-github-program
            (bndos-test-gh-stub stubs (list :number 7 :url "https://github.com/me/repo/pull/7" :state "OPEN"
                                            :baseRefName "main" :baseRefOid (bndos-test-git wt "rev-parse" "main"))
                                (list (bndos-test-gh-thread "T1" "RIGHT" 2 head "Why uppercase?")
                                      (bndos-test-gh-thread "T2" "LEFT" 2 head "Keep two?")
                                      (bndos-test-gh-thread "T3" "RIGHT" 3 head "Fine." :resolved t
                                                            :replies '("Thanks"))))))
      (unwind-protect
          (progn
            (bndos-review-github-refresh review)
            (bndos-proc-wait (lambda () (plist-get (buffer-local-value 'bndos-review-github--state review) :number)) 10)
            (with-current-buffer review
              (should (= (length diff-review-external-comments) 3))
              (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "GitHub +PR #7 · 3 threads (1 resolved)" text))
                (should (string-match-p "GitHub (3)" text))))
            (let ((file (bndos-test-open-file review "src/a.txt")))
              (with-current-buffer file
                ;; On its line, from the commit it was written on.
                (let ((t1 (seq-find (lambda (o) (equal (gethash "id" (overlay-get o 'diff-review-comment)) "github:T1"))
                                    (diff-review--thread-overlays))))
                  (should t1)
                  (should (equal (diff-review--thread-span t1) '(2 . 2))))
                ;; On GitHub's left side: among the removed lines.
                (should (seq-find (lambda (th) (equal (gethash "id" (car th)) "github:T2")) (diff-review--old-threads)))
                ;; Read only.
                (goto-char (diff-review--line-pos 2))
                ;; Two threads here, both GitHub's: whichever is picked refuses.
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (_prompt collection &rest _) (car (all-completions "" collection)))))
                  (should (string-match-p "GitHub thread"
                                          (cadr (should-error (diff-review-reply) :type 'user-error)))))
                (should-error (bndos/task-review-resolve) :type 'user-error)))
            ;; Hidden, and back.
            (with-current-buffer review
              (bndos-review-github-toggle)
              (should-not diff-review-external-comments)
              (bndos-review-github-toggle)
              (should (= (length diff-review-external-comments) 3))))
        (delete-directory stubs t)))))

(defun bndos-test-section-at (regexp)
  "The overview section whose heading matches REGEXP; point there."
  (goto-char (point-min))
  (re-search-forward regexp)
  (magit-current-section))

(ert-deftest bndos-review-the-overview-unfolds-threads-and-diffs ()
  (let ((diff-review-overview-threads 'full)
        (diff-review-overview-diffs t))
    (bndos-test-with-review (key wt review)
        (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\n"))
      (let ((th (bndos-task-thread-add-reply
                 (bndos-task-make-thread :kind "comment" :author "me" :body "why upper?"
                                         :anchor (bndos-task-review--line-anchor wt "src/a.txt" 2 nil))
                 (bndos-task-make-reply :author "agent:x" :kind "agent-response" :status "answered"
                                        :body "For **emphasis**."))))
        (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r th))))
      (with-current-buffer review
        (revert-buffer t t t)
        (switch-to-buffer review)
        ;; A thread: folded, its whole conversation inside.
        (let ((section (bndos-test-section-at "src/a.txt:2  me")))
          (should (oref section hidden))
          (magit-section-show section)
          (let ((body (buffer-substring-no-properties (oref section content) (oref section end))))
            (should (string-match-p "why upper\\?" body))
            (should (string-match-p "For emphasis\\." body))))   ; markdown rendered
        ;; A file: folded, its diff inside with the thread under its line.
        (let ((section (bndos-test-section-at "M a\\.txt")))
          (should (oref section hidden))
          (magit-section-show section)
          (let ((body (buffer-substring-no-properties (oref section content) (oref section end))))
            (should (string-match-p "^ *-two$" body))
            (should (string-match-p "^ *\\+TWO$" body))
            (should (string-match-p "why upper\\?" body)))
          ;; RET on a diff line shows the file at that line.
          (goto-char (oref section content))
          (re-search-forward "^ *\\+TWO$")
          (with-current-buffer (progn (diff-review-open-at-point) (window-buffer))
            (should (string-suffix-p "/src/a.txt" buffer-file-name))
            (should (= (line-number-at-pos) 2))))))))

(ert-deftest bndos-review-a-thread-that-fails-to-draw-leaves-the-overview-whole ()
  (let ((diff-review-overview-threads 'full))
    (bndos-test-with-review (key wt review)
        (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\n"))
      (dolist (body '("first" "second"))
        (let ((th (bndos-task-make-thread :kind "comment" :author "me" :body body
                                          :anchor (bndos-task-review--line-anchor wt "src/a.txt" 2 nil))))
          (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r th)))))
      (with-current-buffer review
        (cl-letf (((symbol-function 'diff-review--overview-thread)
                   (lambda (_) (error "Boom"))))
          (revert-buffer t t t))
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "can't show this thread: Boom" text))
          ;; Both threads listed, and the sections after them drawn and folded.
          (should (string-match-p "first" text))
          (should (string-match-p "second" text)))
        (should (oref (bndos-test-section-at "^Activity") hidden))))))

(ert-deftest bndos-review-refine-by-words-without-a-process ()
  (let ((show (lambda (o n) (let ((r (diff-review--refine-lcs o n)))
                              (list (mapcar (lambda (s) (substring o (car s) (cdr s))) (car r))
                                    (mapcar (lambda (s) (substring n (car s) (cdr s))) (cdr r)))))))
    (should (equal (funcall show "return render(request)\n" "return redirect(request)\n")
                   '(("render") ("redirect"))))
    (should (equal (funcall show "a = f(x, y)\nb = 2\n" "a = f(x, z)\nb = 2\nc = 3\n")
                   '(("y") ("z" "c = 3\n"))))
    ;; Too long to compare here: smerge's turn.
    (should-not (diff-review--refine-lcs (mapconcat #'identity (make-list 300 "a b") " ")
                                         (mapconcat #'identity (make-list 300 "c d") " ")))))

(ert-deftest bndos-review-line-positions-stay-right-across-edits ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (should (= (diff-review--line-pos 3) 9))
    (should (= (diff-review--line-pos 4) 15))   ; counted on from line 3
    (goto-char (point-min))
    (insert "zero\n")                            ; the text changed: counted afresh
    (should (= (diff-review--line-pos 4) 14))
    (should (= (diff-review--line-pos 2) 6))))

(ert-deftest bndos-review-refining-later-changes-waits-for-idle ()
  (let ((diff-review-refine t)
        (diff-review-refine-eager 1))
    (bndos-test-with-review (key wt review)
        (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "ONE\ntwo\nTHREE\n"
                                        "src/b.txt" "x\n"))
      (with-current-buffer (bndos-test-open-file review "src/a.txt")
        (let ((refined (lambda () (seq-count (lambda (o) (eq (overlay-get o 'face) 'diff-review-refine-added))
                                             diff-review--overlays))))
          (when (> (length diff-review--hunks) 1)
            (should diff-review--refine-jobs)
            (let ((before (funcall refined)))
              (while diff-review--refine-jobs (funcall (pop diff-review--refine-jobs)))
              (should (> (funcall refined) before)))))))))

(ert-deftest bndos-review-the-diff-view-is-colored-as-it-shows ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/c.el" "(defun f () 1)\n(defun g () 2)\n"))
    (let ((file (bndos-test-open-file review "src/c.el")))
      (switch-to-buffer file)
      (with-current-buffer (with-current-buffer file (diff-review-toggle-view))
        (diff-review--view-fontify (point-min) (point-max))
        (goto-char (point-min))
        (search-forward "defun")
        (should (memq 'font-lock-keyword-face (ensure-list (get-text-property (match-beginning 0) 'face))))))))

(ert-deftest bndos-review-overview-diffs-are-capped-and-reused ()
  (let ((diff-review-overview-diffs t)
        (diff-review-overview-diff-max-lines 3)
        (drawn 0))
    (bndos-test-with-review (key wt review)
        (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "ONE\nTWO\nTHREE\nfour\nfive\n"))
      (with-current-buffer review
        (switch-to-buffer review)
        (let ((section (bndos-test-section-at "M a\\.txt")))
          (magit-section-show section)
          (should (string-match-p "more lines: RET opens the file"
                                  (buffer-substring-no-properties (oref section content) (oref section end)))))
        ;; Drawn again only when what it shows changed.
        (advice-add 'diff-review--draw-file-diff :before (lambda (&rest _) (cl-incf drawn)) '((name . count)))
        (unwind-protect
            (progn (diff-review--render)
                   (magit-section-show (bndos-test-section-at "M a\\.txt"))
                   (should (= drawn 0)))
          (advice-remove 'diff-review--draw-file-diff 'count))))))

(ert-deftest bndos-review-jumps-land-where-the-thread-hangs ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\nfour\nfive\n"))
    (let ((th (bndos-task-make-thread :kind "comment" :author "me" :body "the range"
                                      :anchor (bndos-task-review--line-anchor wt "src/a.txt" 1 4))))
      (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r th))))
    (with-current-buffer review (revert-buffer t t t))
    (let ((file (bndos-test-open-file review "src/a.txt")))
      (switch-to-buffer file)
      (with-current-buffer review (diff-review--goto-comment (plist-get (car (plist-get (bndos-task-store-load key) :threads)) :id)))
      (with-current-buffer (window-buffer)
        (should (= (line-number-at-pos) 4))))))

;; Threads on lines 1-4, 2 and 5: each step lands on the next, from the
;; last line of a range as from inside a nested one.
(ert-deftest bndos-review-walking-steps-thread-by-thread ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\nfour\nfive\n"))
    (pcase-dolist (`(,body ,from ,to) '(("outer" 1 4) ("inner" 2 2) ("after" 5 5)))
      (let ((th (bndos-task-make-thread :kind "comment" :author "me" :body body
                                        :anchor (bndos-task-review--line-anchor wt "src/a.txt" from to))))
        (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r th)))))
    (with-current-buffer review (revert-buffer t t t))
    (let ((file (bndos-test-open-file review "src/a.txt")))
      (switch-to-buffer file)
      (goto-char (point-min))
      (let (lines)
        (dolist (command '(diff-review-next-comment diff-review-next-comment diff-review-previous-comment
                           diff-review-previous-comment diff-review-next-comment))
          (with-current-buffer (window-buffer) (funcall command))
          (push (with-current-buffer (window-buffer) (line-number-at-pos)) lines))
        (should (equal (nreverse lines) '(2 5 2 4 2)))))))

(ert-deftest bndos-review-marking-a-thread-shows-in-the-overview-diff ()
  (let ((diff-review-overview-diffs t))
    (bndos-test-with-review (key wt review)
        (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\n"))
      (let ((th (bndos-task-make-thread :kind "comment" :author "me" :body "louder"
                                        :anchor (bndos-task-review--line-anchor wt "src/a.txt" 2 2))))
        (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r th)))
        (with-current-buffer review
          (switch-to-buffer review)
          (revert-buffer t t t)
          (magit-section-show (bndos-test-section-at "M a\\.txt"))
          (setq bndos-task-review--marked (list (plist-get th :id)))
          (diff-review--render)
          (let ((section (bndos-test-section-at "M a\\.txt")))
            (magit-section-show section)
            (should (string-match-p "\\[marked\\]"
                                    (buffer-substring-no-properties (oref section content) (oref section end))))))))))

(ert-deftest bndos-review-message-cards-show-the-agents-status ()
  (skip-unless (require 'markdown-mode nil t))
  (bndos-test-with-diff-view (key wt review file)
    (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "why?")))
      (goto-char (diff-review--line-pos 2)) (diff-review-comment))
    (bndos-task-store-update
     key (lambda (r) (bndos-task-update-thread
                      r (plist-get (car (plist-get r :threads)) :id)
                      (lambda (th) (bndos-task-thread-add-reply
                                    th (bndos-task-make-reply :author "agent:x" :kind "agent-response"
                                                              :status "addressed" :body "fixed"
                                                              :files '("src/a.txt")))))))
    (with-current-buffer review (revert-buffer t t t))
    (with-current-buffer file
      (goto-char (diff-review--line-pos 2))
      (diff-review-thread-panel))
    (let ((panel (get-buffer "*review thread*")))
      (unwind-protect
          (with-current-buffer panel
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "◆ agent:x +addressed" text))   ; the status as a chip in its header
              (should (string-match-p "│ files: src/a.txt" text))))
        (when (get-buffer-window panel) (delete-window (get-buffer-window panel)))
        (kill-buffer panel)))))

(ert-deftest bndos-review-the-overview-has-the-brief-and-activity ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\n"))
    (bndos-test-write bndos-task-store-directory (concat "briefs/" key ".md") "Make it shout.\n\nAll of it.\n")
    (bndos-test-add-thread key "louder")
    (with-current-buffer review
      (revert-buffer t t t)
      (let ((brief (bndos-test-section-at "^Brief +Make it shout\\.")))
        (should (oref brief hidden))
        (should (string-match-p "All of it\\."
                                (buffer-substring-no-properties (oref brief content) (oref brief end)))))
      (let ((activity (bndos-test-section-at "^Activity +[0-9]+ events?")))
        (should (oref activity hidden))
        (should (string-match-p "opened “louder”"
                                (buffer-substring-no-properties (oref activity content) (oref activity end))))))))

(defmacro bndos-test-with-github (spec pr threads &rest body)
  "SPEC is (KEY WT REVIEW HEAD): a task review whose gh answers PR and THREADS,
functions of HEAD (the worktree's HEAD), fetched before BODY."
  (declare (indent 3))
  `(progn
     (require 'bndos-review-github)
     (bndos-test-with-two-files (,(nth 0 spec) ,(nth 1 spec) ,(nth 2 spec))
       (let* ((stubs (make-temp-file "bndos-gh-" t))
              (,(nth 3 spec) (bndos-test-git ,(nth 1 spec) "rev-parse" "HEAD"))
              (bndos-review-github-program (bndos-test-gh-stub stubs ,pr ,threads)))
         (unwind-protect
             (progn
               (bndos-review-github-refresh ,(nth 2 spec))
               (bndos-proc-wait (lambda () (plist-get (buffer-local-value 'bndos-review-github--state ,(nth 2 spec))
                                                      :number))
                                15)
               ,@body)
           (delete-directory stubs t))))))

(ert-deftest bndos-review-github-asks-for-the-reviews-own-branch ()
  (require 'bndos-review-github)
  (bndos-test-with-two-files (key wt review)
    (with-current-buffer review
      (let ((default-directory diff-review--root))
        ;; The worktree: gh's own lookup of its checked-out branch.
        (should (equal (bndos-review-github--lookup) (list "pr" "view" "--json" bndos-review-github--fields)))
        ;; A branch: by name.
        (let ((diff-review--tip "abc") (diff-review--head-name "rv"))
          (should (member "rv" (bndos-review-github--lookup))))
        ;; Commits: none.
        (let ((diff-review--tip "abc") (diff-review--head-name "abc..def"))
          (should-not (bndos-review-github--lookup)))))))

(ert-deftest bndos-review-github-a-merged-pull-request-waits-to-be-asked ()
  (bndos-test-with-github (key wt review head)
      (list :number 9 :url "https://github.com/me/repo/pull/9" :state "MERGED" :baseRefName "main"
            :baseRefOid (bndos-test-git wt "rev-parse" "main"))
      (list (bndos-test-gh-thread "M1" "RIGHT" 2 head "old news"))
    (with-current-buffer review
      (should-not diff-review-external-comments)
      (should (string-match-p "^GitHub +PR #9 merged · 1 thread.*hidden" (buffer-string)))
      (bndos-review-github-toggle)
      (should (= (length diff-review-external-comments) 1)))))

(ert-deftest bndos-review-github-threads-on-unknown-commits-are-outdated ()
  (bndos-test-with-github (key wt review head)
      (list :number 3 :url "https://github.com/me/repo/pull/3" :state "OPEN" :baseRefName "main"
            :baseRefOid (bndos-test-git wt "rev-parse" "main"))
      (list (bndos-test-gh-thread "U1" "RIGHT" 2 "0123456789abcdef0123456789abcdef01234567" "force-pushed away")
            (bndos-test-gh-thread "K1" "RIGHT" 2 head "still here"))
    (with-current-buffer review
      (let ((by-id (lambda (id) (seq-find (lambda (c) (equal (gethash "id" c) id)) diff-review-external-comments))))
        (should (gethash "outdated" (funcall by-id "github:U1")))
        (should-not (gethash "outdated" (funcall by-id "github:K1")))
        (should (string-match-p "(0 resolved, 1 outdated)" (buffer-string)))))
    (with-current-buffer (bndos-test-open-file review "src/a.txt")
      (let ((u1 (seq-find (lambda (o) (equal (gethash "id" (overlay-get o 'diff-review-comment)) "github:U1"))
                          (diff-review--thread-overlays))))
        (should u1)
        (should (overlay-get u1 'diff-review-stale))))))

(ert-deftest bndos-review-github-removed-line-threads-use-the-prs-base ()
  ;; The pull request's base is the task's HEAD here, not the task's parent:
  ;; a left-side thread's lines are counted in that base's blob.
  (bndos-test-with-github (key wt review head)
      (list :number 4 :url "https://github.com/me/repo/pull/4" :state "OPEN" :baseRefName "rv"
            :baseRefOid (bndos-test-git wt "rev-parse" "HEAD"))
      (list (bndos-test-gh-thread "L1" "LEFT" 2 head "on the base"))
    (with-current-buffer review
      (let ((c (car diff-review-external-comments)))
        (should (equal (gethash "side" c) "old"))
        (should (equal (gethash "blob" c) (bndos-test-git wt "rev-parse" "HEAD:src/a.txt")))))))

(ert-deftest bndos-review-github-says-nothing-without-a-pull-request-unless-asked ()
  (require 'bndos-review-github)
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\n"))
    (with-current-buffer review
      (setq bndos-review-github--state '(:error "no pull request for this branch"))
      (diff-review--render-all)
      (should-not (string-match-p "^GitHub" (buffer-string)))
      (setq bndos-review-github--state '(:error "no pull request for this branch" :manual t))
      (diff-review--render-all)
      (should (string-match-p "^GitHub +no pull request" (buffer-string))))))

(defvar bndos-test-fence-hook-ran nil)
(define-minor-mode bndos-test-fence-minor-mode "A minor mode a comment must not turn on." :global t)
(define-derived-mode bndos-test-fence-lang-mode prog-mode "Fence"
  "A major mode whose hook a comment must not run.")
(add-hook 'bndos-test-fence-lang-mode-hook (lambda () (setq bndos-test-fence-hook-ran t)))

(ert-deftest bndos-review-a-comments-code-fence-only-picks-major-modes-without-hooks ()
  (skip-unless (require 'markdown-mode nil t))
  (unwind-protect
      (progn
        ;; A fence naming a minor mode (as ```server would name server-mode).
        (diff-review--markdown "```bndos-test-fence-minor\nanything\n```")
        (should-not bndos-test-fence-minor-mode)
        ;; A major mode fontifies, without running its hooks.
        (setq bndos-test-fence-hook-ran nil)
        (diff-review--markdown "```bndos-test-fence-lang\nx\n```")
        (should-not bndos-test-fence-hook-ran)
        ;; Code in a known language is still highlighted.
        (let ((text (diff-review--markdown "```elisp\n(defun f () 1)\n```")))
          (should (string-match "defun" text))
          (should (memq 'font-lock-keyword-face
                        (ensure-list (get-text-property (match-beginning 0) 'face text))))))
    (bndos-test-fence-minor-mode -1)
    (dolist (b (buffer-list))
      (when (string-prefix-p " *markdown-code-fontification:bndos-test" (buffer-name b)) (kill-buffer b)))))

(ert-deftest bndos-review-commenting-in-a-file-goes-to-the-diff-view ()
  (bndos-test-with-diff-view (key wt review file)
    ;; Line 2 selected in the file: its removed lines come along.
    (goto-char (diff-review--line-pos 2))
    (push-mark (point) t t)
    (end-of-line)
    (activate-mark)
    (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "from the file")))
      (diff-review-comment-in-view))
    (should (eq (buffer-local-value 'major-mode (window-buffer)) 'diff-review-view-mode))
    (let ((thread (car (plist-get (bndos-task-store-load key) :threads))))
      (should (equal (bndos-task-get thread :anchor :removed) '((2 3))))
      (should (equal (bndos-task-get (bndos-task-thread-with-selection thread wt) :anchor :selection)
                     "-two\n-three\n+TWO")))))

(ert-deftest bndos-review-diff-view-follows-the-file-and-is-remembered ()
  (bndos-test-with-diff-view (key wt review file)
    (with-current-buffer (diff-review-toggle-view)
      (bndos-test-view-goto "four")
      ;; The file changes; the view is redrawn and stays on the same line
      ;; of the file, as a reverted buffer does.
      (bndos-test-write wt "src/a.txt" "zero\none\nTWO\nfour\n")
      (with-current-buffer file (revert-buffer t t t))
      (with-current-buffer review (revert-buffer t t t))
      (should (equal (seq-take (diff-review--view-at (point)) 2) '("new" 3)))
      (should (equal (cadr (assoc "zero" (mapcar (lambda (l) (list (nth 2 l) (nth 1 l))) (bndos-test-view-lines))))
                     1)))
    ;; The view was used last: the next file opens in it.
    (bndos-test-write wt "b.txt" "b\n")
    (with-current-buffer review (revert-buffer t t t))
    (bndos-test-open-file review "b.txt")
    (should (eq (buffer-local-value 'major-mode (window-buffer (selected-window))) 'diff-review-view-mode))))

(ert-deftest bndos-review-thread-states-follow-both-sides ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\n"))
    (let ((file (bndos-test-open-file review "src/a.txt")))
      (with-current-buffer file
        (goto-char (diff-review--line-pos 2))
        (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "check this")))
          (diff-review-comment))
        (goto-char (diff-review--line-pos 2))
        ;; Our resolve sets diff-review's mark; its toggle reopens ours.
        (bndos/task-review-resolve)
        (should (equal (plist-get (car (plist-get (bndos-task-store-load key) :threads)) :state) "resolved"))
        (should (gethash "resolved" (aref (bndos-task-review--in-review diff-review--comments) 0)))
        (goto-char (diff-review--line-pos 2))
        (diff-review-toggle-resolved)
        (should (equal (plist-get (car (plist-get (bndos-task-store-load key) :threads)) :state) "open")))
      ;; The overview lists threads by state.
      (with-current-buffer review
        (should (string-match-p "Open (1)" (buffer-string)))))))

(ert-deftest bndos-review-threads-from-before-move-into-the-review-file ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((task (bndos-test-task repo "old" "main"))
             (key (car task)) (wt (cdr task))
             (file (bndos-task-store-file key)))
        (bndos-test-commit wt "c" "a.txt" "a\n")
        ;; A record as the old review kept it: threads inside.
        (bndos-task-store--write-atomically
         file (bndos-task-store-encode
               (plist-put (bndos-task-store--read-json-file file) :threads
                          (list (list :id "c-old" :kind "comment" :author "me" :state "sent" :body "from before"
                                      :anchor (list :path "a.txt" :side "new" :line 1 :text "a"))))))
        (clrhash bndos-task-store--cache)
        (let ((review (bndos-test-open-review key)))
          (unwind-protect
              (let ((data (bndos-task-review-file-read (bndos-task-review-file (bndos-task-store-load key)))))
                (should (equal (gethash "body" (aref (gethash "comments" data) 0)) "from before"))
                (should (equal (gethash "lines" (aref (gethash "comments" data) 0)) [1 1]))
                (with-current-buffer review (should-not (buffer-modified-p)))
                ;; The record keeps no threads of its own now; the state survives.
                (should-not (plist-get (bndos-task-store--read-json-file file) :threads))
                (should (equal (plist-get (car (plist-get (bndos-task-store-load key) :threads)) :state) "sent")))
            (kill-buffer review)))))))

(ert-deftest bndos-review-threads-round-trip-through-comments ()
  (let* ((thread (list :id "c-1" :kind "finding" :author "ai:codex" :created "2026-01-01T00:00:00+0000"
                       :state "addressed" :body "Off by one" :title "Off by one"
                       :anchor (list :path "a.el" :side "old" :line 4 :end-line 6 :blob "abc" :hunk "@@ -4 +4 @@")
                       :suggestion (list :old "x" :new "y")
                       :replies (list (list :id "m-1" :author "agent:stub" :created "2026-01-02T00:00:00+0000"
                                            :body "done" :kind "reply" :status "addressed" :files '("a.el") :tests '("t")))))
         (comment (bndos-task-thread-to-comment thread))
         (back (bndos-task-thread-from-comment comment)))
    ;; diff-review's own fields.
    (should (equal (gethash "lines" comment) [4 6]))
    (should (equal (gethash "side" comment) "old"))
    (should-not (gethash "resolved" comment))
    ;; Ours survive the trip.
    (dolist (key '(:id :kind :author :created :state :body :title :suggestion))
      (should (equal (plist-get back key) (plist-get thread key))))
    (should (equal (bndos-task-get back :anchor :end-line) 6))
    (should (equal (bndos-task-get back :anchor :hunk) "@@ -4 +4 @@"))
    (should (equal (plist-get (car (plist-get back :replies)) :files) '("a.el")))
    ;; A comment made in diff-review alone reads as an open comment; its resolved mark is a state.
    (let ((plain (make-hash-table :test #'equal)))
      (puthash "id" "x1" plain) (puthash "path" "a.el" plain) (puthash "lines" [2 2] plain)
      (puthash "author" "me" plain) (puthash "body" "hm" plain) (puthash "replies" [] plain)
      (should (equal (plist-get (bndos-task-thread-from-comment plain) :state) "open"))
      (should (equal (plist-get (bndos-task-thread-from-comment plain) :kind) "comment"))
      (puthash "resolved" t plain)
      (should (equal (plist-get (bndos-task-thread-from-comment plain) :state) "resolved")))))

(ert-deftest bndos-review-prompts-quote-the-commented-lines ()
  (bndos-test-with-repo repo
    (let* ((blob (bndos-test-git repo "rev-parse" "HEAD:src/a.txt"))
           (thread (list :id "c-1" :anchor (list :path "src/a.txt" :side "new" :line 2 :end-line 3 :blob blob)))
           (quoted (bndos-task-thread-with-selection thread repo)))
      (should (equal (bndos-task-get quoted :anchor :selection) " two\n three"))
      (should (equal (bndos-task-get quoted :anchor :text) "two")))))

(ert-deftest bndos-review-a-selection-across-removed-lines-reaches-the-agent-as-a-diff ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nfour\nfive\n"))
    ;; main has one / two / three; now two and three became TWO, four and five were added.
    (let ((file (bndos-test-open-file review "src/a.txt")))
      (with-current-buffer file
        ;; Lines 1-3 selected: the removed two and three sit between one and TWO.
        (push-mark (diff-review--line-pos 1) t t)
        (goto-char (save-excursion (goto-char (diff-review--line-pos 3)) (line-end-position)))
        (activate-mark)
        (should (equal (diff-review--region-lines) '(1 . 3)))
        (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "this rewrite")))
          (diff-review-comment)))
      (let* ((thread (car (plist-get (bndos-task-store-load key) :threads)))
             (quoted (bndos-task-thread-with-selection thread wt)))
        (should (equal (bndos-task-get thread :anchor :base-blob) (bndos-test-git wt "rev-parse" "main:src/a.txt")))
        (should (equal (bndos-task-get quoted :anchor :selection) " one\n-two\n-three\n+TWO\n+four"))
        ;; And that is what the agent's prompt quotes.
        (should (string-match-p (regexp-quote "```diff\n one\n-two\n-three\n+TWO\n+four\n```")
                                (bndos-task-agent-prompt (bndos-task-store-load key)
                                                         (list :id "r-1" :mode "explain")
                                                         (list thread))))))))

(ert-deftest bndos-review-selections-take-the-removed-lines-of-their-lines ()
  (let ((old '("one" "two" "three" "four")) (new '("one" "four")))
    ;; Two removed after line 1, nothing added.
    (should (equal (bndos-task-selection-diff new old '((2 2 1 0)) 1 2) " one\n-two\n-three\n four"))
    ;; Removed lines belong to the line they're drawn above, all of them.
    (should (equal (bndos-task-selection-diff new old '((2 2 1 0)) 2 2) "-two\n-three\n four"))
    (should (equal (bndos-task-selection-diff new old '((2 2 1 0)) 1 1) " one"))
    ;; A deletion at the end of the file belongs to the last line.
    (should (equal (bndos-task-selection-diff '("one") '("one" "two") '((2 1 1 0)) 1 1) " one\n-two"))
    ;; A change: removed, then added in their place.
    (should (equal (bndos-task-selection-diff '("one" "TWO" "four") old '((2 2 2 1)) 1 3)
                   " one\n-two\n-three\n+TWO\n four"))))

(ert-deftest bndos-review-a-deletion-is-commented-on-the-line-below-it ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nthree\n"))
    ;; main has one / two / three: two went, drawn above three.
    (with-current-buffer (bndos-test-open-file review "src/a.txt")
      (goto-char (diff-review--line-pos 2))
      (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "why")))
        (diff-review-comment))
      ;; The change is at the line its removed lines are drawn above.
      (goto-char (diff-review--line-pos 2))
      (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "the change")))
        (diff-review-comment-hunk)))
    (let* ((threads (plist-get (bndos-task-store-load key) :threads))
           (quote (lambda (body) (bndos-task-get (bndos-task-thread-with-selection
                                                   (seq-find (lambda (th) (equal (plist-get th :body) body)) threads)
                                                   wt)
                                                  :anchor :selection))))
      (should (equal (funcall quote "why") "-two\n three"))
      ;; The change itself: the line its removed lines are drawn above.
      (should (equal (bndos-task-get (seq-find (lambda (th) (equal (plist-get th :body) "the change")) threads)
                                     :anchor :line)
                     2))
      (should (equal (funcall quote "the change") "-two\n three")))))

(ert-deftest bndos-review-a-region-over-the-whole-file-can-be-a-file-comment ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\n"))
    (with-current-buffer (bndos-test-open-file review "src/a.txt")
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (activate-mark)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'read-string-from-buffer) (bndos-test-say "all of it")))
        (diff-review-comment)))
    (let ((anchor (plist-get (car (plist-get (bndos-task-store-load key) :threads)) :anchor)))
      (should (equal (plist-get anchor :path) "src/a.txt"))
      (should-not (plist-get anchor :line)))))

(ert-deftest bndos-review-submit-sends-the-thread-at-point ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\n"))
    (let ((file (bndos-test-open-file review "src/a.txt"))
          sent)
      (with-current-buffer file
        (goto-char (diff-review--line-pos 2))
        (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say "rename")))
          (diff-review-comment))
        (goto-char (diff-review--line-pos 2))
        (cl-letf (((symbol-function 'bndos-task-agent-submit)
                   (lambda (k ids mode &rest keys) (setq sent (list k ids mode (plist-get keys :snapshot)))))
                  ((symbol-function 'completing-read) (lambda (&rest _) "implement — the agent may edit files"))
                  ((symbol-function 'y-or-n-p) #'ignore))
          (bndos/task-review-submit)))
      (pcase-let ((`(,k ,ids ,mode ,snapshot) sent))
        (should (equal k key))
        (should (equal ids (list (plist-get (car (plist-get (bndos-task-store-load key) :threads)) :id))))
        (should (equal mode "implement"))
        (should (plist-get snapshot :tree))))))

(ert-deftest bndos-review-suggestions-apply-to-the-worktree ()
  (bndos-test-with-review (key wt review)
      (lambda (wt) (bndos-test-commit wt "change" "src/a.txt" "one\nTWO\nthree\n"))
    (let ((thread (plist-put (bndos-task-make-thread :kind "finding" :author "ai:codex" :body "fix"
                                                     :anchor (bndos-task-review--line-anchor wt "src/a.txt" 2 nil))
                             :suggestion '(:old "TWO\n" :new "two\n"))))
      (bndos-task-store-update key (lambda (r) (bndos-task-add-thread r thread)))
      (with-current-buffer review
        (goto-char (point-min))
        (re-search-forward "src/a.txt:2  ai:codex")   ; the thread's line, not the activity
        (beginning-of-line)
        (bndos/task-review-apply-suggestion)
        ;; A save from the overview leaves it an overview, unmodified.
        (should-not (buffer-modified-p))
        (should (string-match-p "Task +repo/rv" (buffer-string))))
      (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "src/a.txt" wt)) (buffer-string))
                     "one\ntwo\nthree\n"))
      (should (equal (plist-get (bndos-task-thread (bndos-task-store-load key) (plist-get thread :id)) :state)
                     "resolved")))))

(ert-deftest bndos-review-multiple-reviews-are-isolated ()
  (bndos-test-with-store
    (bndos-test-with-repo repo
      (let* ((a (bndos-test-task repo "alpha" "main"))
             (b (bndos-test-task repo "beta" "main")))
        (bndos-test-commit (cdr a) "a" "a.txt" "a\n")
        (bndos-test-commit (cdr b) "b" "b.txt" "b\n")
        (let ((ra (bndos-test-open-review (car a)))
              (rb (bndos-test-open-review (car b))))
          (unwind-protect
              (progn
                (should-not (eq ra rb))
                (should (equal (bndos-test-review-files ra) '("a.txt")))
                (should (equal (bndos-test-review-files rb) '("b.txt")))
                (bndos-test-add-thread (car a) "only alpha")
                (should (= 1 (length (plist-get (bndos-task-store-load (car a)) :threads))))
                (should-not (plist-get (bndos-task-store-load (car b)) :threads))
                (should-not (equal (bndos-task-review-file (bndos-task-store-load (car a)))
                                   (bndos-task-review-file (bndos-task-store-load (car b))))))
            (kill-buffer ra)
            (kill-buffer rb)))))))

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

;;; The pr-review tree

(ert-deftest bndos-pr-review-tree-lists-the-pr-files-and-follows ()
  (require 'pr-review)
  (require 'pr-review-tree)
  (require 'magit-diff)
  (let ((buffer (get-buffer-create "*pr-review test*")))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (delete-other-windows)
          (let ((inhibit-read-only t))
            (pr-review-mode)
            (setq-local pr-review--pr-path '("me" "repo" 7)
                        pr-review--pr-info '((files (nodes . [((path . "src/a.el") (additions . 3) (deletions . 1))
                                                              ((path . "src/b.el") (additions . 5) (deletions . 0))]))))
            (magit-insert-section (pr-review-root)
              (dolist (file '("src/a.el" "src/b.el"))
                (magit-insert-section (file file)
                  (magit-insert-heading (concat file "\n"))
                  (insert "diff lines\n" "more lines\n")))))
          (goto-char (point-min))
          (pr-review-tree-toggle)
          (should (diff-review-host-sidebar-visible-p buffer))
          (let* ((sidebar (buffer-local-value 'diff-review--sidebar buffer))
                 (tree (get-buffer-window sidebar)))
            (with-current-buffer sidebar
              (should (derived-mode-p 'diff-review-sidebar-mode))
              (should (string-match-p "\\`src/.*\n.*a\\.el +\\+3 [−-]1\n.*b\\.el +\\+5 [−-]0"
                                      (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "me/repo #7  0/2 viewed" (diff-review-sidebar--header))))
            ;; Choosing b.el in the tree goes to its section in the review.
            (with-selected-window tree
              (goto-char (point-min))
              (re-search-forward "b\\.el")
              (diff-review-open-at-point))
            (should (eq (window-buffer) buffer))
            (should (equal (pr-review-tree--file-at-point) "src/b.el"))
            ;; Viewed marks are kept for the session, not written anywhere.
            (with-selected-window tree (diff-review-toggle-viewed))
            (with-current-buffer sidebar
              (should (string-match-p "1/2 viewed" (diff-review-sidebar--header))))
            ;; Moving in the review marks the file there.
            (goto-char (point-min)) (forward-line 1)
            (diff-review--host-track)
            (should (equal (plist-get (diff-review--file) :path) "src/a.el"))
            (with-current-buffer sidebar
              (should (equal (plist-get (diff-review--file (get-text-property (point) 'diff-review-index)) :path)
                             "src/a.el"))))
          (pr-review-tree-toggle)
          (should-not (diff-review-host-sidebar-visible-p buffer)))
      (kill-buffer buffer))))

(ert-deftest bndos-review-agent-reply-blocks-parse-into-findings ()
  (should-not (bndos-task-review-ai-extract "no block here"))
  (should (equal (bndos-task-review-ai-extract "x\n```review-findings\n{\"findings\": [{\"file\": \"a\", \"lgtm\": true}]}\n```\n")
                 '(:ok ((:file "a" :lgtm t)))))
  ;; The last block wins (an agent may quote the format first).
  (should (equal (car (bndos-task-review-ai-extract
                       "```review-findings\n{\"findings\": []}\n```\nthen\n```review-findings\n{\"oops\": 1}\n```\n"))
                 :error))
  (should (eq (car (bndos-task-review-ai-extract "```review-findings\n{not json\n```\n")) :error)))

(ert-deftest bndos-review-agent-findings-are-checked-against-the-change ()
  (let ((repo (file-name-as-directory (make-temp-file "bndos-ai-" t))))
    (unwind-protect
        (let ((default-directory repo))
          (bndos-test-write repo "a.txt" "one\ntwo\nthree\ntwo\n")
          (dolist (args '(("init" "-q") ("add" ".") ("-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qm" "a")))
            (apply #'call-process "git" nil nil nil args))
          (let* ((result (bndos-task-review-ai--result
                          repo "HEAD"
                          (concat "```review-findings\n"
                                  (json-encode
                                   `((findings . [((file . "a.txt") (title . "Comment") (lines . "2"))
                                                  ((file . "a.txt") (title . "Fix") (old_lines . "three") (new_lines . "3"))
                                                  ((file . "a.txt") (title . "Twice") (old_lines . "two") (new_lines . "2"))
                                                  ((file . "gone.txt") (title . "Nowhere") (lines . "1"))
                                                  ((file . "a.txt") (title . "Better") (lines . "4") (thread . "c1"))
                                                  ((thread . "c2") (withdraw . t) (description . "it was fine"))
                                                  ((file . "a.txt") (lgtm . t))])))
                                  "\n```\n")
                          "end_turn"))
                 (findings (plist-get result :findings))
                 (titled (lambda (title) (seq-find (lambda (f) (equal (plist-get f :title) title)) findings))))
            (should (eq (plist-get result :status) :ok))
            (should (= (length findings) 6))
            (should (equal (plist-get (funcall titled "Comment") :lines) "2"))
            ;; An edit matching once is anchored on its lines, with a patch.
            (should (equal (plist-get (funcall titled "Fix") :lines) "3"))
            (should (string-match-p "^-three\n\\+3$" (plist-get (funcall titled "Fix") :patch)))
            ;; One matching twice is a comment.
            (should (equal (plist-get (funcall titled "Twice") :lines) "?"))
            (should-not (plist-get (funcall titled "Twice") :old-lines))
            (should-not (funcall titled "Nowhere"))
            (should (equal (plist-get (funcall titled "Better") :thread) "c1"))
            (should (equal (seq-find (lambda (f) (eq (plist-get f :type) 'withdraw)) findings)
                           '(:type withdraw :thread "c2" :desc "it was fine"))))
          (should (equal (bndos-task-review-ai--result repo "HEAD" "no block" nil)
                         '(:status :error :emsg "the reviewer's reply had no review-findings block"))))
      (delete-directory repo t))))

(defun bndos-test-ai-thread (id author title path line &optional state)
  (append (list :id id :kind "finding" :author author :state (or state "open") :title title
                :body (concat title "\n\nwhy") :anchor (list :path path :side "new" :line line))
          nil))

(ert-deftest bndos-review-ai-reviews-dont-repeat-and-revise-their-own ()
  (let* ((record (list :threads (list (bndos-test-ai-thread "c1" "ai:claude" "Off by one" "a.el" 10)
                                      (bndos-test-ai-thread "c2" "ai:claude" "Leaks a buffer" "a.el" 20)
                                      (bndos-test-ai-thread "c3" "alex" "Rename this" "b.el" 5)
                                      (bndos-test-ai-thread "c4" "ai:claude" "Race" "c.el" 1 "resolved"))))
         (finding (lambda (title path line &optional revises)
                    (append (bndos-test-ai-thread nil "ai:codex" title path line) (list :revises revises))))
         (result (bndos-task-review--ai-apply
                  record
                  (list (funcall finding "off  by ONE" "a.el" 10)         ; a repeat, worded the same
                        (funcall finding "Race" "c.el" 1)                 ; resolved: still a repeat
                        (funcall finding "Leaks a buffer on error" "a.el" 21 "c2")
                        (funcall finding "Rename it differently" "b.el" 5 "c3") ; a person's: added
                        (funcall finding "Unchecked nil" "a.el" 30))
                  '((:thread "c1" :reason "it is fine") (:thread "c3" :reason "no"))
                  "ai:codex"))
         (threads (plist-get (car result) :threads))
         (by-id (lambda (id) (bndos-task-thread (car result) id))))
    (should (equal (cdr result) '(2 1 1 2)))
    (should (= (length threads) 6))
    (should (equal (plist-get (funcall by-id "c2") :title) "Leaks a buffer on error"))
    (should (equal (bndos-task-get (funcall by-id "c2") :anchor :line) 21))
    (should (equal (plist-get (funcall by-id "c1") :state) "resolved"))
    (should (string-match-p "Withdrawn: it is fine" (plist-get (car (last (plist-get (funcall by-id "c1") :replies))) :body)))
    ;; A person's thread is never revised or withdrawn.
    (should (equal (plist-get (funcall by-id "c3") :title) "Rename this"))
    (should (equal (plist-get (funcall by-id "c3") :state) "open"))
    (should-not (seq-some (lambda (th) (plist-member th :revises)) threads))))

(ert-deftest bndos-review-ai-prompt-lists-the-threads ()
  (let* ((thread (bndos-test-ai-thread "c1" "ai:claude" "Off by one" "a.el" 10))
         (thread (plist-put thread :replies (list (list :author "alex" :body "agreed" :status nil))))
         (context (let ((bndos-task-review-ai-include-brief nil))
                    (bndos-task-review--ai-context (list :threads (list thread)) nil))))
    (should (string-match-p "id c1, a.el:10, by ai:claude, open" context))
    (should (string-match-p "Off by one" context))
    (should (string-match-p "alex:\n    agreed" context))
    (should (string-match-p "\"withdraw\": true" context)))
  (should (equal (let ((bndos-task-review-ai-include-brief nil))
                   (bndos-task-review--ai-context (list :threads nil) nil))
                 "")))

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
        (cl-letf (((symbol-function 'bndos-task-review--ensure) (lambda () (current-buffer)))
                  ((symbol-function 'bndos-task-review--anchor-at-point)
                   (lambda () (list :path "src/a.txt" :side "new" :line 2 :end-line 3
                                    :selection "two\nthree")))
                  ((symbol-function 'pop-to-buffer) #'ignore))
          (bndos/task-review-send-to-agent)))
      (should (equal inserted
                     `((,shell . "In src/a.txt:2:\n```diff\ntwo\nthree\n```\n\n"))))
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


(ert-deftest bndos-start-options-pick-from-the-agents-list ()
  (require 'bndos-task-dashboard)
  (let ((options '((:id "small" :name "Small" :description "fast") (:id "large" :name "Large")))
        seen)
    (cl-flet ((pick (answer)
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (prompt table &rest _)
                             (setq seen (list prompt (all-completions "" table)
                                              (funcall (alist-get 'annotation-function
                                                                  (cdr (funcall table "" nil 'metadata)))
                                                       "small")))
                             answer)))
                  (bndos-task--read-option "Model" options "the agent's default"))))
      ;; Empty leaves it to the agent; a listed id or any typed id is taken.
      (should-not (pick ""))
      (should (equal (pick "large") "large"))
      (should (equal (pick "custom-id") "custom-id"))
      (pcase-let ((`(,prompt ,candidates ,annotation) seen))
        (should (string-match-p "the agent's default" prompt))
        (should (equal candidates '("small" "large")))
        (should (string-match-p "Small · fast" annotation))))))

(ert-deftest bndos-review-ai-context-joins-instructions-project-file-and-brief ()
  (bndos-test-with-store
    (let* ((worktree (file-name-as-directory (make-temp-file "bndos-wt-" t)))
           (main (file-name-as-directory (make-temp-file "bndos-main-" t)))
           (record (list :key "k1" :ticket "GT-1"
                         :worktree (list :path worktree) :repo (list :main-path main)))
           (bndos-task-review-ai-instructions "Focus on error handling."))
      (unwind-protect
          (progn
            ;; The project file is found in the main checkout when the worktree has none.
            (bndos-test-write main ".bndos/review.md" "Use the review skill.\n")
            (bndos-test-write bndos-task-store-directory "briefs/k1.md" "Add a slugify helper.\nCommit when done.\n")
            (let* ((instructions (bndos-task-review--ai-instructions record))
                   (context (bndos-task-review--ai-context record instructions))
                   (prompt (bndos-task-review-ai--prompt "b0" "h0" "a.txt (+1 -0)" context)))
              (should (equal instructions "Focus on error handling.\n\nUse the review skill."))
              (should (string-match-p (concat "Instructions for this review:\n\nFocus on error handling\\.\n\n"
                                              "Use the review skill\\.")
                                      context))
              (should (string-match-p "> Ticket: GT-1\n> Add a slugify helper\\.\n> Commit when done\\." context))
              ;; Before the required output format, which stays last.
              (should (< (string-search "Use the review skill" prompt) (string-search "End your reply" prompt)))
              (should (string-match-p "lgtm: true for a file with no issues (no other fields)\\'" prompt)))
            ;; The worktree's own file wins over the main checkout's.
            (bndos-test-write worktree ".bndos/review.md" "Use the task's conventions.")
            (should (string-match-p "task's conventions" (bndos-task-review--ai-instructions record)))
            ;; Without any of it the prompt is the plain one.
            (let ((bndos-task-review-ai-instructions nil)
                  (bndos-task-review-ai-instructions-file nil)
                  (bndos-task-review-ai-include-brief nil))
              (should (equal (bndos-task-review--ai-context record (bndos-task-review--ai-instructions record)) ""))
              (should (string-match-p "manufacturing findings\\.\n\nEnd your reply"
                                      (bndos-task-review-ai--prompt "b0" "h0" "a.txt (+1 -0)" "")))))
        (delete-directory worktree t)
        (delete-directory main t)))))

(ert-deftest bndos-review-empty-draft-is-empty-not-its-context ()
  (let ((draft (bndos-task-draft "comment" "the line commented on" #'ignore)))
    (unwind-protect
        (with-current-buffer draft
          (should (equal (bndos-task-draft--text) ""))
          (should-error (bndos-task-draft-submit) :type 'user-error)
          (goto-char (point-max))
          (insert "a real comment")
          (should (equal (bndos-task-draft--text) "a real comment")))
      (kill-buffer draft))))

(ert-deftest bndos-review-ai-instructions-can-be-edited-for-one-review ()
  (bndos-test-with-review (_key _wt review)
      (lambda (wt) (bndos-test-commit wt "change" "a.txt" "a\n"))
    (let ((bndos-task-review-ai-instructions "Use the review skill.")
          started)
      (cl-letf (((symbol-function 'bndos-task-review--ai-start)
                 (lambda (&optional instructions) (push instructions started))))
        ;; Without a prefix the defaults go straight in.
        (with-current-buffer review (bndos/task-review-ai-review))
        (should (equal (car started) "Use the review skill."))
        ;; C-c C-w (or a prefix) starts a draft from them; what's submitted
        ;; is used, even nothing.
        (should (eq (keymap-lookup bndos-task-review-mode-map "C-c C-w")
                    #'bndos/task-review-ai-review-with-instructions))
        (dolist (text '("Focus on the locking." ""))
          (with-current-buffer review (bndos/task-review-ai-review-with-instructions))
          (with-current-buffer (get-buffer "*task-draft: AI review instructions*")
            (should (string-match-p "Use the review skill\\." (bndos-task-draft--text)))
            (let ((inhibit-read-only t))
              (delete-region (next-single-property-change (point-min) 'bndos-draft-context nil (point-max))
                             (point-max)))
            (goto-char (point-max))
            (insert text)
            (bndos-task-draft-submit))
          (should (equal (car started) text)))))))

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
