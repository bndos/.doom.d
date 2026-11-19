;;; bndos-review-changes.el --- The changes a reply links, from git -*- lexical-binding: t; -*-

;;; Commentary:

;; A reply drafted for a pull request may link the changes that answer it:
;; hunks of the diff from the commit the comment was made on (its base) to
;; the branch.  A draft keeps no diff text, only where each hunk is:
;;
;;   "changes": {"base": SHA, "head": SHA,
;;               "links": [{"id": ID, "path": PATH, "from": N, "to": M}],
;;               "rules": [{"in": [PATH...]} | {"matching": REGEXP}]}
;;
;; ID hashes a hunk of `git diff -U0' (its path, the lines it removes and
;; adds); FROM-TO is where it sits in the base, which never changes.  Both
;; ids and places come from git (`bndos-task drafts hunks'), never from what
;; an agent writes.
;;
;; What is shown is computed again from git each time, from the base to the
;; worktree as checked out, and each link is one of:
;;   as linked        that change, there;
;;   changed since    the code there now, changed since it was linked
;;                    (committed or not);
;;   gone             nothing changed there now: the code is back as the
;;                    reviewer saw it.
;; Changes no reply links are "unexplained"; those the pull request's base
;; branch brought (after a rebase) are left out of them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'diff-review)

;;;; Hunks

(defun bndos-review-changes--git (root &rest args)
  "ARGS' output in ROOT, decoded as UTF-8, or nil when git fails."
  (let ((default-directory (file-name-as-directory root))
        (coding-system-for-read 'utf-8))
    (with-temp-buffer
      (when (zerop (apply #'process-file "git" nil '(t nil) nil
                          "--no-optional-locks" "-c" "core.quotePath=false" args))
        (buffer-string)))))

(defun bndos-review-changes-id (path minus plus)
  "The id of the hunk of PATH removing MINUS and adding PLUS, lists of lines."
  (substring (secure-hash 'sha256 (encode-coding-string
                                   (concat path "\0" (string-join minus "\n") "\0" (string-join plus "\n"))
                                   'utf-8))
             0 16))

(defun bndos-review-changes--parse (out)
  "The hunks of OUT, a `git diff -U0' without renames, oldest line first.
Each keeps its file's header (:header) and its own lines (:text) as git
wrote them, so a patch of some of them is git's own text."
  (let (hunks path old-path hunk file-status header)
    (dolist (line (split-string out "\n"))
      (cond
       ((string-prefix-p "diff --git " line)
        (setq path nil old-path nil hunk nil file-status nil header (list line)))
       ;; A file's header: until its first hunk, so a removed "-- x" is a line.
       ((and header (not hunk) (not (string-prefix-p "@@ " line)))
        (push line header)
        (cond
         ((string-match "\\`\\(new\\|deleted\\) file mode \\([0-7]+\\)" line)
          (setq file-status (list (intern (match-string 1 line)) (match-string 2 line))))
         ((string-prefix-p "--- " line)
          (setq old-path (and (not (equal line "--- /dev/null")) (string-trim-right (substring line 6) "\t"))))
         ((string-prefix-p "+++ " line)
          (setq path (if (equal line "+++ /dev/null") old-path (string-trim-right (substring line 6) "\t"))))))
       ((string-match "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" line)
        (setq hunk (list :path (or path old-path)
                         :old (string-to-number (match-string 1 line))
                         :old-len (if (match-string 2 line) (string-to-number (match-string 2 line)) 1)
                         :new (string-to-number (match-string 3 line))
                         :new-len (if (match-string 4 line) (string-to-number (match-string 4 line)) 1)
                         :minus nil :plus nil :file file-status
                         :header (reverse header) :text (list line)))
        (push hunk hunks))
       ((and hunk (string-prefix-p "-" line))
        (plist-put hunk :minus (cons (substring line 1) (plist-get hunk :minus)))
        (plist-put hunk :text (cons line (plist-get hunk :text))))
       ((and hunk (string-prefix-p "+" line))
        (plist-put hunk :plus (cons (substring line 1) (plist-get hunk :plus)))
        (plist-put hunk :text (cons line (plist-get hunk :text))))
       ((and hunk (string-prefix-p "\\" line))   ; no newline at the end
        (plist-put hunk :text (cons line (plist-get hunk :text))))))
    (mapcar (lambda (h)
              (plist-put h :minus (nreverse (plist-get h :minus)))
              (plist-put h :plus (nreverse (plist-get h :plus)))
              (plist-put h :text (nreverse (plist-get h :text)))
              (plist-put h :id (bndos-review-changes-id (plist-get h :path) (plist-get h :minus) (plist-get h :plus))))
            (nreverse hunks))))

(defun bndos-review-changes-hunks (root base &optional target)
  "The hunks from BASE to TARGET, a commit, or to ROOT's worktree; nil when
git can't tell (BASE missing, say)."
  (when-let* ((out (apply #'bndos-review-changes--git root "diff" "-U0" "--no-color" "--no-ext-diff"
                          "--no-renames" "--src-prefix=a/" "--dst-prefix=b/" base
                          (append (and target (list target)) '("--")))))
    (bndos-review-changes--parse out)))

(defun bndos-review-changes-anchor (hunk)
  "Where HUNK sits in its base: (PATH FROM TO), a pure addition the empty
range after its line (TO = FROM - 1)."
  (let ((old (plist-get hunk :old)) (len (plist-get hunk :old-len)))
    (if (> len 0)
        (list (plist-get hunk :path) old (+ old len -1))
      (list (plist-get hunk :path) (1+ old) old))))

(defun bndos-review-changes--overlap-p (a b)
  "Whether anchors A and B touch: ranges, an empty one a point between lines."
  (and (equal (car a) (car b))
       (let ((a1 (nth 1 a)) (a2 (max (nth 1 a) (nth 2 a)))
             (b1 (nth 1 b)) (b2 (max (nth 1 b) (nth 2 b))))
         (if (or (< (nth 2 a) (nth 1 a)) (< (nth 2 b) (nth 1 b)))
             (and (<= a1 (1+ b2)) (<= b1 (1+ a2)))
           (and (<= a1 b2) (<= b1 a2))))))

(defun bndos-review-changes--link-anchor (link)
  (list (plist-get link :path) (plist-get link :from) (plist-get link :to)))

;;;; What a reply's links are now

(defun bndos-review-changes-view (root changes)
  "CHANGES, a draft's, against ROOT's worktree: (:hunks ((HUNK . STATUS)...)
:gone (LINK...) :now HUNKS), or nil when its base isn't here.  STATUS is
`linked', `changed' or `changed-uncommitted'."
  (let* ((base (plist-get changes :base))
         (now (and base (bndos-review-changes-hunks root base)))
         (committed (and now (bndos-review-changes-hunks root base "HEAD"))))
    (when now
      (let (found gone)
        (dolist (link (plist-get changes :links))
          (let* ((anchor (bndos-review-changes--link-anchor link))
                 (same (seq-filter (lambda (h) (and (equal (plist-get h :id) (plist-get link :id))
                                                    (equal (bndos-review-changes-anchor h) anchor)))
                                   now)))
            (if same
                (dolist (h same) (setf (alist-get h found nil nil #'eq) 'linked))
              (let ((there (seq-filter (lambda (h) (bndos-review-changes--overlap-p (bndos-review-changes-anchor h) anchor))
                                       now)))
                (if (null there)
                    (push link gone)
                  (dolist (h there)
                    (unless (alist-get h found nil nil #'eq)
                      (setf (alist-get h found nil nil #'eq)
                            (if (seq-find (lambda (c) (and (equal (plist-get c :id) (plist-get h :id))
                                                           (equal (bndos-review-changes-anchor c) (bndos-review-changes-anchor h))))
                                          committed)
                                'changed
                              'changed-uncommitted)))))))))
        (list :hunks (sort (nreverse found)
                           (lambda (a b) (let ((pa (plist-get (car a) :path)) (pb (plist-get (car b) :path)))
                                           (if (equal pa pb) (< (plist-get (car a) :new) (plist-get (car b) :new))
                                             (string< pa pb)))))
              :gone (nreverse gone) :now now)))))

(defun bndos-review-changes-summary (view)
  "VIEW in a few words, and the warnings it calls for: (TEXT . WARNINGS)."
  (let* ((hunks (plist-get view :hunks))
         (files (length (seq-uniq (mapcar (lambda (h) (plist-get (car h) :path)) hunks))))
         (count (lambda (status) (seq-count (lambda (h) (eq (cdr h) status)) hunks)))
         warnings)
    (when (plist-get view :gone)
      (push (format "%d linked change%s gone: the code is back as the reviewer saw it"
                    (length (plist-get view :gone)) (if (cdr (plist-get view :gone)) "s" ""))
            warnings))
    (when (> (funcall count 'changed) 0)
      (push (format "%d linked change%s changed since linked" (funcall count 'changed)
                    (if (> (funcall count 'changed) 1) "s" ""))
            warnings))
    (when (> (funcall count 'changed-uncommitted) 0)
      (push (format "%d linked change%s changed in the worktree, not committed" (funcall count 'changed-uncommitted)
                    (if (> (funcall count 'changed-uncommitted) 1) "s" ""))
            warnings))
    (cons (format "%d change%s in %d file%s" (length hunks) (if (= (length hunks) 1) "" "s")
                  files (if (= files 1) "" "s"))
          (nreverse warnings))))

(defun bndos-review-changes-unexplained (root base drafts-changes &optional upstream)
  "The hunks from BASE to ROOT's worktree that no DRAFTS-CHANGES links, nor
UPSTREAM brought: (from to), the base branch's merge bases before and now."
  (let* ((now (bndos-review-changes-hunks root base))
         (anchors (cl-loop for changes in drafts-changes
                           append (mapcar #'bndos-review-changes--link-anchor (plist-get changes :links))))
         (brought (and upstream (not (equal (car upstream) (cadr upstream)))
                       (mapcar (lambda (h) (plist-get h :id))
                               (bndos-review-changes-hunks root (car upstream) (cadr upstream))))))
    (seq-remove (lambda (h) (or (member (plist-get h :id) brought)
                                (seq-some (lambda (a) (bndos-review-changes--overlap-p (bndos-review-changes-anchor h) a))
                                          anchors)))
                now)))

;;;; As trees: the worktree, and the worktree without some changes

(defun bndos-review-changes--git-error (root &rest args)
  "Nil when git ARGS succeed in ROOT, else what git said."
  (let ((default-directory (file-name-as-directory root)))
    (with-temp-buffer
      (if (zerop (apply #'process-file "git" nil t nil "--no-optional-locks" args))
          nil
        (let ((text (string-trim (buffer-string))))
          (if (string-empty-p text) (format "git %s failed" (car args)) text))))))

(defun bndos-review-changes-patch (hunks)
  "A patch of HUNKS alone, git's own text: each file's header, then its hunks."
  (mapconcat
   (lambda (path)
     (let ((hs (sort (seq-filter (lambda (h) (equal (plist-get h :path) path)) hunks)
                     (lambda (a b) (< (plist-get a :new) (plist-get b :new))))))
       (concat (string-join (plist-get (car hs) :header) "\n") "\n"
               (mapconcat (lambda (h) (concat (string-join (plist-get h :text) "\n") "\n")) hs ""))))
   (seq-uniq (mapcar (lambda (h) (plist-get h :path)) hunks))
   ""))

(defun bndos-review-changes-trees (root hunks)
  "Trees of ROOT's worktree as checked out, its tracked files, and of the
same with HUNKS undone: (:now TREE :before TREE), or (:error MESSAGE).
From :before to :now is HUNKS, and nothing else: checked, so a review of
them shows what git has, never something like it."
  (let ((index (make-temp-file "bndos-changes-index-"))
        (patch (make-temp-file "bndos-changes-" nil ".patch")))
    (unwind-protect
        (let ((real (string-trim (or (bndos-review-changes--git root "rev-parse" "--path-format=absolute"
                                                                "--git-path" "index")
                                     ""))))
          ;; A copy of the index, its time kept, so git sees what changed.
          (if (and (not (string-empty-p real)) (file-exists-p real))
              (copy-file real index t t)
            (delete-file index))
          (let* ((process-environment (cons (concat "GIT_INDEX_FILE=" index) process-environment))
                 (tree (lambda () (string-trim (or (bndos-review-changes--git root "write-tree") ""))))
                 (failed (bndos-review-changes--git-error root "add" "-u" "--" "."))
                 (now (and (not failed) (funcall tree))))
            (cond
             ((or failed (string-empty-p now)) (list :error (or failed "couldn't read the worktree")))
             ((null hunks) (list :now now :before now))
             (t
              (let ((coding-system-for-write 'utf-8-unix))
                (with-temp-file patch (insert (bndos-review-changes-patch hunks))))
              (if-let* ((failed (bndos-review-changes--git-error root "apply" "--cached" "-R" "--unidiff-zero"
                                                                 "--whitespace=nowarn" patch)))
                  (list :error (format "couldn't undo the changes: %s" failed))
                (let* ((before (funcall tree))
                       (between (bndos-review-changes-hunks root before now))
                       (key (lambda (h) (list (plist-get h :id) (plist-get h :path)))))
                  (if (equal (sort (mapcar (lambda (h) (format "%S" (funcall key h))) between) #'string<)
                             (sort (mapcar (lambda (h) (format "%S" (funcall key h))) hunks) #'string<))
                      (list :now now :before before)
                    (list :error "the changes didn't come out as linked: refresh"))))))))
      (ignore-errors (delete-file index))
      (ignore-errors (delete-file patch)))))

;;;; Cached, for drawing

(defvar bndos-review-changes-cache-seconds 5
  "Seconds a view is reused for drawing; a send computes it afresh.")

(defvar bndos-review-changes--cache (make-hash-table :test #'equal)
  "(ROOT . CHANGES) -> (TIME . VIEW).")

(defun bndos-review-changes-view-cached (root changes)
  "`bndos-review-changes-view', computed at most every few seconds."
  (let* ((key (cons root changes))
         (hit (gethash key bndos-review-changes--cache)))
    (if (and hit (< (float-time (time-since (car hit))) bndos-review-changes-cache-seconds))
        (cdr hit)
      (let ((view (bndos-review-changes-view root changes)))
        (puthash key (cons (current-time) view) bndos-review-changes--cache)
        view))))

(defun bndos-review-changes-head (root)
  "ROOT's HEAD, a sha, or nil."
  (when-let* ((out (bndos-review-changes--git root "rev-parse" "HEAD"))) (string-trim out)))

(defun bndos-review-changes-dirty-p (root)
  "Whether ROOT has changes not committed, untracked files aside."
  (let ((out (bndos-review-changes--git root "status" "--porcelain" "--untracked-files=no")))
    (and out (not (string-empty-p (string-trim out))))))

(defun bndos-review-changes-count (changes)
  "How many changes CHANGES links, in how many files: a short text."
  (let* ((links (plist-get changes :links))
         (files (length (seq-uniq (mapcar (lambda (l) (plist-get l :path)) links)))))
    (format "%d change%s in %d file%s" (length links) (if (= (length links) 1) "" "s")
            files (if (= files 1) "" "s"))))

;;;; The review of some changes, in a workspace of its own

;; A review like any other (`diff-review'), of the worktree as checked
;; out, against a tree of it with the changes shown undone: its files are
;; the real ones, and what it marks in them is those changes alone.  One
;; review file per pull request, rewritten for each reply shown, and one
;; workspace, reused; the pull request's own review is left as it is.

(defface bndos-review-changes-heading '((t :inherit bold))
  "The heading of a review of some changes."
  :group 'diff-review)

(defvar-local bndos-review-changes-scope nil
  "In a review of some changes, what they are: a plist
  :compute  a function giving (:hunks HUNKS :notes ((TEXT . FACE)...)),
            called on every scan, so the review follows the worktree;
  :name     how the overview names the changes.")
(put 'bndos-review-changes-scope 'permanent-local t)

(defvar bndos-review-changes--next-scope nil
  "The scope of the review being opened, before it has a buffer.")

(defvar-local bndos-review-changes--last nil "The last scan's :compute result.")
(put 'bndos-review-changes--last 'permanent-local t)

(defvar-local bndos-review-changes--from nil "The workspace a view was opened from.")
(put 'bndos-review-changes--from 'permanent-local t)

(defun bndos-review-changes--range ()
  "The range of a review of some changes (`diff-review-range-function'):
from the worktree with them undone, to the worktree; nil for any other."
  (when-let* ((scope (or bndos-review-changes--next-scope bndos-review-changes-scope)))
    (setq bndos-review-changes-scope scope)
    (let* ((got (funcall (plist-get scope :compute)))
           (trees (bndos-review-changes-trees diff-review--root (plist-get got :hunks))))
      (setq bndos-review-changes--last got)
      (when (plist-get trees :error)
        (setq bndos-review-changes--last
              (plist-put (copy-sequence got) :notes
                         (append (plist-get got :notes) (list (cons (plist-get trees :error) 'error)))))
        (setq trees (let ((now (plist-get (bndos-review-changes-trees diff-review--root nil) :now)))
                      (list :now now :before now))))
      (list :base (plist-get trees :before) :worktree-tree (plist-get trees :now)
            :base-name (format "without %s" (plist-get scope :name)) :head-name "worktree"))))

(defun bndos-review-changes--install ()
  "Make reviews of some changes take their range, before any other caller's."
  (cond ((null diff-review-range-function)
         (setq diff-review-range-function #'bndos-review-changes--range))
        ((eq diff-review-range-function #'bndos-review-changes--range))
        (t (add-function :before-until diff-review-range-function #'bndos-review-changes--range))))

(bndos-review-changes--install)
;; The tasks' reviews set theirs when loaded.
(with-eval-after-load 'bndos-task-review (bndos-review-changes--install))

(defun bndos-review-changes--overview ()
  "What a review of some changes shows atop it (`diff-review-overview-functions')."
  (when bndos-review-changes-scope
    (dolist (note (plist-get bndos-review-changes--last :notes))
      (insert (propertize (car note) 'face (cdr note)) "\n"))
    (insert (propertize (substitute-command-keys
                         "\\<diff-review-mode-map>\\[diff-review-quit] back to the review this came from\n")
                        'face 'shadow)
            "\n")))

(add-hook 'diff-review-overview-functions #'bndos-review-changes--overview -50)

(declare-function +workspace-current-name "autoload/workspaces")
(declare-function +workspace-exists-p "autoload/workspaces")
(declare-function +workspace-switch "autoload/workspaces")

(defun bndos-review-changes--workspaces-p ()
  (and (bound-and-true-p persp-mode) (fboundp '+workspace-switch)))

(defun bndos-review-changes-open (name root file scope &optional setup)
  "Review SCOPE's changes of ROOT's worktree, from review FILE, in the
workspace NAME: one, reused.  The review it is opened from is left as is.
SETUP, a function, is then called in the review."
  (let* ((from (and (bndos-review-changes--workspaces-p) (+workspace-current-name)))
         (bndos-review-changes--next-scope scope)
         (buffer (find-buffer-visiting file)))
    ;; Afresh for each: no file marked viewed for another reply's changes.
    (when buffer (with-current-buffer buffer (set-buffer-modified-p nil)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (json-serialize `((format . ,diff-review-format)
                                (root . ,(file-name-as-directory root))
                                (viewed . ,(make-hash-table))))
              "\n"))
    (when (and from (not (equal from name))) (+workspace-switch name t))
    (if (and buffer (with-current-buffer buffer (derived-mode-p 'diff-review-mode)))
        (with-current-buffer buffer (revert-buffer t t))
      (setq buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (setq bndos-review-changes-scope scope)
      (when (and from (not (equal from name))) (setq bndos-review-changes--from from))
      (when setup (funcall setup)))
    ;; A new workspace has one window; one reused, the layout left in it.
    (switch-to-buffer buffer)
    buffer))

(defun bndos-review-changes--quit (quit &rest args)
  "Leave a review of some changes for the workspace it was opened from;
any other review, QUIT with ARGS."
  (let ((review (ignore-errors (diff-review--in-review (current-buffer)))))
    (if-let* ((from (and review (buffer-local-value 'bndos-review-changes-scope review)
                         (buffer-local-value 'bndos-review-changes--from review)))
              ((bndos-review-changes--workspaces-p))
              ((+workspace-exists-p from)))
        (+workspace-switch from)
      (apply quit args))))

(advice-add 'diff-review-quit :around #'bndos-review-changes--quit)

(provide 'bndos-review-changes)
;;; bndos-review-changes.el ends here
