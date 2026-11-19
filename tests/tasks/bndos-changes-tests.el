;;; bndos-changes-tests.el --- The changes a reply links, from git -*- lexical-binding: t; -*-

;; A reply's changes are hunks named by an id and a place in the commit
;; commented on; what is shown is computed from git, against the worktree
;; as checked out.  Here: a review, a fix of several threads in one commit,
;; a later commit that undoes one fix and rewrites another, an edit not
;; committed, a rebase; and each view a true diff of the worktree.

(require 'test-helper)
(require 'bndos-review-changes)
(require 'bndos-task-agent)                 ; bndos-task-cli

(defun bndos-test-lines (n fn)
  (mapconcat (lambda (i) (funcall fn i)) (number-sequence 1 n) "\n"))

(defmacro bndos-test-with-fixes (spec &rest body)
  "SPEC is (DIR BASE FIX): a repository whose BASE was reviewed and FIX
addresses the review in one commit."
  (declare (indent 1))
  `(bndos-test-with-repo ,(nth 0 spec)
     (bndos-test-commit ,(nth 0 spec) "reviewed"
                        "src/store.py" (concat "import json\n"
                                               (bndos-test-lines 20 (lambda (i) (format "# store %d" i)))
                                               "\ndef load_cfg(path):\n    return open(path).read()\n\n"
                                               "def save_cfg(path, data):\n    open(path, 'w').write(data)\n\n"
                                               (bndos-test-lines 20 (lambda (i) (format "# tail %d" i))) "\n")
                        "src/api.py" (concat "from store import load_cfg\n"
                                             (bndos-test-lines 30 (lambda (i) (format "# api %d" i)))
                                             "\ndef handler(req):\n    cfg = load_cfg(req.path)\n    return cfg\n"
                                             (bndos-test-lines 10 (lambda (i) (format "# api tail %d" i))) "\n")
                        "src/twice.py" (concat "def a():\n    x = 1\n"
                                               (bndos-test-lines 12 (lambda (i) (format "# pad %d" i)))
                                               "\ndef b():\n    x = 1\n")
                        "docs/gone.md" "Old notes.\n")
     (let ((,(nth 1 spec) (bndos-test-git ,(nth 0 spec) "rev-parse" "HEAD")))
       (bndos-test-commit ,(nth 0 spec) "address review"
                          "src/store.py" (concat "import json\nimport os\n"
                                                 (bndos-test-lines 20 (lambda (i) (format "# store %d" i)))
                                                 "\ndef load_config(path):\n    return open(path).read()\n\n"
                                                 "def save_cfg(path, data):\n    tmp = path + '.tmp'\n"
                                                 "    open(tmp, 'w').write(data)\n    os.replace(tmp, path)\n\n"
                                                 (bndos-test-lines 20 (lambda (i) (format "# tail %d" i))) "\n")
                          "src/api.py" (concat "from store import load_config\n"
                                               (bndos-test-lines 30 (lambda (i) (format "# api %d" i)))
                                               "\ndef handler(req):\n    try:\n        cfg = load_config(req.path)\n"
                                               "    except FileNotFoundError:\n        return None\n    return cfg\n"
                                               (bndos-test-lines 10 (lambda (i) (format "# api tail %d" i))) "\n")
                          "src/twice.py" (concat "def a():\n    x = 2\n"
                                                 (bndos-test-lines 12 (lambda (i) (format "# pad %d" i)))
                                                 "\ndef b():\n    x = 2\n")
                          "tests/test_store.py" "def test_load():\n    assert load_config\n")
       (bndos-test-git ,(nth 0 spec) "rm" "-q" "docs/gone.md")
       (bndos-test-git ,(nth 0 spec) "commit" "-q" "--amend" "--no-edit")
       (let ((,(nth 2 spec) (bndos-test-git ,(nth 0 spec) "rev-parse" "HEAD")))
         ,@body))))

(defun bndos-test-link (hunks pred)
  "Links to the HUNKS PRED takes, as `bndos-task drafts add' keeps them."
  (mapcar (lambda (h) (let ((a (bndos-review-changes-anchor h)))
                        (list :id (plist-get h :id) :path (nth 0 a) :from (nth 1 a) :to (nth 2 a))))
          (seq-filter pred hunks)))

(defun bndos-test-at (path line) (lambda (h) (and (equal (plist-get h :path) path) (= (plist-get h :new) line))))

(defun bndos-test-statuses (view)
  (mapcar (lambda (x) (list (plist-get (car x) :path) (plist-get (car x) :new) (cdr x))) (plist-get view :hunks)))

(defun bndos-test-true-diff-p (dir text)
  "Whether TEXT, a diff, applies backwards to DIR's worktree as it is: what
it shows is what the files have."
  (let ((patch (make-temp-file "bndos-diff-" nil ".patch")))
    (unwind-protect
        (progn (with-temp-file patch (insert text))
               (let ((default-directory (file-name-as-directory dir)))
                 (zerop (process-file "git" nil nil nil "apply" "-R" "--check" "--unidiff-zero" patch))))
      (delete-file patch))))

(defun bndos-test-shown (dir hunks)
  "What a review of HUNKS shows: git's diff between its two trees, which
must be HUNKS and nothing else, the newer the worktree as it is."
  (let ((trees (bndos-review-changes-trees dir hunks)))
    (should-not (plist-get trees :error))
    (should (equal (sort (mapcar (lambda (h) (plist-get h :id))
                                 (bndos-review-changes-hunks dir (plist-get trees :before) (plist-get trees :now)))
                         #'string<)
                   (sort (mapcar (lambda (h) (plist-get h :id)) hunks) #'string<)))
    (let ((text (with-temp-buffer
                  (let ((default-directory (file-name-as-directory dir)))
                    (process-file "git" nil t nil "diff" (plist-get trees :before) (plist-get trees :now)))
                  (buffer-string))))
      (should (or (null hunks) (bndos-test-true-diff-p dir text)))
      text)))

(defun bndos-test-show (dir view)
  (bndos-test-shown dir (mapcar #'car (plist-get view :hunks))))


;;; Hunks, as Emacs and bndos-task see them

(ert-deftest bndos-changes-emacs-and-the-cli-name-hunks-alike ()
  (bndos-test-with-fixes (dir base fix)
    (let* ((emacs (mapcar (lambda (h) (cons (plist-get h :id) (bndos-review-changes-anchor h)))
                          (bndos-review-changes-hunks dir base fix)))
           (cli (with-temp-buffer
                  (call-process "python3" nil t nil "-c"
                                (concat "import json, sys\nfrom importlib.machinery import SourceFileLoader\n"
                                        "m = SourceFileLoader('bt', sys.argv[1]).load_module()\n"
                                        "print(json.dumps([[h['id'], h['path'], h['from'], h['to']]"
                                        " for h in m.diff_hunks(sys.argv[2], sys.argv[3], sys.argv[4])]))")
                                bndos-task-cli dir base fix)
                  (goto-char (point-min))
                  (mapcar (lambda (x) (cons (aref x 0) (list (aref x 1) (aref x 2) (aref x 3))))
                          (json-parse-buffer :array-type 'array)))))
      (should (> (length emacs) 6))
      (should (equal emacs cli))
      ;; A deleted file and a new one are hunks too.
      (should (assoc-default "docs/gone.md" (mapcar (lambda (x) (cons (cadr x) x)) emacs)))
      (should (assoc-default "tests/test_store.py" (mapcar (lambda (x) (cons (cadr x) x)) emacs))))))

(ert-deftest bndos-changes-an-unknown-base-is-no-view ()
  (bndos-test-with-fixes (dir base fix)
    (should-not (bndos-review-changes-view dir (list :base "0123456789abcdef0123456789abcdef01234567" :links nil)))))


;;; What a link is now

(ert-deftest bndos-changes-as-linked-then-changed-gone-and-uncommitted ()
  (bndos-test-with-fixes (dir base fix)
    (let* ((hunks (bndos-review-changes-hunks dir base fix))
           (atomic (list :base base :head fix
                         :links (bndos-test-link hunks (lambda (h) (and (equal (plist-get h :path) "src/store.py")
                                                                        (member "    os.replace(tmp, path)" (plist-get h :plus)))))))
           (handler (list :base base :head fix
                          :links (bndos-test-link hunks (lambda (h) (and (equal (plist-get h :path) "src/api.py")
                                                                         (member "    try:" (plist-get h :plus))))))))
      (should (equal (mapcar #'caddr (bndos-test-statuses (bndos-review-changes-view dir atomic))) '(linked)))
      (should (equal (mapcar #'caddr (bndos-test-statuses (bndos-review-changes-view dir handler))) '(linked)))
      ;; Later: the atomic save undone, the handler written another way.
      (let ((store (with-temp-buffer (insert-file-contents (expand-file-name "src/store.py" dir)) (buffer-string)))
            (api (with-temp-buffer (insert-file-contents (expand-file-name "src/api.py" dir)) (buffer-string))))
        (bndos-test-commit dir "simplify"
                           "src/store.py" (replace-regexp-in-string
                                           "    tmp = path \\+ '.tmp'\n    open(tmp, 'w').write(data)\n    os.replace(tmp, path)\n"
                                           "    open(path, 'w').write(data)\n" store)
                           "src/api.py" (replace-regexp-in-string
                                         "    try:\n        cfg = load_config(req.path)\n    except FileNotFoundError:\n        return None\n    return cfg\n"
                                         "    if not req.path:\n        return None\n    return load_config(req.path)\n" api)))
      (let ((view (bndos-review-changes-view dir atomic)))
        (should-not (plist-get view :hunks))
        (should (= (length (plist-get view :gone)) 1))
        (should (string-match-p "gone: the code is back as the reviewer saw it"
                                (car (cdr (bndos-review-changes-summary view))))))
      (let ((view (bndos-review-changes-view dir handler)))
        (should (equal (mapcar #'caddr (bndos-test-statuses view)) '(changed)))
        (should (string-match-p "if not req.path" (bndos-test-show dir view)))
        (should (bndos-test-true-diff-p dir (bndos-test-show dir view))))
      ;; An edit there, not committed.
      (let ((api (with-temp-buffer (insert-file-contents (expand-file-name "src/api.py" dir)) (buffer-string))))
        (bndos-test-write dir "src/api.py" (replace-regexp-in-string "return None" "return {}" api)))
      (let ((view (bndos-review-changes-view dir handler)))
        (should (equal (mapcar #'caddr (bndos-test-statuses view)) '(changed-uncommitted)))
        (should (string-match-p "not committed" (string-join (cdr (bndos-review-changes-summary view)) " ")))
        (should (string-match-p "return {}" (bndos-test-show dir view)))
        (should (bndos-test-true-diff-p dir (bndos-test-show dir view)))))))

(ert-deftest bndos-changes-the-same-change-twice-is-told-by-its-place ()
  (bndos-test-with-fixes (dir base fix)
    (let* ((hunks (bndos-review-changes-hunks dir base fix))
           (twice (seq-filter (lambda (h) (equal (plist-get h :path) "src/twice.py")) hunks)))
      (should (= (length twice) 2))
      (should (equal (plist-get (car twice) :id) (plist-get (cadr twice) :id)))
      ;; A reply about a() links the first: it shows a(), not b().
      (let ((view (bndos-review-changes-view dir (list :base base :head fix
                                                        :links (bndos-test-link (list (car twice)) #'always)))))
        (should (equal (bndos-test-statuses view) '(("src/twice.py" 2 linked))))))))

(ert-deftest bndos-changes-what-no-reply-explains ()
  (bndos-test-with-fixes (dir base fix)
    (let* ((hunks (bndos-review-changes-hunks dir base fix))
           (rename (list :base base :head fix
                         :links (bndos-test-link hunks (lambda (h) (seq-some (lambda (l) (string-match-p "load_c" l))
                                                                             (append (plist-get h :minus) (plist-get h :plus))))))))
      (bndos-test-write dir "src/store.py"
                        (concat (with-temp-buffer (insert-file-contents (expand-file-name "src/store.py" dir)) (buffer-string))
                                "print('debug')\n"))
      (let ((loose (bndos-review-changes-unexplained dir base (list rename))))
        (should (seq-find (lambda (h) (member "print('debug')" (plist-get h :plus))) loose))
        (should (seq-find (lambda (h) (equal (plist-get h :path) "src/twice.py")) loose))
        (should-not (seq-find (lambda (h) (equal (plist-get h :path) "src/api.py")) loose))))))

(ert-deftest bndos-changes-a-rebase-keeps-links-and-leaves-the-base-branch-out ()
  (bndos-test-with-fixes (dir base fix)
    (let* ((hunks (bndos-review-changes-hunks dir base fix))
           (changes (list :base base :head fix
                          :links (bndos-test-link hunks (bndos-test-at "src/twice.py" 2)))))
      ;; main moves on from the reviewed commit; the branch is rebased onto it.
      (bndos-test-git dir "branch" "-q" "pr")
      (bndos-test-git dir "reset" "-q" "--hard" base)
      (bndos-test-commit dir "main: other work" "docs/main.md" "From main.\n")
      (let ((main (bndos-test-git dir "rev-parse" "HEAD")))
        (bndos-test-git dir "checkout" "-q" "pr")
        (bndos-test-git dir "rebase" "-q" main)
        (should (equal (mapcar #'caddr (bndos-test-statuses (bndos-review-changes-view dir changes))) '(linked)))
        (let ((all (bndos-review-changes-unexplained dir base (list changes)))
              (pr (bndos-review-changes-unexplained dir base (list changes)
                                                    (list (bndos-test-git dir "merge-base" base main)
                                                          (bndos-test-git dir "merge-base" "HEAD" main)))))
          (should (seq-find (lambda (h) (equal (plist-get h :path) "docs/main.md")) all))
          (should-not (seq-find (lambda (h) (equal (plist-get h :path) "docs/main.md")) pr))
          (should (seq-find (lambda (h) (equal (plist-get h :path) "src/store.py")) pr)))))))


;;; Shown: a review from the worktree with the changes undone, to the worktree

(ert-deftest bndos-changes-any-changes-undo-to-exactly-them ()
  (bndos-test-with-fixes (dir base fix)
    (let* ((now (bndos-review-changes-hunks dir base)))
      ;; Each hunk alone, every pair, then all: a new file and a deleted
      ;; one included, those before a hunk left as they are.
      (dolist (h now) (bndos-test-shown dir (list h)))
      (cl-loop for (a . rest) on now do (dolist (b rest) (bndos-test-shown dir (list a b))))
      (bndos-test-shown dir now)
      (should (equal (plist-get (bndos-review-changes-trees dir nil) :now)
                     (plist-get (bndos-review-changes-trees dir nil) :before))))))

(ert-deftest bndos-changes-undone-as-git-wrote-them ()
  "Lines that look like a header, no newline at the end, unicode, a file
deleted from the worktree but not committed: each undone alone."
  (bndos-test-with-repo dir
    (bndos-test-commit dir "base"
                       "notes.md" (concat "title\n-- signed\n" (bndos-test-lines 8 (lambda (i) (format "l%d" i))) "\nend")
                       "café.txt" "un\ndeux\ntrois\n"
                       "old.txt" "old\n")
    (let ((base (bndos-test-git dir "rev-parse" "HEAD")))
      (bndos-test-write dir "notes.md" (concat "title\n++ added\n" (bndos-test-lines 8 (lambda (i) (format "l%d" i)))
                                               "\nl9\nend!"))
      (bndos-test-write dir "café.txt" "un\ndeux ✓\ntrois\n")
      (delete-file (expand-file-name "old.txt" dir))
      (let ((now (bndos-review-changes-hunks dir base)))
        (should (seq-find (lambda (h) (member "-- signed" (plist-get h :minus))) now))
        (should (seq-find (lambda (h) (member "++ added" (plist-get h :plus))) now))
        (should (= (length now) 4))
        (dolist (h now) (bndos-test-shown dir (list h)))
        (cl-loop for (a . rest) on now do (dolist (b rest) (bndos-test-shown dir (list a b))))
        (bndos-test-shown dir now)))))

(ert-deftest bndos-changes-a-review-shows-only-what-it-links ()
  (bndos-test-with-fixes (dir base fix)
    (let* ((now (bndos-review-changes-hunks dir base))
           (one (seq-filter (lambda (h) (member "    try:" (plist-get h :plus))) now))
           (text (bndos-test-shown dir one)))
      (should (= (length one) 1))
      (should (string-match-p "FileNotFoundError" text))
      (should-not (string-match-p "src/store.py\\|twice\\|test_store" text)))))

(provide 'bndos-changes-tests)
;;; bndos-changes-tests.el ends here
