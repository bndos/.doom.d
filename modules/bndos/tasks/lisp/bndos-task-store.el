;;; bndos-task-store.el --- Durable task/review state -*- lexical-binding: t; -*-

;;; Commentary:

;; Task records live outside the project, one JSON file per task under
;; `bndos-task-store-directory' (default $XDG_STATE_HOME/bndos-tasks).  JSON
;; is parsed, never evaluated.  A task is keyed by the canonical identity of
;; its repository (truename of the git common dir) and worktree (truename of
;; its toplevel), so equal branch or handle names in different projects never
;; collide.
;;
;; In memory a record is a plist with keyword keys; arrays are lists; every
;; enumeration (states, modes, owners) is a string.  `nil' round-trips as
;; JSON null.
;;
;; Layout:
;;   tasks/<key>.json      task identity, agent session, threads, requests
;;                         (bin/bndos-task status reads the review state)
;;   logs/<worktree>.log   Hydra shim output from bin/bndos-task
;;   projects.json         repositories shown in the dashboard

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defgroup bndos-task nil
  "Parallel coding tasks: worktrees, agents and reviews."
  :group 'tools
  :prefix "bndos-task-")

(defcustom bndos-task-store-directory
  (expand-file-name "bndos-tasks"
                    (or (getenv "XDG_STATE_HOME") (expand-file-name "~/.local/state")))
  "Directory holding task records and related state."
  :type 'directory)

(defconst bndos-task-store-version 1
  "Record format version written by this code.")

(defvar bndos-task-store--cache (make-hash-table :test #'equal)
  "KEY -> (MTIME . RECORD) for records read from disk.")


;;; Identity and small helpers

(defun bndos-task-key (common-dir worktree)
  "Return the task key for repository COMMON-DIR and WORKTREE directory."
  (substring (secure-hash 'sha1 (concat (directory-file-name (file-truename common-dir))
                                        "\n"
                                        (directory-file-name (file-truename worktree))))
             0 16))

(defun bndos-task-now ()
  "Current time as an ISO 8601 string."
  (format-time-string "%FT%T%z"))

(defun bndos-task-new-id (prefix)
  "Return a fresh random identifier starting with PREFIX."
  (format "%s-%s" prefix (substring (secure-hash 'sha1 (format "%s%s%s%s" (random) (float-time)
                                                               (emacs-pid) (system-name)))
                                    0 10)))

(defun bndos-task-get (record &rest path)
  "Return the value at keyword PATH inside nested plist RECORD."
  (let ((value record))
    (dolist (key path value)
      (setq value (plist-get value key)))))

(defun bndos-task-put (record path value)
  "Return RECORD with the value at keyword list PATH set to VALUE."
  (if (null (cdr path))
      (plist-put (copy-sequence record) (car path) value)
    (plist-put (copy-sequence record) (car path)
               (bndos-task-put (plist-get record (car path)) (cdr path) value))))


;;; JSON conversion

(defun bndos-task-store--plist-p (value)
  (and (consp value) (keywordp (car value))))

(defun bndos-task-store--to-json (value)
  "Convert in-memory VALUE to a form `json-serialize' accepts."
  (cond
   ((null value) :null)
   ((eq value t) t)
   ((bndos-task-store--plist-p value)
    (let (out)
      (while value
        (push (car value) out)
        (push (bndos-task-store--to-json (cadr value)) out)
        (setq value (cddr value)))
      (nreverse out)))
   ((consp value) (vconcat (mapcar #'bndos-task-store--to-json value)))
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   (t value)))

(defun bndos-task-store-encode (record)
  "Serialize RECORD to a (multibyte) JSON string."
  ;; `json-serialize' returns unibyte UTF-8; decode so it composes with text.
  (decode-coding-string (json-serialize (bndos-task-store--to-json record)) 'utf-8))

(defun bndos-task-store-decode (string)
  "Parse JSON STRING into the in-memory record format."
  (json-parse-string string :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))


;;; Files

(defun bndos-task-store--path (&rest parts)
  (apply #'file-name-concat bndos-task-store-directory parts))

(defun bndos-task-store-file (key)
  (bndos-task-store--path "tasks" (concat key ".json")))

(defun bndos-task-store--write-atomically (file string)
  "Write STRING to FILE through a temporary file and rename."
  (make-directory (file-name-directory file) t)
  (let ((tmp (make-temp-file (expand-file-name ".tmp-" (file-name-directory file)))))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region string nil tmp nil 'silent))
    (rename-file tmp file t)))

(defun bndos-task-store--read-json-file (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (bndos-task-store-decode (buffer-string))))

(defun bndos-task-store--mtime (file)
  (and file (file-exists-p file) (file-attribute-modification-time (file-attributes file))))

(defun bndos-task-store-load (key)
  "Return the record for KEY, or nil if none exists.
Its :threads come from the task's review file (see `bndos-task-review-file').
Signal an error for unreadable files or newer record versions rather than
treating them as absent."
  (let ((file (bndos-task-store-file key)))
    (when (file-exists-p file)
      (let* ((cached (gethash key bndos-task-store--cache))
             (review (and cached (plist-get (cdr cached) :review-file)))
             (stamp (list (bndos-task-store--mtime file) (and review (bndos-task-store--mtime review)))))
        (if (and cached (equal (car cached) stamp))
            (cdr cached)
          (let ((record (condition-case err
                            (bndos-task-store--read-json-file file)
                          (error (error "Unreadable task record %s: %s"
                                        file (error-message-string err))))))
            (when (> (or (plist-get record :version) 0) bndos-task-store-version)
              (error "Task record %s has version %s; this code understands %s"
                     file (plist-get record :version) bndos-task-store-version))
            (let* ((review (bndos-task-review-file record))
                   (data (bndos-task-review-file-read review)))
              ;; Threads live in the review file; a record from before keeps its
              ;; own until the next save moves them there.
              (when data
                (setq record (plist-put record :threads
                                        (mapcar #'bndos-task-thread-from-comment
                                                (append (gethash "comments" data) nil)))))
              (setq record (plist-put record :review-file review))
              (puthash key (cons (list (bndos-task-store--mtime file) (bndos-task-store--mtime review)) record)
                       bndos-task-store--cache)
              record)))))))

(defun bndos-task-store-save (record)
  "Write RECORD (which must have :key) and return it with :updated refreshed.
Its threads go to the task's review file, the rest to the task record."
  (let* ((key (or (plist-get record :key) (error "Task record without :key")))
         (record (plist-put (plist-put (copy-sequence record) :version bndos-task-store-version)
                            :updated (bndos-task-now)))
         (file (bndos-task-store-file key))
         (review (bndos-task-review-file record)))
    (bndos-task-review-file-save-threads review record)
    ;; A task with no repository has no review file: its threads stay here.
    (bndos-task-store--write-atomically
     file (bndos-task-store-encode (bndos-task-store--without record (if review '(:threads :review-file)
                                                                       '(:review-file)))))
    (let ((record (plist-put record :review-file review)))
      (puthash key (cons (list (bndos-task-store--mtime file) (bndos-task-store--mtime review)) record)
               bndos-task-store--cache)
      record)))

(defun bndos-task-store--without (plist keys)
  (let (out)
    (while plist
      (unless (memq (car plist) keys)
        (setq out (cons (cadr plist) (cons (car plist) out))))
      (setq plist (cddr plist)))
    (nreverse out)))


;;; Threads in the review file
;;
;; A task's discussion is the comments of its diff-review review file,
;; <common git dir>/review/<branch>.json: diff-review shows and edits them,
;; and the agent flow adds its own fields under each comment's "bndos" key.

(defun bndos-task-review-file (record)
  "The review file of RECORD's task."
  (when-let* ((common (bndos-task-get record :repo :common-dir)))
    (expand-file-name (concat "review/" (or (plist-get record :branch)
                                            (concat "task-" (plist-get record :key)))
                              ".json")
                      common)))

(defun bndos-task-review-file-read (file)
  "FILE's data as diff-review parses it (hash tables), or nil."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
      (condition-case nil
          (json-parse-buffer :null-object nil :false-object nil)
        (error nil)))))

(defun bndos-task-review-file-new (record)
  "Fresh review file data for RECORD's task."
  (let ((data (make-hash-table :test #'equal)))
    (puthash "format" "diff-review/1" data)
    (when-let* ((root (bndos-task-get record :worktree :path)))
      (puthash "root" (file-name-as-directory root) data))
    (when-let* ((base (bndos-task-get record :base :ref))) (puthash "base" base data))
    (puthash "task" (plist-get record :key) data)
    (puthash "viewed" (make-hash-table :test #'equal) data)
    ;; A record from before the review file had its threads itself.
    (puthash "comments" (vconcat (mapcar #'bndos-task-thread-to-comment (plist-get record :threads))) data)
    data))

(declare-function diff-review--read "diff-review")

(defvar bndos-task-review-file--loading nil
  "The review buffer whose load is saving the task now, or nil.")

(defun bndos-task-review-file-write (file data)
  "Write DATA to review FILE as diff-review does; a buffer showing it rereads it."
  (make-directory (file-name-directory file) t)
  (let ((json (json-serialize data :null-object nil)))
    (with-temp-buffer
      (insert (decode-coding-string json 'utf-8))
      (json-pretty-print-buffer)
      (insert "\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) file nil 'silent))))
  (when-let* ((buffer (find-buffer-visiting file)))
    (with-current-buffer buffer
      (when (derived-mode-p 'diff-review-mode)
        (if (eq buffer bndos-task-review-file--loading)
            ;; Written while it loads (its scan saved the task): take the new
            ;; contents as its own and let the load go on, as its revert would.
            (let ((inhibit-read-only t))
              (set-visited-file-modtime)
              (insert-file-contents file nil nil nil t)
              (diff-review--read))
          (revert-buffer t t t))))))

(defun bndos-task-review-file-save-threads (file record)
  "Write RECORD's threads into review FILE when they changed there."
  (when file
    (let* ((existing (bndos-task-review-file-read file))
           (data (or existing (and (plist-get record :threads) (bndos-task-review-file-new record))))
           (old (and existing (append (gethash "comments" existing) nil))))
      (when data
        (let ((comments (vconcat
                         (mapcar (lambda (thread)
                                   (bndos-task-thread-to-comment
                                    thread (seq-find (lambda (c) (equal (gethash "id" c) (plist-get thread :id)))
                                                     old)))
                                 (plist-get record :threads)))))
          (unless (equal (json-serialize comments :null-object nil)
                         (json-serialize (vconcat old) :null-object nil))
            (puthash "comments" comments data)
            (bndos-task-review-file-write file data)))))))

(defun bndos-task--to-table (value)
  "VALUE, plists as hash tables and lists as vectors, for diff-review's JSON."
  (cond ((bndos-task-store--plist-p value)
         (let ((table (make-hash-table :test #'equal)))
           (while value
             (puthash (substring (symbol-name (car value)) 1) (bndos-task--to-table (cadr value)) table)
             (setq value (cddr value)))
           table))
        ((consp value) (vconcat (mapcar #'bndos-task--to-table value)))
        ((memq value '(nil t)) value)
        ((symbolp value) (symbol-name value))
        (t value)))

(defun bndos-task--from-table (value)
  "VALUE as parsed by diff-review, as a plist, lists for vectors."
  (cond ((hash-table-p value)
         (let (out)
           (maphash (lambda (k v) (setq out (cons (bndos-task--from-table v) (cons (intern (concat ":" k)) out))))
                    value)
           (nreverse out)))
        ((vectorp value) (mapcar #'bndos-task--from-table value))
        (t value)))

(defconst bndos-task--anchor-own-keys '(:path :side :line :end-line :blob :base-blob :removed)
  "Anchor fields diff-review itself keeps; the rest go under \"bndos\".")

(defun bndos-task-thread-to-comment (thread &optional old)
  "THREAD as a diff-review comment, starting from its OLD comment if any."
  (let ((c (if old (copy-hash-table old) (make-hash-table :test #'equal)))
        (anchor (plist-get thread :anchor)))
    (puthash "id" (plist-get thread :id) c)
    (dolist (key '("path" "lines" "side" "blob" "base-blob" "removed")) (remhash key c))
    (when-let* ((path (plist-get anchor :path)))
      (puthash "path" path c)
      (when-let* ((line (plist-get anchor :line)))
        (puthash "lines" (vector line (or (plist-get anchor :end-line) line)) c))
      (when (equal (plist-get anchor :side) "old") (puthash "side" "old" c))
      (when-let* ((blob (plist-get anchor :blob))) (puthash "blob" blob c))
      (when-let* ((base (plist-get anchor :base-blob))) (puthash "base-blob" base c))
      (when-let* ((removed (plist-get anchor :removed)))
        (puthash "removed" (vconcat (mapcar #'vconcat removed)) c)))
    (puthash "author" (plist-get thread :author) c)
    (puthash "at" (or (plist-get thread :created) (bndos-task-now)) c)
    (puthash "body" (plist-get thread :body) c)
    (if (equal (plist-get thread :state) "resolved") (puthash "resolved" t c) (remhash "resolved" c))
    (let ((old-replies (append (and old (gethash "replies" old)) nil)))
      (puthash "replies"
               (vconcat
                (mapcar (lambda (reply)
                          (let ((r (or (seq-find (lambda (r) (equal (bndos-task-get (bndos-task--from-table r) :bndos :id)
                                                                    (plist-get reply :id)))
                                                 old-replies)
                                       (make-hash-table :test #'equal))))
                            (setq r (copy-hash-table r))
                            (puthash "author" (plist-get reply :author) r)
                            (puthash "at" (or (plist-get reply :created) (bndos-task-now)) r)
                            (puthash "body" (or (plist-get reply :body) "") r)
                            (puthash "bndos" (bndos-task--to-table
                                              (bndos-task-store--without reply '(:author :created :body)))
                                     r)
                            r))
                        (plist-get thread :replies)))
               c))
    (puthash "bndos" (bndos-task--to-table
                      (append (bndos-task-store--without thread '(:id :author :created :body :anchor :replies))
                              (when-let* ((extra (bndos-task-store--without anchor bndos-task--anchor-own-keys)))
                                (list :anchor extra))))
             c)
    c))

(defvar bndos-task--blob-lines (make-hash-table :test #'equal)
  "Blob id -> its lines: blobs never change.")

(defun bndos-task--blob-lines (directory blob)
  (or (gethash blob bndos-task--blob-lines)
      (with-temp-buffer
        (let ((default-directory (file-name-as-directory directory)))
          (when (zerop (process-file "git" nil t nil "cat-file" "blob" blob))
            (puthash blob (split-string (buffer-string) "\n") bndos-task--blob-lines))))))

(defvar bndos-task--blob-hunks (make-hash-table :test #'equal)
  "(OLD . NEW) blob ids -> their hunks: blobs never change.")

(defun bndos-task--blob-hunks (directory old new)
  "Hunks from blob OLD to NEW as (OLD-START OLD-COUNT NEW-START NEW-COUNT)."
  (let ((key (cons old new)))
    (or (gethash key bndos-task--blob-hunks)
        (with-temp-buffer
          (let ((default-directory (file-name-as-directory directory)))
            (when (zerop (process-file "git" nil t nil "diff" "-U0" "--no-color" old new))
              (goto-char (point-min))
              (let (hunks)
                (while (re-search-forward
                        "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" nil t)
                  (push (list (string-to-number (match-string 1))
                              (if (match-string 2) (string-to-number (match-string 2)) 1)
                              (string-to-number (match-string 3))
                              (if (match-string 4) (string-to-number (match-string 4)) 1))
                        hunks))
                (puthash key (nreverse hunks) bndos-task--blob-hunks))))))))

(defun bndos-task-selection-diff (new-lines old-lines hunks first last &optional removed)
  "Lines FIRST..LAST of NEW-LINES as a unified diff from OLD-LINES, by HUNKS.
Unchanged lines start with a space, added ones with +, removed ones with -
where they were.  REMOVED, ranges (FIRST LAST) of old lines, says exactly
which removed lines are in (as a diff view selection gives them); without
it removed lines belong to the line they're drawn above, so each selected
line brings its whole block, and a deletion at the end of the file belongs
to the last line."
  (let ((final (length new-lines)) out)
    (cl-flet* ((wanted (k) (or (null removed) (seq-some (lambda (r) (<= (car r) k (cadr r))) removed)))
               (attach (hunk) (pcase-let ((`(,_ ,_ ,new-start ,new-count) hunk))
                                (if (zerop new-count) (1+ new-start) new-start)))
               (emit-removed (hunk)
                 (pcase-let ((`(,old-start ,old-count ,_ ,_) hunk))
                   (dotimes (i old-count)
                     (let ((k (+ old-start i)))
                       (when (wanted k)
                         (push (concat "-" (or (nth (1- k) old-lines) "")) out)))))))
      (cl-loop for n from first to last do
               (dolist (hunk hunks)
                 (when (and (> (nth 1 hunk) 0) (= n (attach hunk))) (emit-removed hunk)))
               (push (concat (if (seq-some (lambda (hunk)
                                             (pcase-let ((`(,_ ,_ ,new-start ,new-count) hunk))
                                               (and (> new-count 0) (<= new-start n (+ new-start new-count -1)))))
                                           hunks)
                                 "+" " ")
                             (or (nth (1- n) new-lines) ""))
                     out))
      (dolist (hunk hunks)
        (when (and (> (nth 1 hunk) 0)
                   (or (and removed (= (attach hunk) (1+ last)))   ; chosen below the last line
                       (and (>= last final) (zerop (nth 3 hunk)) (>= (nth 2 hunk) final))))
          (emit-removed hunk))))
    (mapconcat #'identity (nreverse out) "\n")))

(defun bndos-task-thread-with-selection (thread directory)
  "THREAD with its anchor's selected lines, when it has none.
Comments made in diff-review keep their lines, the blob they were made on
and the base's blob: the lines are rebuilt from those as a diff, with the
removed lines each brings, so an agent sees the change itself."
  (let ((anchor (plist-get thread :anchor)))
    (if-let* (((not (plist-get anchor :selection)))
              (line (plist-get anchor :line))
              (blob (plist-get anchor :blob))
              (lines (bndos-task--blob-lines directory blob)))
        (let* ((last (or (plist-get anchor :end-line) line))
               (base (plist-get anchor :base-blob))
               (old (and base (not (equal (plist-get anchor :side) "old")) (bndos-task--blob-lines directory base)))
               (hunks (and old (bndos-task--blob-hunks directory base blob)))
               (selection
                (if old
                    (bndos-task-selection-diff lines old hunks line last (plist-get anchor :removed))
                  (let ((prefix (if (equal (plist-get anchor :side) "old") "-" " ")))
                    (mapconcat (lambda (n) (concat prefix (or (nth (1- n) lines) "")))
                               (number-sequence line last) "\n")))))
          (plist-put (copy-sequence thread) :anchor
                     (append anchor (list :selection selection :text (nth (1- line) lines)))))
      thread)))

(defun bndos-task-thread-from-comment (c)
  "Diff-review comment C as a thread plist."
  (let* ((extras (bndos-task--from-table (gethash "bndos" c)))
         (lines (gethash "lines" c))
         (path (gethash "path" c))
         (stored (plist-get extras :state))
         (state (cond ((gethash "resolved" c) "resolved")
                      ;; Reopened in diff-review.
                      ((or (null stored) (equal stored "resolved")) "open")
                      (t stored))))
    (append
     (list :id (gethash "id" c)
           :kind (or (plist-get extras :kind) "comment")
           :author (gethash "author" c)
           :created (gethash "at" c)
           :state state
           :body (gethash "body" c)
           :anchor (when path
                     (append (list :path path :side (or (gethash "side" c) "new"))
                             (when (vectorp lines)
                               (append (list :line (aref lines 0))
                                       (unless (equal (aref lines 0) (aref lines 1))
                                         (list :end-line (aref lines 1)))))
                             (when-let* ((blob (gethash "blob" c))) (list :blob blob))
                             (when-let* ((base (gethash "base-blob" c))) (list :base-blob base))
                             (when-let* ((removed (gethash "removed" c)))
                               (list :removed (mapcar (lambda (r) (append r nil)) removed)))
                             (plist-get extras :anchor)))
           :replies (mapcar (lambda (r)
                              (append (list :author (gethash "author" r) :created (gethash "at" r)
                                            :body (gethash "body" r))
                                      (bndos-task--from-table (gethash "bndos" r))))
                            (gethash "replies" c)))
     (bndos-task-store--without extras '(:state :kind :anchor)))))

(defun bndos-task-store-update (key fn)
  "Load KEY's record, replace it with (FN RECORD) and save.  Return the result.
FN receives nil when no record exists yet."
  (let ((new (funcall fn (bndos-task-store-load key))))
    (when new
      (bndos-task-store-save (plist-put new :key key)))))

(defun bndos-task-store-all ()
  "Return every readable task record.  Unreadable ones are reported, not fatal."
  (let (records)
    (dolist (file (file-expand-wildcards (bndos-task-store--path "tasks" "*.json")))
      (condition-case err
          (when-let* ((record (bndos-task-store-load (file-name-base file))))
            (push record records))
        (error (message "bndos-task: %s" (error-message-string err)))))
    (nreverse records)))

(cl-defun bndos-task-store-ensure (&key key repo worktree branch project handle)
  "Return the record for KEY, creating a minimal one with the given identity."
  (or (bndos-task-store-load key)
      (bndos-task-store-save
       (list :key key
             :created (bndos-task-now)
             :repo (list :common-dir repo :project project)
             :worktree (list :path worktree :handle handle)
             :branch branch
             :review (list :state "unreviewed")
             :threads nil
             :requests nil
             :iterations nil))))


;;; Threads, replies and requests

(defconst bndos-task-thread-states '("open" "sent" "addressed" "resolved")
  "Discussion thread states.  Staleness is tracked separately in :stale.")

(defun bndos-task-thread (record id)
  (seq-find (lambda (thread) (equal (plist-get thread :id) id))
            (plist-get record :threads)))

(defun bndos-task-update-thread (record id fn)
  "Return RECORD with thread ID replaced by (FN THREAD)."
  (plist-put (copy-sequence record) :threads
             (mapcar (lambda (thread)
                       (if (equal (plist-get thread :id) id) (funcall fn thread) thread))
                     (plist-get record :threads))))

(cl-defun bndos-task-make-thread (&key kind author body anchor)
  "Return a new thread plist.  KIND is \"comment\" or \"question\"."
  (list :id (bndos-task-new-id "c")
        :kind kind
        :author author
        :created (bndos-task-now)
        :state "open"
        :body body
        :anchor anchor
        :replies nil
        :submissions nil))

(defun bndos-task-add-thread (record thread)
  (plist-put (copy-sequence record) :threads
             (append (plist-get record :threads) (list thread))))

(cl-defun bndos-task-make-reply (&key author body kind status files tests request)
  (list :id (bndos-task-new-id "m")
        :author author
        :kind (or kind "reply")
        :created (bndos-task-now)
        :body body
        :status status
        :files files
        :tests tests
        :request request))

(defun bndos-task-thread-add-reply (thread reply)
  (plist-put (copy-sequence thread) :replies
             (append (plist-get thread :replies) (list reply))))

(defun bndos-task-thread-set-state (thread state &optional actor note)
  "Return THREAD in STATE, recording ACTOR and NOTE as a state-change reply."
  (unless (member state bndos-task-thread-states)
    (error "Invalid thread state %S" state))
  (let ((thread (plist-put (copy-sequence thread) :state state)))
    (if (or actor note)
        (bndos-task-thread-add-reply
         thread (bndos-task-make-reply :author (or actor "system") :kind "state"
                                       :status state :body (or note "")))
      thread)))

(defun bndos-task-request (record id)
  (seq-find (lambda (request) (equal (plist-get request :id) id))
            (plist-get record :requests)))

(defun bndos-task-update-request (record id fn)
  (plist-put (copy-sequence record) :requests
             (mapcar (lambda (request)
                       (if (equal (plist-get request :id) id) (funcall fn request) request))
                     (plist-get record :requests))))

(defun bndos-task-in-flight-request (record)
  "Return RECORD's request that is still waiting on the agent, if any."
  (seq-find (lambda (request)
              (member (plist-get request :status) '("sending" "in-progress")))
            (plist-get record :requests)))

(defun bndos-task-unresolved-count (record)
  "Number of RECORD's threads that are not resolved."
  (seq-count (lambda (thread) (not (equal (plist-get thread :state) "resolved")))
             (plist-get record :threads)))

(provide 'bndos-task-store)
;;; bndos-task-store.el ends here
