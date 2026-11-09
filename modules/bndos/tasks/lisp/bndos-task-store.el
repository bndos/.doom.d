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

(defun bndos-task-store-load (key)
  "Return the record for KEY, or nil if none exists.
Signal an error for unreadable files or newer record versions rather than
treating them as absent."
  (let ((file (bndos-task-store-file key)))
    (when (file-exists-p file)
      (let ((mtime (file-attribute-modification-time (file-attributes file)))
            (cached (gethash key bndos-task-store--cache)))
        (if (and cached (equal (car cached) mtime))
            (cdr cached)
          (let ((record (condition-case err
                            (bndos-task-store--read-json-file file)
                          (error (error "Unreadable task record %s: %s"
                                        file (error-message-string err))))))
            (when (> (or (plist-get record :version) 0) bndos-task-store-version)
              (error "Task record %s has version %s; this code understands %s"
                     file (plist-get record :version) bndos-task-store-version))
            (puthash key (cons mtime record) bndos-task-store--cache)
            record))))))

(defun bndos-task-store-save (record)
  "Write RECORD (which must have :key) and return it with :updated refreshed."
  (let* ((key (or (plist-get record :key) (error "Task record without :key")))
         (record (plist-put (plist-put (copy-sequence record) :version bndos-task-store-version)
                            :updated (bndos-task-now)))
         (file (bndos-task-store-file key)))
    (bndos-task-store--write-atomically file (bndos-task-store-encode record))
    (puthash key (cons (file-attribute-modification-time (file-attributes file)) record)
             bndos-task-store--cache)
    record))

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
