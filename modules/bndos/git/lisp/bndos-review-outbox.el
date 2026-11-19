;;; bndos-review-outbox.el --- Replies to a pull request: drafted, confirmed, sent -*- lexical-binding: t; -*-

;;; Commentary:

;; Replies to a pull request's GitHub threads, and comments on its
;; conversation, are drafts first: written here (r on a GitHub thread) or by
;; an agent (`bndos-task drafts'), shown in their threads, and sent from the
;; outbox only: the drafts marked there, after a confirmation.
;;
;; A draft is a file, its folder its state, under the repository's common
;; git directory, in review/github/OWNER/NAME/NUMBER/:
;;
;;   drafts/ID.json    written, to edit, delete or send
;;   sending/ID.json   claimed by a send; nothing else touches it
;;   sent/ID.json      on GitHub, its link kept
;;
;; Moving a file is atomic, so one send claims a draft, once.  A mark keeps
;; the hash of the draft as shown; the send claims the draft and sends it
;; only when it is still that one, so a draft changed or deleted after it was
;; marked, by an agent or another Emacs, is not sent.  An edit or a deletion
;; claims the draft too, and checks it is the one the user saw.
;;
;; A sent reply ends with an invisible marker, <!-- bndos-draft:ID -->.  A
;; send that may or may not have reached GitHub (no answer, a server error)
;; leaves its draft in sending/, unconfirmed, and never sends it again by
;; itself: the next fetch that finds the marker on GitHub moves it to sent/;
;; otherwise the user, having looked, puts it back among the drafts.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'filenotify)
(require 'diff-review)
(require 'bndos-review-github)
(require 'bndos-review-changes)

(declare-function evil-define-key* "evil-core")
(declare-function evil-set-initial-state "evil-core")

(defgroup bndos-review-outbox nil
  "Drafted replies to pull requests."
  :group 'diff-review)

(defcustom bndos-review-outbox-max-age 120
  "Seconds GitHub's threads may be old when drafts are sent; older, they are
fetched again first, and the outbox shown again before anything is sent."
  :type 'integer)

(defface bndos-review-outbox-warning '((t :inherit warning))
  "Warnings about a draft.")
(defface bndos-review-outbox-blocked '((t :inherit error))
  "Why a draft can't be sent.")

(defconst bndos-review-outbox--states '("drafts" "sending" "sent"))

;;;; The store

(defun bndos-review-outbox-store (&optional review)
  "The folder of the drafts of REVIEW's pull request (default this buffer's
review), or nil when it has none fetched."
  (when-let* ((review (or review (ignore-errors (diff-review--review-buffer)))))
    (with-current-buffer review
      (let ((state bndos-review-github--state))
        (when (and (plist-get state :number) (plist-get state :owner))
          (file-name-as-directory
           (expand-file-name (format "review/github/%s/%s/%d" (plist-get state :owner)
                                     (plist-get state :name) (plist-get state :number))
                             (diff-review--common-dir diff-review--root))))))))

(defun bndos-review-outbox--path (store state id)
  (expand-file-name (format "%s/%s.json" state id) store))

(defun bndos-review-outbox--ensure (store)
  (dolist (state bndos-review-outbox--states)
    (make-directory (expand-file-name state store) t)))

(defun bndos-review-outbox--read (file)
  (ignore-errors
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file))
      (json-parse-buffer :object-type 'plist :array-type 'list :null-object nil :false-object nil))))

(defun bndos-review-outbox--clean (draft)
  "DRAFT as it is written: without what loading added, its arrays (read as
lists) vectors again, as `json-serialize' takes arrays."
  (let (out)
    (cl-loop for (k v) on draft by #'cddr
             unless (memq k '(:state :file))
             do (setq out (plist-put out k (if (eq k :changes) (bndos-review-outbox--changes-json v) v))))
    out))

(defun bndos-review-outbox--changes-json (changes)
  "CHANGES, a draft's, as JSON is written: :links and :rules arrays, a
rule's :in too."
  (let ((out (copy-sequence changes)))
    (setq out (plist-put out :links (vconcat (plist-get changes :links))))
    (setq out (plist-put out :rules (vconcat (mapcar (lambda (r) (if (plist-member r :in)
                                                                     (list :in (vconcat (plist-get r :in)))
                                                                   r))
                                                     (plist-get changes :rules)))))
    out))

(defun bndos-review-outbox--write-json (file value)
  "Write VALUE to FILE as JSON: UTF-8 bytes, as they are, never a question
of which coding system."
  (let ((coding-system-for-write 'no-conversion))
    ;; `json-serialize' gives UTF-8 bytes.
    (write-region (json-serialize value) nil file nil 'silent)))

(defun bndos-review-outbox--write (file draft)
  "Write DRAFT to FILE, a file only this caller holds."
  (bndos-review-outbox--write-json file (bndos-review-outbox--clean draft)))

(defun bndos-review-outbox--move (from to)
  "Move FROM to TO, which mustn't exist: non-nil when this call moved it."
  (condition-case nil (progn (rename-file from to) t) (file-error nil)))

(defun bndos-review-outbox--changes-text (changes)
  "CHANGES as a draft's hash takes them, as `bndos-task' does."
  (string-join
   (append (list (concat "base:" (or (plist-get changes :base) "None"))
                 (concat "head:" (or (plist-get changes :head) "None")))
           (sort (mapcar (lambda (l) (format "link:%s@%s:%s-%s" (plist-get l :id) (plist-get l :path)
                                             (plist-get l :from) (plist-get l :to)))
                         (plist-get changes :links))
                 #'string<)
           (sort (mapcar (lambda (r) (if (plist-member r :in)
                                         (concat "rule:in:" (string-join (plist-get r :in) ","))
                                       (concat "rule:matching:" (plist-get r :matching))))
                         (plist-get changes :rules))
                 #'string<))
   "\n"))

(defun bndos-review-outbox-hash (draft)
  "What a mark or an edit keeps of DRAFT: the hash of its body, and of the
changes it links."
  (secure-hash 'sha256 (encode-coding-string
                        (concat (or (plist-get draft :body) "")
                                (when-let* ((changes (plist-get draft :changes)))
                                  (concat "\0" (bndos-review-outbox--changes-text changes))))
                        'utf-8)))

(defun bndos-review-outbox--now () (format-time-string "%FT%TZ" nil t))

(defun bndos-review-outbox--new-id ()
  (format "d%s-%04x%04x" (format-time-string "%Y%m%d%H%M%S" nil t) (random 65536) (random 65536)))

(defun bndos-review-outbox--held (store id)
  "A name for draft ID held by this Emacs while it is changed."
  (expand-file-name (format "drafts/.%s.held-%d" id (emacs-pid)) store))

(defvar bndos-review-outbox--hold-grace 60
  "Seconds a hold by a process not seen running is left before it is undone.")

(defun bndos-review-outbox--recover (store)
  "Put back drafts a dead process held while changing them."
  (dolist (file (directory-files (expand-file-name "drafts" store) t "\\`\\.[^.].*\\.held-[0-9]+\\'"))
    (when (string-match "\\`\\.\\(.+\\)\\.held-\\([0-9]+\\)\\'" (file-name-nondirectory file))
      (let ((id (match-string 1 (file-name-nondirectory file)))
            (pid (string-to-number (match-string 2 (file-name-nondirectory file)))))
        ;; Gone, by its pid; and a minute old, should the holder be a
        ;; process whose pid isn't seen from here (in a sandbox).  The
        ;; status change time is the hold's: a rename keeps the other.
        (unless (or (= pid (emacs-pid)) (process-attributes pid)
                    (< (float-time (time-since (file-attribute-status-change-time (file-attributes file))))
                       bndos-review-outbox--hold-grace))
          (bndos-review-outbox--move file (bndos-review-outbox--path store "drafts" id)))))))

(defun bndos-review-outbox-load (store)
  "STORE's drafts, being sent and sent, oldest first, each with :state and :file."
  (when (file-directory-p store)
    (bndos-review-outbox--recover store)
    (sort (cl-loop for state in bndos-review-outbox--states
                   for dir = (expand-file-name state store)
                   append (cl-loop for file in (and (file-directory-p dir)
                                                    (directory-files dir t "\\`[^.].*\\.json\\'"))
                                   for draft = (bndos-review-outbox--read file)
                                   when draft collect (append (list :state state :file file) draft)))
          (lambda (a b) (string< (or (plist-get a :created) "") (or (plist-get b :created) ""))))))

(defun bndos-review-outbox-create (store draft)
  "Add DRAFT to STORE's drafts, with a new id; return it."
  (bndos-review-outbox--ensure store)
  (let* ((id (bndos-review-outbox--new-id))
         (draft (append (list :version 1 :id id) (bndos-review-outbox--clean draft)
                        (list :created (bndos-review-outbox--now) :updated (bndos-review-outbox--now))))
         (tmp (make-temp-file (expand-file-name "drafts/.new-" store))))
    (bndos-review-outbox--write tmp draft)
    (unless (bndos-review-outbox--move tmp (bndos-review-outbox--path store "drafts" id))
      (delete-file tmp)
      (error "Couldn't add the draft"))
    (bndos-review-outbox-changed store)
    draft))

(defun bndos-review-outbox--hold (store id hash)
  "Hold draft ID if it is still the one HASH names: the held file, or a reason."
  (let ((file (bndos-review-outbox--path store "drafts" id))
        (held (bndos-review-outbox--held store id)))
    (cond ((not (bndos-review-outbox--move file held)) "it was sent, deleted or is being changed meanwhile")
          ((not (equal (bndos-review-outbox-hash (bndos-review-outbox--read held)) hash))
           (bndos-review-outbox--move held file)
           "it changed meanwhile")
          (t held))))

(defun bndos-review-outbox-change (store id hash body)
  "Make BODY draft ID's, if it is still the one HASH names: t, or a reason."
  (let ((held (bndos-review-outbox--hold store id hash)))
    (if (not (file-name-absolute-p held))
        held
      (let ((draft (bndos-review-outbox--read held)))
        (bndos-review-outbox--write held (plist-put (plist-put draft :body body) :updated (bndos-review-outbox--now)))
        (bndos-review-outbox--move held (bndos-review-outbox--path store "drafts" id))
        (bndos-review-outbox-changed store)
        t))))

(defun bndos-review-outbox-remove (store id hash)
  "Delete draft ID, if it is still the one HASH names: t, or a reason."
  (let ((held (bndos-review-outbox--hold store id hash)))
    (if (not (file-name-absolute-p held))
        held
      (delete-file held)
      (bndos-review-outbox-changed store)
      t)))

;;;; Sending

(defvar bndos-review-outbox--in-flight nil
  "Ids of the drafts this Emacs is sending now.")

(defun bndos-review-outbox--sender-alive-p (claim)
  "Whether the Emacs CLAIM names is running, sending: not this one, whose
sends are `bndos-review-outbox--in-flight'."
  (let ((pid (plist-get claim :pid)))
    (and (integerp pid) (/= pid (emacs-pid))
         (equal (plist-get claim :host) (system-name))
         (let ((attrs (process-attributes pid)))
           (and attrs (string-match-p "emacs" (downcase (or (alist-get 'comm attrs) ""))))))))

(defun bndos-review-outbox-unconfirmed-p (draft)
  "Whether DRAFT, being sent, is being sent by nobody: it may or may not have
reached GitHub.  Not while this Emacs or another sends it: a draft is in
sending/ with its claim, cleared once a send ends without an answer; a
claim of an Emacs no longer running was cut short."
  (and (equal (plist-get draft :state) "sending")
       (not (member (plist-get draft :id) bndos-review-outbox--in-flight))
       (not (bndos-review-outbox--sender-alive-p (plist-get draft :claim)))))

(defcustom bndos-review-outbox-stuck-after 600
  "Seconds after which a send another Emacs claimed, still running, may be
put back by hand (R): that Emacs lost track of it."
  :type 'integer
  :group 'diff-review)

(defun bndos-review-outbox--stuck-p (draft)
  "Whether DRAFT's send, claimed by another Emacs still running, is older than
`bndos-review-outbox-stuck-after'."
  (let* ((claim (plist-get draft :claim))
         (at (diff-review--time (plist-get claim :at))))
    (and (equal (plist-get draft :state) "sending")
         (bndos-review-outbox--sender-alive-p claim)
         ;; A time that can't be read is no sign of a stuck send.
         (> at 0)
         (> (float-time (time-since at)) bndos-review-outbox-stuck-after))))

(defun bndos-review-outbox--claim (store id hash pr)
  "Claim draft ID of pull request PR for sending if it is still the one HASH
names: the draft, or a reason it wasn't claimed."
  ;; Held first, out of everyone's reach, so it is in sending/ only with
  ;; its claim written.
  (let ((from (bndos-review-outbox--path store "drafts" id))
        (held (bndos-review-outbox--held store id))
        (to (bndos-review-outbox--path store "sending" id)))
    (if (not (bndos-review-outbox--move from held))
        "sent, deleted or being changed since it was marked"
      (let ((draft (bndos-review-outbox--read held)))
        (cond
         ((not (equal (bndos-review-outbox-hash draft) hash))
          (bndos-review-outbox--move held from)
          "changed since it was marked")
         ((not (equal (plist-get draft :pr) pr))
          (bndos-review-outbox--move held from)
          "the review shows another pull request now")
         (t
          (setq draft (plist-put draft :claim (list :pid (emacs-pid) :host (system-name)
                                                    :at (bndos-review-outbox--now))))
          (setq draft (plist-put draft :error nil))
          (bndos-review-outbox--write held draft)
          (if (bndos-review-outbox--move held to)
              draft
            (bndos-review-outbox--write held (plist-put draft :claim nil))
            (bndos-review-outbox--move held from)
            "a draft with its id is being sent already")))))))

(defun bndos-review-outbox--settle (store draft outcome)
  "Record OUTCOME of sending DRAFT, claimed: (sent URL ID), (failed MESSAGE)
or (unknown MESSAGE).  Its file may have gone meanwhile, to sent/ by a fetch
that found it on GitHub: then a sent draft is recorded there, nothing else."
  (let* ((id (plist-get draft :id))
         (file (bndos-review-outbox--path store "sending" id))
         (sent-file (bndos-review-outbox--path store "sent" id))
         (here (file-exists-p file)))
    (pcase outcome
      (`(sent ,url ,remote)
       (let ((draft (plist-put (plist-put draft :claim nil)
                               :sent (list :at (bndos-review-outbox--now) :url url :id remote))))
         (cond (here (bndos-review-outbox--write file draft)
                     (unless (bndos-review-outbox--move file sent-file)
                       (bndos-review-outbox--write sent-file draft)
                       (delete-file file)))
               (t (bndos-review-outbox--write sent-file draft)))))
      (`(failed ,message)
       (when here
         (bndos-review-outbox--write file (plist-put (plist-put draft :claim nil) :error message))
         (bndos-review-outbox--move file (bndos-review-outbox--path store "drafts" id))))
      (`(unknown ,message)
       (when here
         (bndos-review-outbox--write file (plist-put (plist-put draft :claim nil) :error message)))))
    (bndos-review-outbox-changed store)))

(defun bndos-review-outbox--marker (id) (format "\n\n<!-- bndos-draft:%s -->" id))

(defun bndos-review-outbox--endpoint (draft)
  "Where GitHub takes DRAFT, of the pull request it was written for."
  (let* ((target (plist-get draft :target))
         (pr (plist-get draft :pr))
         (repo (format "repos/%s/%s" (plist-get pr :owner) (plist-get pr :name))))
    (unless (and (stringp (plist-get pr :owner)) (stringp (plist-get pr :name)) (integerp (plist-get pr :number)))
      (error "The draft names no pull request"))
    (pcase (plist-get target :kind)
      ("reply" (if (integerp (plist-get target :comment))
                   (format "%s/pulls/%d/comments/%d/replies" repo (plist-get pr :number) (plist-get target :comment))
                 (error "The draft names no comment to reply to")))
      ("comment" (format "%s/issues/%d/comments" repo (plist-get pr :number)))
      (kind (error "Unknown kind of draft: %s" kind)))))

(defun bndos-review-outbox--response (result)
  "What gh's RESULT, run raw with --include, says of a post: (STATUS . BODY),
STATUS the HTTP status or nil when there is none, BODY the parsed reply or nil."
  (let* ((out (or (plist-get result :out) ""))
         (status (and (string-match "\\`HTTP/[0-9.]+ \\([0-9][0-9][0-9]\\)" out)
                      (string-to-number (match-string 1 out))))
         (start (and status (string-match "\r?\n\r?\n" out) (match-end 0))))
    (cons status
          (and start (ignore-errors (json-parse-string (substring out start) :object-type 'hash-table
                                                       :null-object nil :false-object nil))))))

(defun bndos-review-outbox--failure (status body result)
  "A message for a post GitHub refused with STATUS and BODY."
  (let ((errors (and (hash-table-p body) (gethash "errors" body))))
    (cond ((seq-some (lambda (e) (and (hash-table-p e)
                                      (string-match-p "pending review" (or (gethash "message" e) ""))))
                     errors)
           "you have a pending review on this pull request on GitHub: submit or discard it there first")
          (t (format "GitHub refused it (HTTP %d): %s" status
                     (or (and (hash-table-p body) (gethash "message" body))
                         (plist-get result :err) ""))))))

(defun bndos-review-outbox--post (review draft callback)
  "Post DRAFT to REVIEW's pull request; then CALLBACK with its outcome.
The outcome goes by the HTTP status GitHub answered with: a 2xx is sent, a
4xx refused (nothing was posted); anything else, no answer or a 5xx, may or
may not have been posted."
  (with-current-buffer review
    (unless (executable-find bndos-review-github-program)
      (error "No %s to post with" bndos-review-github-program))
    (let ((input (make-temp-file "bndos-outbox-" nil ".json"))
          (marker (bndos-review-outbox--marker (plist-get draft :id))))
      (bndos-review-outbox--write-json input (list :body (concat (plist-get draft :body) marker)))
      (bndos-review-github--gh
       (list "api" "--include" "-X" "POST" (bndos-review-outbox--endpoint draft)
             "--input" input)
       (lambda (result)
         (ignore-errors (delete-file input))
         (pcase-let ((`(,status . ,body) (condition-case nil (bndos-review-outbox--response result)
                                           (error (cons nil nil)))))
           (funcall callback
                    (cond ((and status (<= 200 status 299) (hash-table-p body) (gethash "id" body))
                           (unless (string-suffix-p (string-trim-left marker) (string-trim-right (or (gethash "body" body) "")))
                             (display-warning 'bndos-review-outbox
                                              (format "GitHub didn't keep the hidden tag of draft %s: a send left unconfirmed can't be checked"
                                                      (plist-get draft :id))))
                           (list 'sent (gethash "html_url" body) (gethash "id" body)))
                          ((and status (<= 400 status 499))
                           (list 'failed (bndos-review-outbox--failure status body result)))
                          (status (list 'unknown (format "HTTP %d" status)))
                          (t (list 'unknown (let ((err (plist-get result :err)))
                                              (if (string-empty-p (or err "")) "no answer" err))))))))
       t))))

(defun bndos-review-outbox-send-marks (review store marks done)
  "Send the drafts MARKS names, (ID . HASH) each, one at a time, to REVIEW's
pull request; then DONE with (ID . OUTCOME) each, OUTCOME as
`bndos-review-outbox--settle' takes it or (skipped REASON).  DONE is called
whatever goes wrong; a draft claimed is always settled."
  (let ((results nil)
        (pr (bndos-review-outbox--pr review)))
    (cl-labels
        ((next (rest)
           (if (null rest)
               (funcall done (nreverse results))
             (pcase-let* ((`(,id . ,hash) (car rest))
                          (draft (condition-case err (bndos-review-outbox--claim store id hash pr)
                                   (error (format "not claimed: %s" (error-message-string err))))))
               (if (stringp draft)
                   (progn (push (cons id (list 'skipped draft)) results)
                          (next (cdr rest)))
                 (push id bndos-review-outbox--in-flight)
                 (let ((finished nil))
                   (cl-flet ((finish (outcome)
                               (unless finished
                                 (setq finished t)
                                 (setq bndos-review-outbox--in-flight (delete id bndos-review-outbox--in-flight))
                                 (condition-case err (bndos-review-outbox--settle store draft outcome)
                                   (error (display-warning 'bndos-review-outbox
                                                           (format "Draft %s: %S, not recorded: %s" id outcome
                                                                   (error-message-string err)))))
                                 (push (cons id outcome) results)
                                 (next (cdr rest)))))
                     (ignore-errors (bndos-review-outbox-changed store))
                     ;; Before gh starts, nothing was posted.
                     (condition-case err
                         (bndos-review-outbox--post review draft #'finish)
                       (error (finish (list 'failed (format "not sent: %s" (error-message-string err)))))))))))))
      (next marks))))

(defun bndos-review-outbox-reconcile (review sent)
  "Move the drafts GitHub shows, SENT as (ID URL WHERE), to sent/: an
unconfirmed send that did arrive, or a failed one that did after all.  Only
where the draft was going: its thread, or the conversation."
  (when-let* ((store (bndos-review-outbox-store review)))
    (bndos-review-outbox--watch store)
    (pcase-dolist (`(,id ,url ,where) sent)
      (unless (member id bndos-review-outbox--in-flight)
        (dolist (state '("sending" "drafts"))
          (let ((file (bndos-review-outbox--path store state id)))
            (when (file-exists-p file)
              (let* ((sent-file (bndos-review-outbox--path store "sent" id))
                     (draft (bndos-review-outbox--read file))
                     (target (plist-get draft :target)))
                (when (and draft
                           (pcase (plist-get target :kind)
                             ("reply" (equal where (plist-get target :thread)))
                             ("comment" (eq where 'conversation)))
                           (not (bndos-review-outbox--sender-alive-p (plist-get draft :claim)))
                           (bndos-review-outbox--move file sent-file))
                  (bndos-review-outbox--write
                   sent-file (plist-put (plist-put (plist-put draft :error nil) :claim nil)
                                        :sent (list :at (bndos-review-outbox--now) :url url
                                                    :found t))))))))))
    (bndos-review-outbox-changed store)))

;;;; Drafts in the review's threads

(defvar bndos-review-outbox--cache (make-hash-table :test #'equal)
  "Each store's drafts as loaded: STORE -> list.")

(defun bndos-review-outbox-drafts (store)
  "STORE's drafts, loaded once until they change."
  (or (gethash store bndos-review-outbox--cache)
      (puthash store (or (bndos-review-outbox-load store) (list nil)) bndos-review-outbox--cache)))

(defun bndos-review-outbox--live (store)
  "STORE's drafts not sent yet."
  (seq-filter (lambda (d) (and d (not (equal (plist-get d :state) "sent"))))
              (bndos-review-outbox-drafts store)))

(defun bndos-review-outbox--state-text (draft)
  (cond ((equal (plist-get draft :state) "drafts")
         (if (plist-get draft :error) "draft, not sent: it failed" "draft, not sent"))
        ((bndos-review-outbox-unconfirmed-p draft) "unconfirmed: may be on GitHub")
        (t "sending")))

(defun bndos-review-outbox--message (draft)
  "DRAFT as a message of a thread."
  (let ((m (make-hash-table :test #'equal)))
    (puthash "author" (plist-get draft :author) m)
    (puthash "at" (plist-get draft :updated) m)
    (puthash "body" (plist-get draft :body) m)
    (puthash "draft" (concat (bndos-review-outbox--state-text draft)
                             (when-let* ((changes (plist-get draft :changes)))
                               (format " · %s (%s)" (bndos-review-changes-count changes)
                                       (substitute-command-keys "\\[bndos-review-outbox-thread-changes]"))))
             m)
    (puthash "draft-id" (plist-get draft :id) m)
    m))

(defun bndos-review-outbox--thread-drafts (comment)
  "The drafts replying to COMMENT, a GitHub thread (`diff-review-thread-drafts-function')."
  (when-let* ((thread (gethash "thread-id" comment))
              (store (bndos-review-outbox-store)))
    (mapcar #'bndos-review-outbox--message
            (seq-filter (lambda (d) (equal (plist-get (plist-get d :target) :thread) thread))
                        (bndos-review-outbox--live store)))))

(defun bndos-review-outbox-live-count (review)
  "How many drafts of REVIEW's pull request aren't sent yet."
  (if-let* ((store (bndos-review-outbox-store review))) (length (bndos-review-outbox--live store)) 0))

(defun bndos-review-outbox-notes (review)
  "REVIEW's drafted comments on the conversation, as notes of its timeline."
  (when-let* ((store (bndos-review-outbox-store review)))
    (mapcar (lambda (draft)
              (let ((m (bndos-review-outbox--message draft)))
                (puthash "id" (concat "draft:" (plist-get draft :id)) m)
                (puthash "at" (plist-get draft :created) m)
                (puthash "external" "GitHub" m)
                (puthash "note" t m)
                (puthash "where" "conversation" m)
                (puthash "chip" "Draft" m)
                (puthash "chip-face" 'diff-review-draft m)
                m))
            (seq-filter (lambda (d) (equal (plist-get (plist-get d :target) :kind) "comment"))
                        (bndos-review-outbox--live store)))))

(defvar bndos-review-outbox--watches nil "(STORE . DESCRIPTORS) watched.")
(defvar bndos-review-outbox--timer nil)
(defvar bndos-review-outbox--dirty nil "Stores changed since the last redraw.")

(defun bndos-review-outbox--watch (store)
  "Redraw what shows STORE's drafts when they change, here or elsewhere."
  (unless (assoc store bndos-review-outbox--watches)
    (bndos-review-outbox--ensure store)
    (push (cons store (delq nil (mapcar (lambda (state)
                                          (ignore-errors
                                            (file-notify-add-watch (expand-file-name state store) '(change)
                                                                   (lambda (_) (bndos-review-outbox-changed store)))))
                                        bndos-review-outbox--states)))
          bndos-review-outbox--watches)))

(defun bndos-review-outbox-changed (store)
  "Note STORE's drafts changed: what shows them is drawn again shortly."
  (remhash store bndos-review-outbox--cache)
  (cl-pushnew store bndos-review-outbox--dirty :test #'equal)
  (unless (timerp bndos-review-outbox--timer)
    (setq bndos-review-outbox--timer (run-with-timer 0.2 nil #'bndos-review-outbox--redraw))))

(defun bndos-review-outbox--redraw ()
  "Draw again the reviews and outboxes of the stores that changed."
  (setq bndos-review-outbox--timer nil)
  (let ((stores bndos-review-outbox--dirty))
    (setq bndos-review-outbox--dirty nil)
    (dolist (buffer (buffer-list))
      (cond ((and (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'diff-review-mode)
                  (member (ignore-errors (bndos-review-outbox-store buffer)) stores))
             (bndos-review-github--show buffer)
             (diff-review--panel-refresh))
            ((and (eq (buffer-local-value 'major-mode buffer) 'bndos-review-outbox-mode)
                  (member (buffer-local-value 'bndos-review-outbox--store buffer) stores))
             (with-current-buffer buffer (bndos-review-outbox--render)))))))

;;;; Writing a draft

(defun bndos-review-outbox--last-at (messages)
  (car (last (sort (delq nil (mapcar (lambda (m) (gethash "at" m)) messages))
                   (lambda (a b) (< (diff-review--time a) (diff-review--time b)))))))

(defun bndos-review-outbox--conversation-at (review)
  "When the latest comment on REVIEW's conversation was written."
  (with-current-buffer review
    (bndos-review-outbox--last-at (seq-filter #'bndos-review-outbox--note-p bndos-review-github--comments))))

(defun bndos-review-outbox--note-p (comment)
  "Whether COMMENT is on the conversation: a comment, or a review's summary."
  (equal (gethash "where" comment) "conversation"))

(defun bndos-review-outbox--read-body (prompt &optional initial)
  (let ((body (string-trim (read-string-from-buffer prompt (or initial "")))))
    (when (string-empty-p body) (user-error "Nothing to say"))
    body))

(defun bndos-review-outbox--author ()
  (or (diff-review--author) user-login-name))

(defun bndos-review-outbox-edit-draft (store draft)
  "Edit DRAFT of STORE; keep the text in the kill ring if it can't be saved."
  (let* ((hash (bndos-review-outbox-hash draft))
         (body (bndos-review-outbox--read-body "Edit the draft" (plist-get draft :body)))
         (result (bndos-review-outbox-change store (plist-get draft :id) hash body)))
    (unless (eq result t)
      (kill-new body)
      (user-error "Draft not saved: %s; your text is in the kill ring" result))
    (message "Draft saved: send it from the outbox (%s)"
             (substitute-command-keys "\\[bndos-review-outbox]"))))

(defun bndos-review-outbox-reply (comment)
  "Draft a reply to COMMENT, a GitHub thread or comment
\(`diff-review-external-reply-function'); a draft is edited."
  (let* ((review (diff-review--in-review (current-buffer)))
         (store (or (bndos-review-outbox-store review) (user-error "No pull request fetched for this review")))
         (own (diff-review--in-review (bndos-review-outbox--author)))
         (draft-id (gethash "draft-id" comment))
         (thread (gethash "thread-id" comment))
         (mine (and thread (seq-find (lambda (d) (and (equal (plist-get (plist-get d :target) :thread) thread)
                                                      (equal (plist-get d :state) "drafts")
                                                      (equal (plist-get d :author) own)))
                                     (bndos-review-outbox--live store)))))
    (cond
     (draft-id
      (let ((draft (seq-find (lambda (d) (equal (plist-get d :id) draft-id)) (bndos-review-outbox--live store))))
        (unless (equal (plist-get draft :state) "drafts") (user-error "That draft is being sent"))
        (bndos-review-outbox-edit-draft store draft)))
     (mine (bndos-review-outbox-edit-draft store mine))
     (thread
      (unless (gethash "reply-to" comment) (user-error "This thread has no comment to reply to"))
      (let ((body (bndos-review-outbox--read-body (format "Reply to %s (a draft)" (gethash "author" comment)))))
        (bndos-review-outbox-create
         store (list :pr (bndos-review-outbox--pr review)
                     :target (list :kind "reply" :thread thread :comment (gethash "reply-to" comment)
                                   :path (gethash "path" comment)
                                   :line (cdr (diff-review--comment-lines comment)))
                     :body body :author own
                     :seen (bndos-review-outbox--last-at (cons comment (append (gethash "replies" comment) nil)))))
        (message "Reply drafted: send it from the outbox (%s)" (substitute-command-keys "\\[bndos-review-outbox]"))))
     ;; GitHub has no replies to the conversation's comments and reviews:
     ;; a comment of its own, quoting all of it, to answer it point by point.
     (t (bndos-review-outbox-comment
         review (concat (mapconcat (lambda (line) (if (string-empty-p line) ">" (concat "> " line)))
                                   (split-string (string-trim-right (or (gethash "body" comment) "")) "\n")
                                   "\n")
                        "\n\n")
         (gethash "author" comment))))))

(defun bndos-review-outbox--pr (review)
  (with-current-buffer review
    (list :owner (plist-get bndos-review-github--state :owner)
          :name (plist-get bndos-review-github--state :name)
          :number (plist-get bndos-review-github--state :number))))

(defun bndos-review-outbox-comment (&optional review initial answers)
  "Draft a comment on the conversation of this review's pull request; one
ANSWERS, an author, starts with INITIAL, the quote of what they wrote."
  (interactive)
  (let* ((review (or review (diff-review--in-review (current-buffer))))
         (store (or (bndos-review-outbox-store review) (user-error "No pull request fetched for this review")))
         (body (bndos-review-outbox--read-body (if answers (format "Answer to %s on the pull request (a draft)" answers)
                                                 "Comment on the pull request (a draft)")
                                               initial)))
    (when (and initial (equal body (string-trim initial)))
      (user-error "Nothing said but the quote"))
    (bndos-review-outbox-create
     store (list :pr (bndos-review-outbox--pr review)
                 :target (append (list :kind "comment") (and answers (list :answers answers)))
                 :body body
                 :author (with-current-buffer review (bndos-review-outbox--author))
                 :seen (bndos-review-outbox--conversation-at review)))
    (message "Comment drafted: send it from the outbox (%s)" (substitute-command-keys "\\[bndos-review-outbox]"))))

;;;; The outbox

(defvar-local bndos-review-outbox--review nil "The review this outbox sends for.")
(defvar-local bndos-review-outbox--store nil "Its drafts' folder.")
(defvar-local bndos-review-outbox--marks nil "(ID . HASH) of the drafts marked, as shown when marked.")
(defvar-local bndos-review-outbox--sending nil "Non-nil while this outbox sends.")
(defvar-local bndos-review-outbox--report nil "What the last send did, for the header.")
(defvar-local bndos-review-outbox--login nil "Whom GitHub posts as.")

(defun bndos-review-outbox--github (review)
  "REVIEW's fetched GitHub state."
  (buffer-local-value 'bndos-review-github--state review))

(defun bndos-review-outbox--age (review)
  "Seconds since REVIEW's threads were fetched, or nil."
  (when-let* ((at (plist-get (bndos-review-outbox--github review) :at)))
    (float-time (time-subtract nil at))))

(defun bndos-review-outbox--thread (review draft)
  "The GitHub thread DRAFT replies to, as fetched, or nil."
  (seq-find (lambda (c) (equal (gethash "thread-id" c) (plist-get (plist-get draft :target) :thread)))
            (buffer-local-value 'bndos-review-github--comments review)))

(defun bndos-review-outbox-problems (review draft)
  "What to know before sending DRAFT: (BLOCKER . WARNINGS), strings."
  (let ((target (plist-get draft :target))
        (seen (diff-review--time (plist-get draft :seen)))
        blocker warnings)
    (when (string-prefix-p "agent:" (or (plist-get draft :author) ""))
      (push (format "drafted by %s: read it before sending" (plist-get draft :author)) warnings))
    (pcase (plist-get target :kind)
      ("reply"
       (let ((thread (bndos-review-outbox--thread review draft)))
         (cond
          ((not thread)
           (setq blocker "its thread isn't on the pull request (deleted, or not fetched)"))
          ((plist-get (bndos-review-outbox--github review) :pending-review)
           (setq blocker "you have a pending review on this pull request on GitHub: submit or discard it there, then g"))
          (t
           (let ((new (seq-count (lambda (m) (> (diff-review--time (gethash "at" m)) seen))
                                 (cons thread (append (gethash "replies" thread) nil)))))
             (when (> new 0)
               (push (format "%d new message%s in the thread since it was drafted" new (if (= new 1) "" "s"))
                     warnings)))
           (when (gethash "resolved" thread) (push "the thread is resolved on GitHub" warnings))
           (when (gethash "github-outdated" thread) (push "the thread is on outdated lines" warnings))))))
      ("comment"
       (let ((new (seq-count (lambda (c) (and (bndos-review-outbox--note-p c)
                                              (> (diff-review--time (gethash "at" c)) seen)))
                             (buffer-local-value 'bndos-review-github--comments review))))
         (when (> new 0)
           (push (format "%d new comment%s or review%s on the pull request since it was drafted"
                         new (if (= new 1) "" "s") (if (= new 1) "" "s"))
                 warnings)))))
    (when (plist-get draft :error)
      (push (format "the last send failed: %s" (plist-get draft :error)) warnings))
    (when-let* ((changes (plist-get draft :changes)))
      (let* ((root (buffer-local-value 'diff-review--root review))
             (view (bndos-review-changes-view-cached root changes))
             (head (plist-get (bndos-review-outbox--github review) :head-oid)))
        (cond
         ((null view) (push "its changes' base commit isn't here: fetch it to see them" warnings))
         (t (setq warnings (append (reverse (cdr (bndos-review-changes-summary view))) warnings))))
        ;; It says what changed: the reviewer must be able to see it.
        (unless blocker
          (cond ((null head) (setq blocker "the pull request's head on GitHub isn't known: g fetches it"))
                ((not (equal head (bndos-review-changes-head root)))
                 (setq blocker "the branch here isn't the pull request's head on GitHub: push (or pull) first"))))
        (when (bndos-review-changes-dirty-p root)
          (push "uncommitted changes in the worktree: the reviewer won't see them" warnings))))
    (cons blocker (nreverse warnings))))

(defun bndos-review-outbox--describe-target (review draft)
  (let ((target (plist-get draft :target)))
    (pcase (plist-get target :kind)
      ("reply"
       (let ((thread (bndos-review-outbox--thread review draft)))
         (format "Reply on %s:%s%s" (plist-get target :path) (or (plist-get target :line) "")
                 (if thread
                     (format "  %s: %s" (gethash "author" thread)
                             (truncate-string-to-width (diff-review--first-line thread) 50 nil nil "…"))
                   ""))))
      (_ (if-let* ((answers (plist-get target :answers)))
             (format "Comment on the pull request, answering %s" answers)
           "Comment on the pull request")))))

(defun bndos-review-outbox--marked-p (draft)
  (equal (cdr (assoc (plist-get draft :id) bndos-review-outbox--marks)) (bndos-review-outbox-hash draft)))

(defun bndos-review-outbox--insert-draft (draft)
  (let* ((review bndos-review-outbox--review)
         (problems (bndos-review-outbox-problems review draft))
         (start (point))
         (state (plist-get draft :state))
         (stale-mark (and (assoc (plist-get draft :id) bndos-review-outbox--marks)
                          (not (bndos-review-outbox--marked-p draft)))))
    (insert (pcase state
              ("drafts" (cond ((car problems) (propertize " ✗ " 'face 'bndos-review-outbox-blocked))
                              ((bndos-review-outbox--marked-p draft) (propertize "[x]" 'face 'success))
                              (t "[ ]")))
              ("sending" (if (bndos-review-outbox-unconfirmed-p draft)
                             (propertize " ? " 'face 'bndos-review-outbox-warning)
                           (propertize " … " 'face 'shadow)))
              (_ (propertize " ✓ " 'face 'success)))
            " " (propertize (bndos-review-outbox--describe-target review draft) 'face 'bold) "\n"
            "    " (propertize (format "by %s · %s" (plist-get draft :author)
                                       (diff-review--when (plist-get draft :updated)))
                               'face 'shadow)
            (if-let* ((sent (plist-get draft :sent)))
                (concat "  " (propertize (or (plist-get sent :url) "") 'face 'link))
              "")
            "\n")
    (when-let* ((changes (plist-get draft :changes)))
      (insert "    " (propertize (format "Changes: %s%s · v to review them" (bndos-review-changes-count changes)
                                         (if (plist-get changes :rules)
                                             (concat ", " (bndos-review-outbox--rules-text changes)) ""))
                                 'face 'shadow)
              "\n"))
    (when (equal state "sending")
      (insert "    " (propertize (if (bndos-review-outbox-unconfirmed-p draft)
                                     (format "unconfirmed (%s): it may be on GitHub. g fetches and settles it if it is; R puts it back among the drafts"
                                             (or (plist-get draft :error) "the sending Emacs stopped"))
                                   "sending…")
                                 'face 'bndos-review-outbox-warning)
              "\n"))
    (when (equal state "drafts")
      (when stale-mark
        (insert "    " (propertize "changed since you marked it: m to mark it again" 'face 'bndos-review-outbox-warning) "\n"))
      (when (car problems)
        (insert "    " (propertize (concat "can't send: " (car problems)) 'face 'bndos-review-outbox-blocked) "\n"))
      (dolist (w (cdr problems))
        (insert "    " (propertize (concat "⚠ " w) 'face 'bndos-review-outbox-warning) "\n")))
    (unless (equal state "sent")
      (insert (mapconcat (lambda (line) (concat "    " (propertize "│ " 'face 'shadow) line))
                         (split-string (plist-get draft :body) "\n") "\n")
              "\n"))
    (insert "\n")
    (put-text-property start (point) 'bndos-review-outbox-id (plist-get draft :id))
    (put-text-property start (point) 'bndos-review-outbox-hash (bndos-review-outbox-hash draft))))

(defun bndos-review-outbox--render ()
  "Draw this outbox."
  (let* ((inhibit-read-only t)
         (review bndos-review-outbox--review)
         (state (and (buffer-live-p review) (bndos-review-outbox--github review)))
         (drafts (delq nil (bndos-review-outbox-load bndos-review-outbox--store)))
         (id (get-text-property (point) 'bndos-review-outbox-id))
         (line (count-lines (point-min) (point))))
    ;; Marks of drafts gone are dropped; of drafts changed, kept to say so.
    (setq bndos-review-outbox--marks
          (seq-filter (lambda (m) (seq-find (lambda (d) (and (equal (plist-get d :id) (car m))
                                                             (equal (plist-get d :state) "drafts")))
                                            drafts))
                      bndos-review-outbox--marks))
    (erase-buffer)
    (insert (propertize (format "Outbox  %s/%s#%s" (plist-get state :owner) (plist-get state :name)
                                (plist-get state :number))
                        'face 'diff-review-title)
            "  " (propertize (format "posting as %s" (if bndos-review-outbox--login
                                                          (concat "@" bndos-review-outbox--login) "?"))
                             'face 'bold)
            "\n"
            (let ((age (bndos-review-outbox--age review)))
              (propertize (if age (format "GitHub's threads fetched %s ago" (format-seconds "%Y, %D, %H, %M, %z%S" age))
                            "GitHub's threads not fetched")
                          'face (if (and age (< age bndos-review-outbox-max-age)) 'shadow 'bndos-review-outbox-warning)))
            (if (plist-get state :fetching) (propertize "  fetching…" 'face 'shadow) "")
            "\n"
            (diff-review--key-hints '((bndos-review-outbox-mark . "mark") (bndos-review-outbox-mark-all . "mark all")
                                      (bndos-review-outbox-unmark-all . "unmark all") (bndos-review-outbox-edit . "edit")
                                      (bndos-review-outbox-delete . "delete") (bndos-review-outbox-new-comment . "comment")
                                      (bndos-review-outbox-send . "send marked")
                                      (bndos-review-outbox-refresh . "fetch")))
            "\n")
    (when bndos-review-outbox--sending (insert (propertize "Sending…\n" 'face 'bndos-review-outbox-warning)))
    (when bndos-review-outbox--report (insert bndos-review-outbox--report "\n"))
    (insert "\n")
    (let ((groups `(("Drafts" . ,(seq-filter (lambda (d) (equal (plist-get d :state) "drafts")) drafts))
                    ("Being sent or unconfirmed" . ,(seq-filter (lambda (d) (equal (plist-get d :state) "sending")) drafts))
                    ("Sent" . ,(last (seq-filter (lambda (d) (equal (plist-get d :state) "sent")) drafts) 10)))))
      (pcase-dolist (`(,heading . ,items) groups)
        (when (or items (equal heading "Drafts"))
          (insert (propertize (format "%s (%d%s)" heading (length items)
                                      (if (equal heading "Drafts")
                                          (format ", %d marked" (seq-count #'bndos-review-outbox--marked-p items)) ""))
                              'face 'magit-section-heading)
                  "\n")
          (if items (mapc #'bndos-review-outbox--insert-draft items)
            (insert (propertize "  None: r on a GitHub thread drafts a reply\n\n" 'face 'shadow))))))
    (goto-char (or (and id (text-property-any (point-min) (point-max) 'bndos-review-outbox-id id))
                   (progn (goto-char (point-min)) (forward-line line) (point))))))

(defun bndos-review-outbox--draft-at-point ()
  "The draft at point, as it is now; an error if it isn't as shown."
  (let* ((id (or (get-text-property (point) 'bndos-review-outbox-id) (user-error "No draft here")))
         (draft (seq-find (lambda (d) (equal (plist-get d :id) id)) (bndos-review-outbox-load bndos-review-outbox--store))))
    (unless draft
      (bndos-review-outbox--render)
      (user-error "That draft is gone"))
    (unless (equal (bndos-review-outbox-hash draft) (get-text-property (point) 'bndos-review-outbox-hash))
      (bndos-review-outbox--render)
      (user-error "That draft changed since it was shown: look again"))
    draft))

(defun bndos-review-outbox--shown ()
  "The drafts shown, as (ID . HASH) of what is shown."
  (let ((pos (point-min)) shown)
    (while (setq pos (text-property-not-all pos (point-max) 'bndos-review-outbox-id nil))
      (push (cons (get-text-property pos 'bndos-review-outbox-id) (get-text-property pos 'bndos-review-outbox-hash))
            shown)
      (setq pos (or (next-single-property-change pos 'bndos-review-outbox-id) (point-max))))
    (nreverse shown)))

(defun bndos-review-outbox-mark ()
  "Mark the draft at point to be sent, or unmark it; then go to the next."
  (interactive)
  (let ((draft (bndos-review-outbox--draft-at-point)))
    (unless (equal (plist-get draft :state) "drafts") (user-error "Only drafts are sent"))
    (if (bndos-review-outbox--marked-p draft)
        (setq bndos-review-outbox--marks (assoc-delete-all (plist-get draft :id) bndos-review-outbox--marks))
      (when-let* ((blocker (car (bndos-review-outbox-problems bndos-review-outbox--review draft))))
        (user-error "Can't send this one: %s" blocker))
      (setf (alist-get (plist-get draft :id) bndos-review-outbox--marks nil nil #'equal)
            (bndos-review-outbox-hash draft)))
    (bndos-review-outbox--render)
    (when-let* ((next (next-single-property-change (point) 'bndos-review-outbox-id)))
      (goto-char next)
      (unless (get-text-property (point) 'bndos-review-outbox-id)
        (goto-char (or (next-single-property-change (point) 'bndos-review-outbox-id) (point)))))))

(defun bndos-review-outbox-mark-all ()
  "Mark every draft shown that can be sent, as shown."
  (interactive)
  (let ((shown (bndos-review-outbox--shown)) (changed 0))
    (dolist (draft (bndos-review-outbox-load bndos-review-outbox--store))
      (let ((as-shown (cdr (assoc (plist-get draft :id) shown))))
        (when (and (equal (plist-get draft :state) "drafts")
                   (not (car (bndos-review-outbox-problems bndos-review-outbox--review draft))))
          (if (not (equal as-shown (bndos-review-outbox-hash draft)))
              (cl-incf changed)
            (setf (alist-get (plist-get draft :id) bndos-review-outbox--marks nil nil #'equal) as-shown)))))
    (bndos-review-outbox--render)
    (when (> changed 0)
      (message "%d draft%s new or changed since shown, not marked: look again" changed (if (= changed 1) "" "s")))))

(defun bndos-review-outbox-unmark-all ()
  "Unmark every draft."
  (interactive)
  (setq bndos-review-outbox--marks nil)
  (bndos-review-outbox--render))

(defun bndos-review-outbox-edit ()
  "Edit the draft at point."
  (interactive)
  (let ((draft (bndos-review-outbox--draft-at-point)))
    (unless (equal (plist-get draft :state) "drafts") (user-error "Only drafts are edited"))
    (bndos-review-outbox-edit-draft bndos-review-outbox--store draft)))

(defun bndos-review-outbox-delete ()
  "Delete the draft at point, if it is still as shown."
  (interactive)
  (let ((draft (bndos-review-outbox--draft-at-point)))
    (unless (equal (plist-get draft :state) "drafts") (user-error "Only drafts are deleted"))
    (when (y-or-n-p (format "Delete the draft by %s, “%s”? " (plist-get draft :author)
                            (truncate-string-to-width (car (split-string (plist-get draft :body) "\n")) 40 nil nil "…")))
      (let ((result (bndos-review-outbox-remove bndos-review-outbox--store (plist-get draft :id)
                                                (bndos-review-outbox-hash draft))))
        (bndos-review-outbox--render)
        (unless (eq result t) (user-error "Not deleted: %s" result))))))

(defun bndos-review-outbox-return ()
  "Put the unconfirmed draft at point back among the drafts, once you know it
isn't on GitHub.  Also one another Emacs, still running, claimed long ago."
  (interactive)
  (let* ((draft (bndos-review-outbox--draft-at-point))
         (store bndos-review-outbox--store)
         (id (plist-get draft :id))
         (stuck (bndos-review-outbox--stuck-p draft))
         (returnable (lambda (d) (and (plist-get d :body)
                                      (or (bndos-review-outbox-unconfirmed-p d)
                                          (and stuck (bndos-review-outbox--stuck-p d)))))))
    (unless (funcall returnable draft) (user-error "Not an unconfirmed draft"))
    (when (yes-or-no-p
           (if stuck
               (format "Emacs %s claimed it %s and still runs: is it on GitHub?  Put it back only if it isn't, and that Emacs isn't sending it: "
                       (plist-get (plist-get draft :claim) :pid)
                       (diff-review--when (plist-get (plist-get draft :claim) :at)))
             "Put it back among the drafts?  Only if it isn't on GitHub, or it may be sent twice: "))
      (let* ((file (bndos-review-outbox--path store "sending" id))
             (held (bndos-review-outbox--held store id))
             (draft (and (bndos-review-outbox--move file held)
                         (append (list :state "sending" :file held) (bndos-review-outbox--read held)))))
        (cond
         ((not draft) (bndos-review-outbox--render) (user-error "It moved meanwhile: look again"))
         ((not (funcall returnable draft))
          (bndos-review-outbox--move held file)
          (bndos-review-outbox--render)
          (user-error "It is being sent, or can't be read: look again")))
        (bndos-review-outbox--write held (plist-put (plist-put draft :claim nil) :error "put back after an unconfirmed send"))
        (unless (bndos-review-outbox--move held (bndos-review-outbox--path store "drafts" id))
          (bndos-review-outbox--move held file)
          (user-error "A draft with its id is there already: look again"))
        (bndos-review-outbox-changed store)
        (bndos-review-outbox--render)))))

(defun bndos-review-outbox-new-comment ()
  "Draft a comment on the pull request's conversation."
  (interactive)
  (bndos-review-outbox-comment bndos-review-outbox--review))

(defun bndos-review-outbox-visit ()
  "Show the thread the draft at point replies to."
  (interactive)
  (let* ((draft (bndos-review-outbox--draft-at-point))
         (thread (plist-get (plist-get draft :target) :thread))
         (review bndos-review-outbox--review))
    (pop-to-buffer review)
    (if thread
        (diff-review--goto-comment (concat "github:" thread))
      (goto-char (point-min))
      (re-search-forward "^Comments" nil t))))

(defun bndos-review-outbox-refresh (&optional then)
  "Fetch GitHub's threads again, settling unconfirmed sends; then THEN."
  (interactive)
  (let ((outbox (current-buffer))
        (review bndos-review-outbox--review))
    (bndos-review-outbox--whoami outbox)
    (bndos-review-github-refresh review t)
    (bndos-review-outbox--render)
    (bndos-review-outbox--when-fetched
     review (lambda ()
              (when (buffer-live-p outbox)
                (with-current-buffer outbox
                  (bndos-review-outbox--render)
                  (when then (funcall then))))))))

(defun bndos-review-outbox--when-fetched (review fn)
  "Call FN once REVIEW's fetch is over."
  (let ((timer nil) (tries 0))
    (setq timer (run-with-timer 0.2 0.2
                                (lambda ()
                                  (cl-incf tries)
                                  (when (or (not (buffer-live-p review)) (> tries 600)
                                            (not (plist-get (bndos-review-outbox--github review) :fetching)))
                                    (cancel-timer timer)
                                    (funcall fn)))))))

(defun bndos-review-outbox--whoami (outbox)
  (with-current-buffer bndos-review-outbox--review
    (bndos-review-github--gh '("api" "user")
                             (lambda (user)
                               (when (and (hash-table-p user) (buffer-live-p outbox))
                                 (with-current-buffer outbox
                                   (setq bndos-review-outbox--login (gethash "login" user))
                                   (bndos-review-outbox--render)))))))

(defun bndos-review-outbox-send ()
  "Send the drafts marked, as they were when marked, after a confirmation."
  (interactive)
  (when bndos-review-outbox--sending (user-error "Already sending"))
  (let* ((outbox (current-buffer))
         (review bndos-review-outbox--review)
         (store bndos-review-outbox--store)
         (drafts (bndos-review-outbox-load store))
         (marks (seq-filter (lambda (m) (seq-find (lambda (d) (and (equal (plist-get d :id) (car m))
                                                                   (equal (plist-get d :state) "drafts")
                                                                   (equal (bndos-review-outbox-hash d) (cdr m))))
                                                  drafts))
                            bndos-review-outbox--marks))
         (age (bndos-review-outbox--age review)))
    (cond
     ((null bndos-review-outbox--marks) (user-error "Nothing marked: m marks a draft"))
     ((plist-get (bndos-review-outbox--github review) :fetching)
      (user-error "Fetching GitHub's threads: look again once they're in, then send"))
     ((/= (length marks) (length bndos-review-outbox--marks))
      (bndos-review-outbox--render)
      (user-error "Marked drafts changed or went since you marked them: look again, then send"))
     ((or (null age) (> age bndos-review-outbox-max-age))
      (bndos-review-outbox-refresh)
      (message "GitHub's threads were old: fetching them; look at the outbox again, then send"))
     ((let ((bndos-review-changes-cache-seconds 0))   ; the code as it is now
        (seq-some (lambda (m) (car (bndos-review-outbox-problems
                                    review (seq-find (lambda (d) (equal (plist-get d :id) (car m))) drafts))))
                  marks))
      (user-error "A marked draft can't be sent: see the outbox"))
     ((y-or-n-p (format "Send the %d marked draft%s to %s/%s#%s as %s? "
                        (length marks) (if (= 1 (length marks)) "" "s")
                        (plist-get (bndos-review-outbox--github review) :owner)
                        (plist-get (bndos-review-outbox--github review) :name)
                        (plist-get (bndos-review-outbox--github review) :number)
                        (if bndos-review-outbox--login (concat "@" bndos-review-outbox--login) "this project's GitHub account")))
      (setq bndos-review-outbox--sending t
            bndos-review-outbox--report nil
            bndos-review-outbox--marks nil)
      (bndos-review-outbox--render)
      (bndos-review-outbox-send-marks
       review store marks
       (lambda (results)
         (when (buffer-live-p outbox)
           (with-current-buffer outbox
             (setq bndos-review-outbox--sending nil
                   bndos-review-outbox--report (bndos-review-outbox--summary results))
             (bndos-review-outbox--render)
             (message "%s" (substring-no-properties bndos-review-outbox--report))))
         (when (buffer-live-p review) (bndos-review-github-refresh review t))))))))

(defun bndos-review-outbox--summary (results)
  (let ((count (lambda (kind) (seq-count (lambda (r) (eq (cadr r) kind)) results))))
    (string-join
     (delq nil
           (list (and (> (funcall count 'sent) 0) (propertize (format "%d sent" (funcall count 'sent)) 'face 'success))
                 (and (> (funcall count 'failed) 0)
                      (propertize (format "%d failed, kept as drafts" (funcall count 'failed)) 'face 'error))
                 (and (> (funcall count 'unknown) 0)
                      (propertize (format "%d unconfirmed: check GitHub" (funcall count 'unknown)) 'face 'warning))
                 (and (> (funcall count 'skipped) 0)
                      (propertize (format "%d not sent: %s" (funcall count 'skipped)
                                          (string-join (seq-uniq (mapcar #'caddr (seq-filter (lambda (r) (eq (cadr r) 'skipped)) results)))
                                                       "; "))
                                  'face 'warning))))
     ", ")))

(defvar-keymap bndos-review-outbox-mode-map
  :doc "Keys of the outbox."
  :parent special-mode-map
  "m" #'bndos-review-outbox-mark
  "M" #'bndos-review-outbox-mark-all
  "U" #'bndos-review-outbox-unmark-all
  "e" #'bndos-review-outbox-edit
  "d" #'bndos-review-outbox-delete
  "c" #'bndos-review-outbox-new-comment
  "R" #'bndos-review-outbox-return
  "RET" #'bndos-review-outbox-visit
  "v" #'bndos-review-outbox-view-changes
  "u" #'bndos-review-outbox-unexplained
  "g" #'bndos-review-outbox-refresh
  "C-c C-c" #'bndos-review-outbox-send)

(define-derived-mode bndos-review-outbox-mode special-mode "Outbox"
  "Drafted replies to a pull request, sent from here once marked and confirmed.
\\{bndos-review-outbox-mode-map}"
  (setq truncate-lines nil)
  (setq-local revert-buffer-function (lambda (&rest _) (bndos-review-outbox-refresh))))

(with-eval-after-load 'evil
  (evil-set-initial-state 'bndos-review-outbox-mode 'normal)
  (evil-define-key* 'normal bndos-review-outbox-mode-map
    "m" #'bndos-review-outbox-mark
    "M" #'bndos-review-outbox-mark-all
    "U" #'bndos-review-outbox-unmark-all
    "e" #'bndos-review-outbox-edit
    "d" #'bndos-review-outbox-delete
    "c" #'bndos-review-outbox-new-comment
    "R" #'bndos-review-outbox-return
    (kbd "RET") #'bndos-review-outbox-visit
    "v" #'bndos-review-outbox-view-changes
    "u" #'bndos-review-outbox-unexplained
    "gr" #'bndos-review-outbox-refresh
    "q" #'quit-window))

;;;; The changes a draft links

(defun bndos-review-outbox--rules-text (changes)
  (mapconcat (lambda (r) (if (plist-member r :in)
                             (format "all in %s" (string-join (plist-get r :in) ", "))
                           (format "all matching `%s'" (plist-get r :matching))))
             (plist-get changes :rules) ", "))

(defun bndos-review-outbox--changes-name (review)
  (let ((state (bndos-review-outbox--github review)))
    (format "changes %s/%s#%s" (plist-get state :owner) (plist-get state :name) (plist-get state :number))))

(defun bndos-review-outbox--quote (text)
  (mapconcat (lambda (line) (concat "  │ " line)) (split-string (string-trim-right (or text "")) "\n") "\n"))

(defun bndos-review-outbox--changes-file (store)
  "The review file of the changes shown for STORE's pull request: local, one."
  (expand-file-name "changes-review.json" store))

(defun bndos-review-outbox-show-changes (review draft)
  "Review the changes DRAFT links, in REVIEW's worktree as checked out: a
review of its own, in a workspace of its own, of those changes alone, with
the thread DRAFT answers and DRAFT."
  (let* ((changes (or (plist-get draft :changes) (user-error "This draft links no changes")))
         (root (buffer-local-value 'diff-review--root review))
         (store (bndos-review-outbox-store review))
         (id (plist-get draft :id))
         (target (plist-get draft :target))
         (thread (plist-get target :thread))
         (borrow (cons review (if thread
                                  (lambda (c) (equal (gethash "thread-id" c) thread))
                                (lambda (c) (equal (gethash "id" c) (concat "draft:" id))))))
         (bndos-review-github--borrow-next borrow))
    (bndos-review-changes-open
     (bndos-review-outbox--changes-name review) root (bndos-review-outbox--changes-file store)
     (list
      :name "the reply's changes"
      :compute
      (lambda ()
        (let* ((draft (or (seq-find (lambda (d) (equal (plist-get d :id) id)) (bndos-review-outbox-load store)) draft))
               (changes (or (plist-get draft :changes) changes))
               (base (plist-get changes :base))
               (view (bndos-review-changes-view root changes))
               (heading (concat (if thread
                                    (format "Reply on %s:%s" (plist-get target :path) (or (plist-get target :line) ""))
                                  (if-let* ((answers (plist-get target :answers)))
                                      (format "Comment answering %s" answers)
                                    "Comment on the pull request"))
                                (format ", by %s, %s" (plist-get draft :author)
                                        (if (equal (plist-get draft :state) "sent") "sent" "not sent")))))
          (if (null view)
              (list :hunks nil
                    :notes (list (cons heading 'bndos-review-changes-heading)
                                 (cons (format "Its changes are from %s, which isn't here: fetch it to see them"
                                               (substring base 0 12))
                                       'warning)))
            (pcase-let ((`(,summary . ,warnings) (bndos-review-changes-summary view)))
              (list :hunks (mapcar #'car (plist-get view :hunks))
                    :notes (append
                            (list (cons heading 'bndos-review-changes-heading)
                                  (cons (format "Its changes: %s, from %s (where it was commented) to the worktree as checked out"
                                                summary (substring base 0 12))
                                        'shadow))
                            (and (plist-get changes :rules)
                                 (list (cons (format "Linked: %s" (bndos-review-outbox--rules-text changes)) 'shadow)))
                            (mapcar (lambda (w) (cons w 'warning)) warnings)
                            (mapcar (lambda (link) (cons (format "%s:%d: gone, the code is back as the reviewer saw it"
                                                                 (plist-get link :path) (plist-get link :from))
                                                         'warning))
                                    (plist-get view :gone)))))))))
     (lambda () (bndos-review-outbox--borrow borrow)))))

(defun bndos-review-outbox--borrow (borrow)
  "Make this review show BORROW's threads, see `bndos-review-github-borrow'."
  (setq bndos-review-github-borrow borrow)
  (bndos-review-github--borrow (current-buffer)))

(defun bndos-review-outbox-view-changes ()
  "Show the changes the draft at point links."
  (interactive)
  (bndos-review-outbox-show-changes bndos-review-outbox--review (bndos-review-outbox--draft-at-point)))

(defun bndos-review-outbox-thread-changes ()
  "Show the changes the draft replying to the thread at point links."
  (interactive)
  (let* ((review (diff-review--in-review (current-buffer)))
         (comment (with-current-buffer review (diff-review--thread-here)))
         (thread (and comment (gethash "thread-id" comment)))
         (store (or (bndos-review-outbox-store review) (user-error "No pull request fetched for this review")))
         (draft (seq-find (lambda (d) (and (plist-get d :changes)
                                           (if thread (equal (plist-get (plist-get d :target) :thread) thread)
                                             (equal (plist-get d :id) (and comment (gethash "draft-id" comment))))))
                          (bndos-review-outbox--live store))))
    (unless draft (user-error "No draft here links changes"))
    (bndos-review-outbox-show-changes review draft)))

(defun bndos-review-outbox-unexplained ()
  "Show the changes no draft links, since the oldest commit a draft's changes
are taken from; those the base branch brought left out."
  (interactive)
  (let* ((review bndos-review-outbox--review)
         (store bndos-review-outbox--store)
         (root (buffer-local-value 'diff-review--root review))
         (all (delq nil (mapcar (lambda (d) (plist-get d :changes)) (bndos-review-outbox--live store)))))
    (unless all (user-error "No draft links changes"))
    (let* ((borrow (cons review #'ignore))
           (bndos-review-github--borrow-next borrow))
      (bndos-review-changes-open
       (bndos-review-outbox--changes-name review) root (bndos-review-outbox--changes-file store)
       (list
        :name "the changes no draft explains"
        :compute
        (lambda ()
          (let* ((all (delq nil (mapcar (lambda (d) (plist-get d :changes)) (bndos-review-outbox--live store))))
                 (base (bndos-review-outbox--oldest-base root (mapcar (lambda (c) (plist-get c :base)) all)))
                 (base-oid (plist-get (bndos-review-outbox--github review) :base-oid))
                 (upstream (and base base-oid
                                (list (string-trim (or (bndos-review-changes--git root "merge-base" base base-oid) ""))
                                      (string-trim (or (bndos-review-changes--git root "merge-base" "HEAD" base-oid) "")))))
                 (hunks (and base (bndos-review-changes-unexplained
                                   root base all (and upstream (not (member "" upstream)) upstream)))))
            (list :hunks hunks
                  :notes (list (cons "Changes no draft explains" 'bndos-review-changes-heading)
                               (cons (if base
                                         (format "From %s to the worktree as checked out · %d change%s%s"
                                                 (substring base 0 12) (length hunks) (if (= (length hunks) 1) "" "s")
                                                 (if upstream ", the base branch's own left out" ""))
                                       "No draft links changes now")
                                     'shadow))))))
       (lambda () (bndos-review-outbox--borrow borrow))))))

(defun bndos-review-outbox--oldest-base (root bases)
  "Of BASES, the one the others descend from, or the first."
  (let ((bases (seq-uniq bases)))
    (or (seq-find (lambda (b) (seq-every-p (lambda (o) (or (equal o b)
                                                            (bndos-review-changes--git root "merge-base" "--is-ancestor" b o)))
                                           bases))
                  bases)
        (car bases))))

(defun bndos-review-outbox ()
  "Show the outbox of this review's pull request: its drafts, to mark and send."
  (interactive)
  (let* ((review (diff-review--in-review (current-buffer)))
         (store (or (bndos-review-outbox-store review) (user-error "No pull request fetched for this review")))
         (state (bndos-review-outbox--github review))
         (buffer (get-buffer-create (format "*outbox: %s/%s#%s*" (plist-get state :owner) (plist-get state :name)
                                            (plist-get state :number)))))
    (bndos-review-outbox--watch store)
    (with-current-buffer buffer
      (unless (derived-mode-p 'bndos-review-outbox-mode) (bndos-review-outbox-mode))
      (setq bndos-review-outbox--review review
            bndos-review-outbox--store store)
      (bndos-review-outbox-refresh))
    (pop-to-buffer buffer)
    buffer))

(setq diff-review-thread-drafts-function #'bndos-review-outbox--thread-drafts
      diff-review-external-reply-function #'bndos-review-outbox-reply)

(provide 'bndos-review-outbox)
;;; bndos-review-outbox.el ends here
