;;; bndos-review-outbox.el --- What to say on a pull request: drafted, picked, sent -*- lexical-binding: t; -*-

;;; Commentary:

;; What the user says on a pull request is a draft first: a reply in a
;; GitHub thread, a comment on its conversation, a comment on lines or a
;; file, or the summary of a review.  Written here (r on a thread, C-c C-g
;; c on lines) or by an agent (`bndos-task drafts'), each is shown where it
;; goes, a comment of the review like any other: edited, deleted or
;; quoted at point.  C-c C-r sends those the user picks, after a
;; confirmation: comments on lines and the summary as one GitHub review.
;; An agent's draft the user edits is the user's.
;;
;; A draft is a file, its folder its state, under the repository's common
;; git directory, in review/github/OWNER/NAME/NUMBER/:
;;
;;   drafts/ID.json    written, to edit, delete or send
;;   sending/ID.json   claimed by a send; nothing else touches it
;;   sent/ID.json      on GitHub, its link kept
;;
;; Moving a file is atomic, so one send claims a draft, once.  Picking one
;; keeps the hash of the draft as asked about; the send claims the draft and
;; sends it only when it is still that one, so a draft changed or deleted
;; after it was picked, by an agent or another Emacs, is not sent.  An edit or a deletion
;; claims the draft too, and checks it is the one the user saw.
;;
;; A sent draft ends with an invisible marker, <!-- bndos-draft:ID -->.  A
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

(defgroup bndos-review-outbox nil
  "Drafted replies to pull requests."
  :group 'diff-review)

(defcustom bndos-review-outbox-max-age 120
  "Seconds GitHub's threads may be old when drafts are sent; older, they are
fetched again first, before anything is asked or sent."
  :type 'integer)

(defface bndos-review-outbox-warning '((t :inherit warning))
  "Warnings about a draft.")

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
  "What picking or editing keeps of DRAFT: the hash of its body, and of the
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

(defun bndos-review-outbox-change (store id hash body &optional author)
  "Make BODY draft ID's, and AUTHOR its author when given, if it is still the
one HASH names: t, or a reason."
  (let ((held (bndos-review-outbox--hold store id hash)))
    (if (not (file-name-absolute-p held))
        held
      (let ((draft (bndos-review-outbox--read held)))
        (when author (setq draft (plist-put draft :author author)))
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
put back by hand (`bndos-review-draft-put-back'): that Emacs lost track of it."
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
        "sent, deleted or being changed since you chose it"
      (let ((draft (bndos-review-outbox--read held)))
        (cond
         ((not (equal (bndos-review-outbox-hash draft) hash))
          (bndos-review-outbox--move held from)
          "changed since you chose it")
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
      ("file" (format "%s/pulls/%d/comments" repo (plist-get pr :number)))
      ((or "line" "summary") (format "%s/pulls/%d/reviews" repo (plist-get pr :number)))
      (kind (error "Unknown kind of draft: %s" kind)))))

(defun bndos-review-outbox--review-kind-p (draft)
  "Whether DRAFT goes with a review: a comment on lines, or the summary."
  (member (plist-get (plist-get draft :target) :kind) '("line" "summary")))

(defun bndos-review-outbox--payload (draft)
  "What is posted of DRAFT, alone: its body, with its marker."
  (let ((body (concat (plist-get draft :body) (bndos-review-outbox--marker (plist-get draft :id))))
        (target (plist-get draft :target)))
    (pcase (plist-get target :kind)
      ("file" (list :body body :commit_id (plist-get target :commit) :path (plist-get target :path)
                    :subject_type "file"))
      (_ (list :body body)))))

(defun bndos-review-outbox--review-payload (drafts event head)
  "The review DRAFTS make, comments on lines and a summary, with EVENT, on
HEAD, the pull request's head commit."
  (let ((summary (seq-find (lambda (d) (equal (plist-get (plist-get d :target) :kind) "summary")) drafts)))
    (list :commit_id head :event event
          :body (if summary (concat (plist-get summary :body) (bndos-review-outbox--marker (plist-get summary :id))) "")
          :comments
          (vconcat
           (cl-loop for d in drafts
                    for target = (plist-get d :target)
                    when (equal (plist-get target :kind) "line")
                    collect (append (list :path (plist-get target :path)
                                          :body (concat (plist-get d :body) (bndos-review-outbox--marker (plist-get d :id)))
                                          :line (plist-get target :line)
                                          :side (or (plist-get target :side) "RIGHT"))
                                    (when-let* ((start (plist-get target :start_line)))
                                      (list :start_line start :start_side (or (plist-get target :side) "RIGHT")))))))))

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

(defun bndos-review-outbox--post-to (review endpoint payload marker callback)
  "Post PAYLOAD to ENDPOINT, for REVIEW's pull request; then CALLBACK with its
outcome.  The outcome goes by the HTTP status GitHub answered with: a 2xx is
sent, a 4xx refused (nothing was posted); anything else, no answer or a 5xx,
may or may not have been posted.  MARKER, when given, is what the body
GitHub keeps should end with."
  (with-current-buffer review
    (unless (executable-find bndos-review-github-program)
      (error "No %s to post with" bndos-review-github-program))
    (let ((input (make-temp-file "bndos-outbox-" nil ".json")))
      (bndos-review-outbox--write-json input payload)
      (bndos-review-github--gh
       (list "api" "--include" "-X" "POST" endpoint "--input" input)
       (lambda (result)
         (ignore-errors (delete-file input))
         (pcase-let ((`(,status . ,body) (condition-case nil (bndos-review-outbox--response result)
                                           (error (cons nil nil)))))
           (funcall callback
                    (cond ((and status (<= 200 status 299) (hash-table-p body) (gethash "id" body))
                           (when (and marker
                                      (not (string-suffix-p (string-trim-left marker)
                                                            (string-trim-right (or (gethash "body" body) "")))))
                             (display-warning 'bndos-review-outbox
                                              "GitHub didn't keep the hidden tag of a draft: a send left unconfirmed can't be checked"))
                           (list 'sent (gethash "html_url" body) (gethash "id" body)))
                          ((and status (<= 400 status 499))
                           (list 'failed (bndos-review-outbox--failure status body result)))
                          (status (list 'unknown (format "HTTP %d" status)))
                          (t (list 'unknown (let ((err (plist-get result :err)))
                                              (if (string-empty-p (or err "")) "no answer" err))))))))
       t))))

(defun bndos-review-outbox--post (review draft callback)
  "Post DRAFT, alone, to REVIEW's pull request; then CALLBACK with its outcome."
  (bndos-review-outbox--post-to review (bndos-review-outbox--endpoint draft) (bndos-review-outbox--payload draft)
                                (bndos-review-outbox--marker (plist-get draft :id)) callback))

(defun bndos-review-outbox--post-review (review drafts event callback)
  "Post DRAFTS as one review with EVENT, on the pull request's head as REVIEW
has it; then CALLBACK with its outcome."
  (let ((head (or (plist-get (bndos-review-outbox--github review) :head-oid)
                  (error "The pull request's head isn't known")))
        (summary (seq-find (lambda (d) (equal (plist-get (plist-get d :target) :kind) "summary")) drafts)))
    (bndos-review-outbox--post-to review (bndos-review-outbox--endpoint (car drafts))
                                  (bndos-review-outbox--review-payload drafts event head)
                                  (and summary (bndos-review-outbox--marker (plist-get summary :id)))
                                  callback)))

(defun bndos-review-outbox-send-marks (review store marks done &optional event)
  "Send the drafts MARKS names, (ID . HASH) each, to REVIEW's pull request:
replies, comments and comments on files one at a time; comments on lines
and the summary then as one review, with EVENT (default COMMENT).  Then
DONE with (ID . OUTCOME) each, OUTCOME as `bndos-review-outbox--settle'
takes it or (skipped REASON).  DONE is called whatever goes wrong; a draft
claimed is always settled."
  (let* ((results nil)
         (pr (bndos-review-outbox--pr review))
         (drafts (bndos-review-outbox-load store))
         (reviewed (seq-filter (lambda (m) (bndos-review-outbox--review-kind-p
                                            (seq-find (lambda (d) (equal (plist-get d :id) (car m))) drafts)))
                               marks))
         (alone (seq-difference marks reviewed)))
    (cl-labels
        ((claim (id hash)
           (condition-case err (bndos-review-outbox--claim store id hash pr)
             (error (format "not claimed: %s" (error-message-string err)))))
         (settle (claimed outcome)
           (dolist (draft claimed)
             (let ((id (plist-get draft :id)))
               (setq bndos-review-outbox--in-flight (delete id bndos-review-outbox--in-flight))
               (condition-case err (bndos-review-outbox--settle store draft outcome)
                 (error (display-warning 'bndos-review-outbox
                                         (format "Draft %s: %S, not recorded: %s" id outcome
                                                 (error-message-string err)))))
               (push (cons id outcome) results))))
         (send (claimed post then)
           ;; POST calls back once; before gh starts, nothing was posted.
           (dolist (draft claimed) (push (plist-get draft :id) bndos-review-outbox--in-flight))
           (ignore-errors (bndos-review-outbox-changed store))
           (let ((finished nil))
             (cl-flet ((finish (outcome)
                         (unless finished
                           (setq finished t)
                           (settle claimed outcome)
                           (funcall then))))
               (condition-case err (funcall post #'finish)
                 (error (finish (list 'failed (format "not sent: %s" (error-message-string err)))))))))
         (next (rest)
           (if (null rest)
               (send-review)
             (pcase-let* ((`(,id . ,hash) (car rest))
                          (draft (claim id hash)))
               (if (stringp draft)
                   (progn (push (cons id (list 'skipped draft)) results)
                          (next (cdr rest)))
                 (send (list draft) (lambda (finish) (bndos-review-outbox--post review draft finish))
                       (lambda () (next (cdr rest))))))))
         (send-review ()
           (let (claimed)
             (pcase-dolist (`(,id . ,hash) reviewed)
               (let ((draft (claim id hash)))
                 (if (stringp draft)
                     (push (cons id (list 'skipped draft)) results)
                   (push draft claimed))))
             (setq claimed (nreverse claimed))
             (if (null claimed)
                 (funcall done (nreverse results))
               (send claimed
                     (lambda (finish) (bndos-review-outbox--post-review review claimed (or event "COMMENT") finish))
                     (lambda () (funcall done (nreverse results))))))))
      (next alone))))

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
                             ("comment" (eq where 'conversation))
                             ;; A thread of its own.
                             ((or "line" "file") (stringp where))
                             ("summary" (eq where 'review)))
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

(defvar bndos-review-outbox--blobs (make-hash-table :test #'equal)
  "(ROOT REV PATH) -> the blob, or `none'.")

(defun bndos-review-outbox--blob (rev path)
  "The blob of PATH at REV here, or nil; from git once."
  (let* ((key (list default-directory rev path))
         (hit (gethash key bndos-review-outbox--blobs)))
    (if hit
        (and (not (eq hit 'none)) hit)
      (let ((blob (bndos-review-github--git "rev-parse" "--verify" "--quiet" (concat rev ":" path))))
        ;; A commit not here yet may be fetched: only a blob found is kept.
        (when blob (puthash key blob bndos-review-outbox--blobs))
        blob))))

(defun bndos-review-outbox--draft-thread (draft base)
  "DRAFT, a comment on lines or a file, as a thread of the review, on the
lines it is on in the commit it was written on (or, on removed lines, that
commit's merge base with BASE).  `default-directory' is the review's root."
  (let* ((c (bndos-review-outbox--message draft))
         (target (plist-get draft :target))
         (path (plist-get target :path))
         (line (plist-get target :line))
         (commit (plist-get target :commit))
         (old (equal (plist-get target :side) "LEFT")))
    (puthash "id" (concat "draft:" (plist-get draft :id)) c)
    (puthash "at" (plist-get draft :created) c)
    (puthash "path" path c)
    (puthash "external" "GitHub" c)
    (puthash "chip" "Draft" c)
    (puthash "chip-face" 'diff-review-draft c)
    (puthash "replies" [] c)
    (when line
      (puthash "lines" (vector (or (plist-get target :start_line) line) line) c)
      (let* ((rev (if old (and base commit (bndos-review-github--git "merge-base" base commit)) commit))
             (blob (and rev (bndos-review-outbox--blob rev path))))
        (if (not blob)
            (puthash "outdated" t c)
          (puthash "blob" blob c)
          (when old (puthash "base-blob" blob c))))
      (when old (puthash "side" "old" c)))
    c))

(defun bndos-review-outbox-comments (review)
  "REVIEW's drafts that aren't replies, as comments of the review: comments on
the conversation and the review's summary in its timeline, comments on lines
and files on them."
  (when-let* ((store (bndos-review-outbox-store review)))
    (with-current-buffer review
      (let ((default-directory diff-review--root)
            (base (plist-get bndos-review-github--state :base-oid)))
        (delq nil
              (mapcar (lambda (draft)
                        (pcase (plist-get (plist-get draft :target) :kind)
                          ((or "comment" "summary")
                           (let ((m (bndos-review-outbox--message draft))
                                 (summary (equal (plist-get (plist-get draft :target) :kind) "summary")))
                             (puthash "id" (concat "draft:" (plist-get draft :id)) m)
                             (puthash "at" (plist-get draft :created) m)
                             (puthash "external" "GitHub" m)
                             (puthash "note" t m)
                             (puthash "where" "conversation" m)
                             (puthash "chip" (if summary "Draft review" "Draft") m)
                             (puthash "chip-face" 'diff-review-draft m)
                             m))
                          ((or "line" "file") (bndos-review-outbox--draft-thread draft base))))
                      (bndos-review-outbox--live store)))))))

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
      (when (and (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'diff-review-mode)
                 (member (ignore-errors (bndos-review-outbox-store buffer)) stores))
        (bndos-review-github--show buffer)
        (diff-review--panel-refresh)))))

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

(defun bndos-review-outbox-edit-draft (store draft &optional author)
  "Edit DRAFT of STORE, AUTHOR's from then on when given; keep the text in the
kill ring if it can't be saved."
  (let* ((hash (bndos-review-outbox-hash draft))
         (body (bndos-review-outbox--read-body (format "Edit the draft%s"
                                                       (if (and author (not (equal author (plist-get draft :author))))
                                                           (format " (%s's, yours once saved)" (plist-get draft :author))
                                                         ""))
                                               (plist-get draft :body)))
         (result (bndos-review-outbox-change store (plist-get draft :id) hash body author)))
    (unless (eq result t)
      (kill-new body)
      (user-error "Draft not saved: %s; your text is in the kill ring" result))
    (message "Draft saved: %s sends it" (substitute-command-keys "\\[bndos-review-send]"))))

(defun bndos-review-outbox--drafts-of (store comment &optional states)
  "The drafts of STORE that COMMENT shows, in STATES (default all not sent):
COMMENT itself, a draft, or the replies drafted in its thread."
  (let ((id (gethash "draft-id" comment))
        (thread (gethash "thread-id" comment)))
    (seq-filter (lambda (d) (and (or (null states) (member (plist-get d :state) states))
                                 (if id (equal (plist-get d :id) id)
                                   (and thread (equal (plist-get (plist-get d :target) :thread) thread)))))
                (bndos-review-outbox--live store))))

(defun bndos-review-outbox--pick (drafts what)
  "One of DRAFTS, asked for when there are several, to WHAT."
  (if (cdr drafts)
      (let* ((names (mapcar (lambda (d) (cons (format "%s: %s  [%s]" (plist-get d :author)
                                                      (truncate-string-to-width
                                                       (car (split-string (plist-get d :body) "\n")) 50 nil nil "…")
                                                      (plist-get d :id))
                                                  d))
                            drafts)))
        (cdr (assoc (completing-read (format "Draft to %s: " what) (mapcar #'car names) nil t) names)))
    (car drafts)))

(defun bndos-review-outbox-reply (comment)
  "Draft a reply to COMMENT, a GitHub thread or comment
\(`diff-review-external-reply-function'); a draft there is edited, yours
once saved."
  (let* ((review (diff-review--in-review (current-buffer)))
         (store (or (bndos-review-outbox-store review) (user-error "No pull request fetched for this review")))
         (own (with-current-buffer review (bndos-review-outbox--author)))
         (thread (gethash "thread-id" comment))
         (drafts (bndos-review-outbox--drafts-of store comment '("drafts"))))
    (cond
     (drafts (bndos-review-outbox-edit-draft store (bndos-review-outbox--pick drafts "edit") own))
     ((gethash "draft-id" comment) (user-error "That draft is being sent"))
     (thread (bndos-review-outbox--new-reply review store comment nil))
     ;; GitHub has no replies to the conversation's comments and reviews:
     ;; a comment of its own, quoting all of it, to answer it point by point.
     (t (bndos-review-outbox-comment review (concat (bndos-review-outbox--quoted (gethash "body" comment)) "\n\n")
                                     (gethash "author" comment))))))

(defun bndos-review-outbox--quoted (text)
  "TEXT as a quote, each line with \"> \"."
  (mapconcat (lambda (line) (if (string-empty-p (string-trim line)) ">" (concat "> " line)))
             (split-string (string-trim-right (or text "")) "\n")
             "\n"))

(defun bndos-review-outbox--new-reply (review store comment initial)
  "Draft a reply in COMMENT's thread, starting with INITIAL."
  (unless (gethash "reply-to" comment) (user-error "This thread has no comment to reply to"))
  (let ((body (bndos-review-outbox--read-body (format "Reply to %s (a draft)" (gethash "author" comment)) initial)))
    (when (and initial (equal body (string-trim initial)))
      (user-error "Nothing said but the quote"))
    (bndos-review-outbox-create
     store (list :pr (bndos-review-outbox--pr review)
                 :target (list :kind "reply" :thread (gethash "thread-id" comment) :comment (gethash "reply-to" comment)
                               :path (gethash "path" comment)
                               :line (cdr (diff-review--comment-lines comment)))
                 :body body :author (with-current-buffer review (bndos-review-outbox--author))
                 :seen (bndos-review-outbox--last-at (cons comment (append (gethash "replies" comment) nil)))))
    (message "Reply drafted: %s sends it" (substitute-command-keys "\\[bndos-review-send]"))))

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
    (message "Comment drafted: %s sends it" (substitute-command-keys "\\[bndos-review-send]"))))

;;;; What to know of a draft

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
           (setq blocker "you have a pending review on this pull request on GitHub: submit or discard it there, then fetch again"))
          (t
           (let ((new (seq-count (lambda (m) (> (diff-review--time (gethash "at" m)) seen))
                                 (cons thread (append (gethash "replies" thread) nil)))))
             (when (> new 0)
               (push (format "%d new message%s in the thread since it was drafted" new (if (= new 1) "" "s"))
                     warnings)))
           (when (gethash "resolved" thread) (push "the thread is resolved on GitHub" warnings))
           (when (gethash "github-outdated" thread) (push "the thread is on outdated lines" warnings))))))
      ((or "line" "file" "summary")
       (let ((head (plist-get (bndos-review-outbox--github review) :head-oid))
             (commit (plist-get target :commit)))
         (cond
          ((plist-get (bndos-review-outbox--github review) :pending-review)
           (setq blocker "you have a pending review on this pull request on GitHub: submit or discard it there, then fetch again"))
          ((and commit (not (equal commit head)))
           (setq blocker (format "written on %s, the pull request's head then; it is %s now: its lines may have moved, write it again"
                                 (substring commit 0 (min 12 (length commit)))
                                 (if head (substring head 0 (min 12 (length head))) "not known")))))))
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
          (cond ((null head) (setq blocker "the pull request's head on GitHub isn't known: fetch it"))
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
      ("line"
       (let ((start (plist-get target :start_line)) (line (plist-get target :line)))
         (format "Comment on %s:%s%s" (plist-get target :path)
                 (if (and start (/= start line)) (format "%d-%d" start line) line)
                 (if (equal (plist-get target :side) "LEFT") " (removed lines)" ""))))
      ("file" (format "Comment on %s" (plist-get target :path)))
      ("summary" "The review's summary")
      (_ (if-let* ((answers (plist-get target :answers)))
             (format "Comment on the pull request, answering %s" answers)
           "Comment on the pull request")))))

;;;; At point, in the review

(defun bndos-review-outbox--here ()
  "The review, its drafts' store and the comment at point: in the overview,
a file, or the thread panel (its thread)."
  (let* (comment review
         (here (lambda () (setq comment (diff-review--thread-here)
                                review (diff-review--in-review (current-buffer))))))
    (if (bound-and-true-p diff-review--panel-source)
        (diff-review--panel-in-source here)
      (funcall here))
    (list review
          (or (bndos-review-outbox-store review) (user-error "No pull request fetched for this review"))
          comment)))

(defun bndos-review-draft-edit ()
  "Edit the draft at point: yours once saved, sent as your own."
  (interactive)
  (pcase-let* ((`(,review ,store ,comment) (bndos-review-outbox--here))
               (drafts (bndos-review-outbox--drafts-of store comment '("drafts"))))
    (unless drafts (user-error "No draft here%s" (if (bndos-review-outbox--drafts-of store comment) " but one being sent" "")))
    (bndos-review-outbox-edit-draft store (bndos-review-outbox--pick drafts "edit")
                                    (with-current-buffer review (bndos-review-outbox--author)))))

(defun bndos-review-draft-delete ()
  "Delete the draft at point, if it is still as it was when asked."
  (interactive)
  (pcase-let* ((`(,_review ,store ,comment) (bndos-review-outbox--here))
               (drafts (bndos-review-outbox--drafts-of store comment '("drafts")))
               (draft (or (bndos-review-outbox--pick drafts "delete") (user-error "No draft here")))
               (hash (bndos-review-outbox-hash draft)))
    (when (y-or-n-p (format "Delete the draft by %s, “%s”? " (plist-get draft :author)
                            (truncate-string-to-width (car (split-string (plist-get draft :body) "\n")) 40 nil nil "…")))
      (let ((result (bndos-review-outbox-remove store (plist-get draft :id) hash)))
        (unless (eq result t) (user-error "Not deleted: %s" result))
        (message "Draft deleted")))))

(defun bndos-review-draft-quote ()
  "Reply to a message of the thread at point quoting it: in a thread, a reply;
on the conversation, a comment of its own."
  (interactive)
  (pcase-let* ((`(,review ,store ,comment) (bndos-review-outbox--here))
               (messages (seq-remove (lambda (m) (gethash "draft" m))
                                     (cons comment (append (gethash "replies" comment) nil)))))
    (unless (diff-review--external-p comment) (user-error "Not a GitHub thread or comment"))
    (when (gethash "draft-id" comment) (user-error "A draft: edit it instead"))
    (let* ((names (mapcar (lambda (m) (cons (format "%s: %s" (gethash "author" m)
                                                    (truncate-string-to-width (diff-review--first-line m) 60 nil nil "…"))
                                            m))
                          messages))
           (message (if (cdr names)
                        (cdr (assoc (completing-read "Quote: " (mapcar #'car names) nil t nil nil (car (car (last names))))
                                    names))
                      (cdar names)))
           (quote (concat (bndos-review-outbox--quoted (gethash "body" message)) "\n\n")))
      (if (gethash "thread-id" comment)
          (bndos-review-outbox--new-reply review store comment quote)
        (bndos-review-outbox-comment review quote (gethash "author" message))))))

(defun bndos-review-outbox--diff-lines (root base head path)
  "The lines of PATH the pull request's diff shows, from BASE's merge base with
HEAD to HEAD, as GitHub does (3 lines of context): (RIGHT . LEFT) ranges,
\(FROM . TO) each; nil when it doesn't change PATH."
  (let ((default-directory root) right left)
    (when-let* ((merge-base (bndos-review-github--git "merge-base" base head)))
      (with-temp-buffer
        (when (zerop (process-file "git" nil t nil "--no-optional-locks" "diff" "-U3" "--no-color" "--no-ext-diff"
                                   "--no-renames" merge-base head "--" path))
          (goto-char (point-min))
          (while (re-search-forward "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" nil t)
            (let ((old (string-to-number (match-string 1))) (old-len (if (match-string 2) (string-to-number (match-string 2)) 1))
                  (new (string-to-number (match-string 3))) (new-len (if (match-string 4) (string-to-number (match-string 4)) 1)))
              (when (> new-len 0) (push (cons new (+ new new-len -1)) right))
              (when (> old-len 0) (push (cons old (+ old old-len -1)) left))))
          (and (> (buffer-size) 0) (cons (nreverse right) (nreverse left))))))))

(defun bndos-review-outbox--line-target (review path lines)
  "A comment's target on LINES, (FIRST . LAST) of PATH as the pull request's
head has them, or of the file with LINES nil; an error when GitHub wouldn't
take it."
  (with-current-buffer review
    (let* ((state bndos-review-github--state)
           (head (or (plist-get state :head-oid) (user-error "The pull request's head isn't known: fetch it")))
           (base (or (plist-get state :base-oid) (user-error "The pull request's base isn't known: fetch it")))
           (ranges (or (bndos-review-outbox--diff-lines diff-review--root base head path)
                       (user-error "%s isn't changed by the pull request (or its head isn't here): comments go on its changed files"
                                   path))))
      (if (null lines)
          (list :kind "file" :path path :commit head)
        (unless (seq-some (lambda (r) (and (<= (car r) (car lines)) (<= (cdr lines) (cdr r)))) (car ranges))
          (user-error "%s:%s isn't in one hunk of the pull request's diff, so GitHub won't take it; lines there: %s"
                      path (if (= (car lines) (cdr lines)) (car lines) (format "%d-%d" (car lines) (cdr lines)))
                      (mapconcat (lambda (r) (if (= (car r) (cdr r)) (number-to-string (car r)) (format "%d-%d" (car r) (cdr r))))
                                 (car ranges) ", ")))
        (append (list :kind "line" :path path :line (cdr lines) :side "RIGHT" :commit head)
                (and (/= (car lines) (cdr lines)) (list :start_line (car lines))))))))

(defun bndos-review-outbox--as-head-p (review path)
  "Whether this buffer, a file of REVIEW, is PATH as the pull request's head has it."
  (let ((head (plist-get (buffer-local-value 'bndos-review-github--state review) :head-oid)))
    (and head (equal (diff-review--buffer-blob)
                     (with-current-buffer review
                       (let ((default-directory diff-review--root)) (bndos-review-outbox--blob head path)))))))

(defun bndos-review-draft-comment (&optional file)
  "Draft a comment on the region's lines, or the line at point, of the pull
request's head, sent with your review; with FILE (a prefix argument), on
the whole file."
  (interactive "P")
  (unless diff-review-file-mode (user-error "Not in a file under review"))
  (let* ((review (diff-review--in-review (current-buffer)))
         (store (or (bndos-review-outbox-store review) (user-error "No pull request fetched for this review")))
         (path diff-review--file-path)
         (lines (and (not file) (diff-review--region-lines))))
    (unless (bndos-review-outbox--as-head-p review path)
      (user-error "%s here isn't as the pull request's head has it: comment where it is (commit and push, or fetch)" path))
    (let* ((target (bndos-review-outbox--line-target review path lines))
           (body (bndos-review-outbox--read-body
                  (format "%s (a draft, sent with your review)"
                          (bndos-review-outbox--describe-target review (list :target target))))))
      (deactivate-mark)
      (bndos-review-outbox-create
       store (list :pr (bndos-review-outbox--pr review) :target target :body body
                   :author (with-current-buffer review (bndos-review-outbox--author))))
      (message "Comment drafted: %s sends it with your review" (substitute-command-keys "\\[bndos-review-send]")))))

(defun bndos-review-draft-summary ()
  "Draft the summary of your review, or edit it."
  (interactive)
  (let* ((review (diff-review--in-review (current-buffer)))
         (store (or (bndos-review-outbox-store review) (user-error "No pull request fetched for this review")))
         (own (with-current-buffer review (bndos-review-outbox--author)))
         (drafted (seq-filter (lambda (d) (equal (plist-get (plist-get d :target) :kind) "summary"))
                              (bndos-review-outbox--live store))))
    (cond
     ((seq-find (lambda (d) (equal (plist-get d :state) "drafts")) drafted)
      (bndos-review-outbox-edit-draft store (seq-find (lambda (d) (equal (plist-get d :state) "drafts")) drafted) own))
     (drafted (user-error "The review's summary is being sent"))
     (t (bndos-review-outbox-create
         store (list :pr (bndos-review-outbox--pr review) :target (list :kind "summary")
                     :body (bndos-review-outbox--read-body "The review's summary (a draft)") :author own))
        (message "Summary drafted: %s sends it with your review" (substitute-command-keys "\\[bndos-review-send]"))))))

(defun bndos-review-draft-put-back ()
  "Put the unconfirmed send at point back among the drafts, once you know it
isn't on GitHub.  Also one another Emacs, still running, claimed long ago."
  (interactive)
  (pcase-let* ((`(,_review ,store ,comment) (bndos-review-outbox--here))
               (stuck-p (lambda (d) (or (bndos-review-outbox-unconfirmed-p d) (bndos-review-outbox--stuck-p d))))
               (draft (or (bndos-review-outbox--pick (seq-filter stuck-p (bndos-review-outbox--drafts-of store comment '("sending")))
                                                     "put back")
                          (user-error "No unconfirmed send here")))
               (id (plist-get draft :id))
               (stuck (bndos-review-outbox--stuck-p draft))
               (returnable (lambda (d) (and (plist-get d :body)
                                            (or (bndos-review-outbox-unconfirmed-p d)
                                                (and stuck (bndos-review-outbox--stuck-p d)))))))
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
         ((not draft) (user-error "It moved meanwhile: look again"))
         ((not (funcall returnable draft))
          (bndos-review-outbox--move held file)
          (user-error "It is being sent, or can't be read: look again")))
        (bndos-review-outbox--write held (plist-put (plist-put draft :claim nil) :error "put back after an unconfirmed send"))
        (unless (bndos-review-outbox--move held (bndos-review-outbox--path store "drafts" id))
          (bndos-review-outbox--move held file)
          (user-error "A draft with its id is there already: look again"))
        (bndos-review-outbox-changed store)
        (message "Put back among the drafts")))))

;;;; Sending what the user picks

(defvar-local bndos-review-outbox-report nil "In a review: what its last send did.")
(defvar-local bndos-review-outbox--sending nil "In a review: non-nil while it sends.")

(defvar bndos-review-outbox-choose-function #'bndos-review-outbox--choose
  "Function of (REVIEW DRAFTS) giving those of DRAFTS to send.")

(defun bndos-review-outbox--line (review draft problems)
  "DRAFT in a line, with PROBLEMS' warnings."
  (format "%s — %s: “%s”%s"
          (bndos-review-outbox--describe-target review draft)
          (plist-get draft :author)
          (truncate-string-to-width (replace-regexp-in-string "\n+" " " (plist-get draft :body)) 60 nil nil "…")
          (if (cdr problems) (propertize (concat "  ⚠ " (string-join (cdr problems) "; ")) 'face 'bndos-review-outbox-warning) "")))

(defun bndos-review-outbox--choose (review drafts)
  "Ask, draft by draft, which of DRAFTS to send: y, n, ! all the rest, q stop."
  (let (chosen)
    (map-y-or-n-p (lambda (d) (format "Send %s? " (bndos-review-outbox--line review d (bndos-review-outbox-problems review d))))
                  (lambda (d) (push d chosen))
                  drafts
                  '("draft" "drafts" "send"))
    (nreverse chosen)))

(defun bndos-review-outbox--own-p (review)
  "Whether REVIEW's pull request is that of whom GitHub posts as."
  (let ((state (bndos-review-outbox--github review)))
    (and (plist-get state :viewer) (equal (plist-get state :viewer) (plist-get state :author)))))

(defun bndos-review-outbox--confirm (review chosen)
  "Confirm sending CHOSEN to REVIEW's pull request: the event of the review
they make, when they make one (\"COMMENT\", \"REQUEST_CHANGES\" or
\"APPROVE\"), t when they make none, nil when declined."
  (let* ((state (bndos-review-outbox--github review))
         (count (lambda (pred) (seq-count pred chosen)))
         (lines (funcall count (lambda (d) (equal (plist-get (plist-get d :target) :kind) "line"))))
         (summary (seq-some (lambda (d) (equal (plist-get (plist-get d :target) :kind) "summary")) chosen))
         (alone (funcall count (lambda (d) (not (bndos-review-outbox--review-kind-p d)))))
         (what (string-join
                (delq nil (list (and (> alone 0) (format "%d draft%s" alone (if (= alone 1) "" "s")))
                                (and (or summary (> lines 0))
                                     (format "a review (%s)"
                                             (string-join (delq nil (list (and summary "its summary")
                                                                          (and (> lines 0) (format "%d comment%s on lines" lines (if (= lines 1) "" "s")))))
                                                          ", ")))))
                " and "))
         (where (format "%s/%s#%s as %s" (plist-get state :owner) (plist-get state :name) (plist-get state :number)
                        (if (plist-get state :viewer) (concat "@" (plist-get state :viewer)) "this project's GitHub account"))))
    (cond
     ((not (or summary (> lines 0)))
      (and (y-or-n-p (format "Send %s to %s? " what where)) t))
     ;; GitHub takes no verdict on your own pull request.
     ((bndos-review-outbox--own-p review)
      (and (y-or-n-p (format "Send %s to %s, your own pull request: a comment review? " what where)) "COMMENT"))
     (t
      (pcase (car (read-multiple-choice (format "Send %s to %s" what where)
                                        '((?c "comment" "a review that only comments")
                                          (?r "request changes" "a review asking for changes")
                                          (?a "approve" "a review approving it")
                                          (?q "cancel" "send nothing"))))
        (?c "COMMENT")
        (?r (if summary "REQUEST_CHANGES"
              (user-error "Asking for changes needs the review's summary: %s drafts it"
                          (substitute-command-keys "\\[bndos-review-draft-summary]"))))
        (?a "APPROVE")
        (_ nil))))))

(defun bndos-review-send ()
  "Send drafts of this review's pull request: those you pick, after a
confirmation.  Comments on lines and the summary go as one review."
  (interactive)
  (let* ((review (diff-review--in-review (current-buffer)))
         (store (or (bndos-review-outbox-store review) (user-error "No pull request fetched for this review")))
         (age (bndos-review-outbox--age review)))
    (cond
     ((buffer-local-value 'bndos-review-outbox--sending review) (user-error "Already sending"))
     ((plist-get (bndos-review-outbox--github review) :fetching)
      (user-error "Fetching GitHub's threads: send once they're in"))
     ((or (null age) (> age bndos-review-outbox-max-age))
      ;; What to know before sending is GitHub's as it is now.
      (message "Fetching GitHub's threads first…")
      (bndos-review-github-refresh review t)
      (bndos-review-outbox--when-fetched
       review (lambda () (when (and (buffer-live-p review) (bndos-review-outbox--age review)
                                    (< (bndos-review-outbox--age review) bndos-review-outbox-max-age))
                           (with-current-buffer review (bndos-review-send))))))
     (t
      (let* ((bndos-review-changes-cache-seconds 0)   ; the code as it is now
             (drafts (seq-filter (lambda (d) (equal (plist-get d :state) "drafts")) (bndos-review-outbox-load store)))
             (blocked (seq-filter (lambda (d) (car (bndos-review-outbox-problems review d))) drafts))
             (ready (seq-difference drafts blocked)))
        (unless drafts (user-error "No drafts to send"))
        (when blocked
          (display-message-or-buffer
           (concat (format "%d draft%s can't be sent:\n" (length blocked) (if (cdr blocked) "s" ""))
                   (mapconcat (lambda (d) (format "  %s — %s" (bndos-review-outbox--describe-target review d)
                                                  (car (bndos-review-outbox-problems review d))))
                              blocked "\n"))))
        (unless ready (user-error "Nothing can be sent"))
        ;; Each as it was when asked about: one changed since isn't sent.
        (let* ((chosen (funcall bndos-review-outbox-choose-function review ready))
               (marks (mapcar (lambda (d) (cons (plist-get d :id) (bndos-review-outbox-hash d))) chosen))
               (event (and chosen (bndos-review-outbox--confirm review chosen))))
          (if (not event)
              (message "Nothing sent")
            (with-current-buffer review
              (setq bndos-review-outbox--sending t
                    bndos-review-outbox-report nil))
            (message "Sending…")
            (bndos-review-outbox-send-marks
             review store marks
             (lambda (results)
               (when (buffer-live-p review)
                 (with-current-buffer review
                   (setq bndos-review-outbox--sending nil
                         bndos-review-outbox-report (bndos-review-outbox--summary results))
                   (message "%s" (substring-no-properties bndos-review-outbox-report))
                   (bndos-review-github-refresh review t))))
             (and (stringp event) event)))))))))

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

(defun bndos-review-outbox-thread-changes ()
  "Show the changes the draft at point links: a reply in its thread, or a
comment."
  (interactive)
  (pcase-let* ((`(,review ,store ,comment) (bndos-review-outbox--here))
               (draft (bndos-review-outbox--pick
                       (seq-filter (lambda (d) (plist-get d :changes))
                                   (bndos-review-outbox--drafts-of store comment '("drafts" "sending" "sent")))
                       "show the changes of")))
    (unless draft (user-error "No draft here links changes"))
    (bndos-review-outbox-show-changes review draft)))

(defun bndos-review-outbox-unexplained ()
  "Show the changes no draft links, since the oldest commit a draft's changes
are taken from; those the base branch brought left out."
  (interactive)
  (let* ((review (diff-review--in-review (current-buffer)))
         (store (or (bndos-review-outbox-store review) (user-error "No pull request fetched for this review")))
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

(setq diff-review-thread-drafts-function #'bndos-review-outbox--thread-drafts
      diff-review-external-reply-function #'bndos-review-outbox-reply)

(provide 'bndos-review-outbox)
;;; bndos-review-outbox.el ends here
