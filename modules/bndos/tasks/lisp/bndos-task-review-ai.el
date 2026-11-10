;;; bndos-task-review-ai.el --- Reviews by an agent-shell agent -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A change reviewed by a new read-only agent-shell session, which ends its
;; reply with a review-findings JSON block.  A turn without the block pauses
;; the review; the next turn with one completes it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'seq)

(declare-function agent-shell-start "ext:agent-shell")
(declare-function agent-shell-insert "ext:agent-shell")
(declare-function agent-shell-subscribe-to "ext:agent-shell")
(declare-function agent-shell-unsubscribe "ext:agent-shell")
(declare-function agent-shell-interrupt "ext:agent-shell")
(declare-function agent-shell-set-session-model "ext:agent-shell")
(defvar agent-shell-session-strategy)
(defvar agent-shell-cwd-function)

(defcustom bndos-task-review-ai-read-only-modes '((claude-code . "plan") (codex . "read-only"))
  "Session mode a reviewer starts in, by agent-shell config identifier.
Reviewers only read.  Agents not listed start in their configured mode; the
prompt tells every reviewer not to change anything either way."
  :type '(alist :key-type symbol :value-type string)
  :group 'bndos-task)

(defcustom bndos-task-review-ai-keep-buffers t
  "Keep a reviewer's agent-shell once it has answered, to ask it follow-ups."
  :type 'boolean
  :group 'bndos-task)

(defvar-local bndos-task-review-ai-pause-reason nil
  "Why this reviewer's review is paused, or nil when it isn't.")

(defvar bndos-task-review-ai-resume-prompt
  "Your review was interrupted before you finished.  Complete it, then end \
your reply with the full review-findings block in the format given above."
  "Prompt `bndos-task-review-ai-resume' sends a paused reviewer.")

(defvar bndos-task-review-ai-prompt-template
  "You are an independent code reviewer.  You did not write this change and \
have no other context about it: judge only what is in the repository.  Do \
not modify files, commit, or run anything that changes state; only read.

The change under review is `%s` in this repository (an id may be a tree \
object; pass the ids to git diff / git show exactly as given).

Changed files (insertions/deletions):
%s

Read what you need: each file's diff (add `-- FILE` to the command above), \
the files themselves, their callers and tests.  Look for substantive \
problems: bugs, logic errors, edge cases, error handling, security, races, \
broken contracts, missing tests.  Look through at least three angles.  Aim \
for 2-5 findings on a substantive change; on trivial changes (docs, \
formatting, renames) mark files lgtm rather than manufacturing findings.
%s
End your reply with exactly one fenced block of valid JSON, nothing after it:

```review-findings
{\"findings\": [
  {\"file\": \"path\", \"title\": \"...\", \"description\": \"...\", \"lines\": \"42-50\"},
  {\"file\": \"path\", \"title\": \"...\", \"description\": \"...\", \"old_lines\": \"...\", \"new_lines\": \"...\"},
  {\"file\": \"path\", \"lgtm\": true}
]}
```

Finding fields:
- file: path relative to the repository root
- title: at most 80 characters
- description: at most 1000 characters
- old_lines / new_lines: a suggested edit: old_lines is exact verbatim text \
from the file (every space and tab), matching it exactly once (include an \
extra line above or below if needed), typically 3-10 lines; new_lines is its \
replacement.  Use them for trivial, certain fixes (typo, wrong operator, \
off-by-one).
- lines: the anchor of a comment-only finding (\"42\", \"42-50\" or \
\"42, 90\"); omit it when old_lines is set
- lgtm: true for a file with no issues (no other fields)"
  "Prompt for a reviewer: %s the diff command, %s the changed files,
%s the context section.")

(defun bndos-task-review-ai--git (directory &rest args)
  "ARGS' output in DIRECTORY, or nil when git fails."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (when (zerop (apply #'process-file "git" nil '(t nil) nil "--no-optional-locks" args))
        (buffer-string)))))

(defun bndos-task-review-ai-manifest (directory base head)
  "The files changed from BASE to HEAD, one \"path (+A -D)\" a line, or nil."
  (when-let* ((output (bndos-task-review-ai--git directory "diff" "--numstat" base head))
              (lines (split-string output "\n" t)))
    (mapconcat (lambda (line)
                 (pcase (split-string line "\t")
                   (`(,added ,removed ,path . ,_) (format "%s (+%s -%s)" path added removed))
                   (_ "")))
               lines "\n")))

(defun bndos-task-review-ai--prompt (base head manifest context)
  (format bndos-task-review-ai-prompt-template
          (format "git diff %s %s" base head)
          manifest
          (if (and context (not (string-blank-p context)))
              (concat "\n" (string-trim context) "\n")
            "")))

(defun bndos-task-review-ai-extract (text)
  "Raw findings from the last review-findings block in TEXT.
Return (:ok FINDINGS), (:error REASON), or nil when there is no block."
  (let (block (start 0))
    (while (string-match "^```review-findings[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n```" text start)
      (setq block (match-string 1 text)
            start (match-end 0)))
    (when block
      (condition-case err
          (let ((parsed (json-parse-string block :object-type 'plist :array-type 'list
                                           :null-object nil :false-object nil)))
            (if (and (plistp parsed) (plist-member parsed :findings) (listp (plist-get parsed :findings)))
                (list :ok (plist-get parsed :findings))
              (list :error "the review-findings block has no findings list")))
        (error (list :error (format "invalid JSON in the review-findings block: %s"
                                    (error-message-string err))))))))

(defun bndos-task-review-ai--truncate (string max)
  (if (> (length string) max) (concat (substring string 0 (1- max)) "…") string))

(defun bndos-task-review-ai--occurrences (haystack needle)
  "The 1-based lines where NEEDLE starts in HAYSTACK."
  (unless (or (null needle) (string-empty-p needle))
    (let ((pos 0) lines)
      (while (setq pos (string-search needle haystack pos))
        (push (1+ (cl-count ?\n haystack :end pos)) lines)
        (setq pos (1+ pos)))
      (nreverse lines))))

(defun bndos-task-review-ai--patch (path before after)
  "A unified diff of PATH from BEFORE to AFTER."
  (let ((a (make-temp-file "review-before-")) (b (make-temp-file "review-after-")))
    (unwind-protect
        (progn (with-temp-file a (insert before))
               (with-temp-file b (insert after))
               (with-temp-buffer
                 (call-process "diff" nil t nil "-u" "--label" (concat "a/" path) "--label" (concat "b/" path) a b)
                 (buffer-string)))
      (delete-file a)
      (delete-file b))))

(defun bndos-task-review-ai--finding (raw directory head)
  "RAW, a finding from the block, checked against HEAD in DIRECTORY, or nil.
A plist: :type (`comment', `suggestion', `lgtm' or `withdraw'), :file,
:lines (\"42\", \"42-50\"), :title, :desc, :old-lines, :new-lines, :patch
and :thread, the earlier thread it revises or withdraws."
  (let* ((file (plist-get raw :file))
         (thread (and (stringp (plist-get raw :thread)) (plist-get raw :thread)))
         (title (bndos-task-review-ai--truncate (or (plist-get raw :title) "Issue") 80))
         (desc (bndos-task-review-ai--truncate (or (plist-get raw :description) "") 1000))
         (old (plist-get raw :old_lines))
         (new (plist-get raw :new_lines))
         (comment (lambda (lines) (list :type 'comment :file file :lines lines :title title :desc desc
                                        :thread thread))))
    (cond
     ((and thread (eq (plist-get raw :withdraw) t))
      (list :type 'withdraw :thread thread :desc (if (string-empty-p desc) title desc)))
     ((not (stringp file)) nil)
     ((eq (plist-get raw :lgtm) t) (list :type 'lgtm :file file))
     ;; Files outside the change are dropped.
     ((not (bndos-task-review-ai--git directory "cat-file" "-e" (format "%s:%s" head file))) nil)
     ((and (stringp old) (stringp new))
      (let* ((source (bndos-task-review-ai--git directory "show" (format "%s:%s" head file)))
             (hits (bndos-task-review-ai--occurrences source old)))
        (if (/= (length hits) 1)
            (funcall comment "?")
          (let ((end (+ (car hits) (length (string-lines old)) -1)))
            (append (funcall comment (if (= end (car hits)) (format "%d" end) (format "%d-%d" (car hits) end)))
                    (list :old-lines old :new-lines new
                          :patch (bndos-task-review-ai--patch file source (string-replace old new source))))))))
     (t (funcall comment (format "%s" (or (plist-get raw :lines) "?")))))))

(defun bndos-task-review-ai--result (directory head text stop-reason)
  "The result of the reviewer's reply TEXT: (:status :ok :findings F) or
\(:status :error :emsg MESSAGE)."
  (if (member (format "%s" stop-reason) '("cancelled" "canceled"))
      (list :status :error :emsg "the reviewer's turn was cancelled")
    (pcase (bndos-task-review-ai-extract text)
      (`(:ok ,raw) (list :status :ok
                         :findings (delq nil (mapcar (lambda (f) (bndos-task-review-ai--finding f directory head))
                                                     raw))))
      (`(:error ,reason) (list :status :error :emsg reason))
      (_ (list :status :error :emsg "the reviewer's reply had no review-findings block")))))

(defun bndos-task-review-ai--config (config model)
  "A copy of CONFIG set up for one read-only review, with MODEL if a string."
  (let* ((config (copy-alist config))
         (mode (alist-get (map-elt config :identifier) bndos-task-review-ai-read-only-modes)))
    (when mode (setf (alist-get :default-session-mode-id config) (lambda () mode)))
    (when (stringp model) (setf (alist-get :default-model-id config) (lambda () model)))
    config))

(defun bndos-task-review-ai--choose-model-then (buffer then)
  "Offer BUFFER's session models (agent-shell's picker), then call THEN once.
Quitting the picker, or an agent without models, keeps its default.  The
picker reports a refused model only as a message, so THEN also runs after a
while without an answer."
  (condition-case nil
      (with-current-buffer buffer
        (agent-shell-set-session-model (lambda (&rest _) (funcall then)))
        (run-at-time 15 nil then))
    ((quit error) (funcall then))))

(defun bndos-task-review-ai-resume (buffer)
  "Ask the paused reviewer in BUFFER to finish its review."
  (unless (buffer-local-value 'bndos-task-review-ai-pause-reason buffer)
    (user-error "That review isn't paused"))
  (agent-shell-insert :text bndos-task-review-ai-resume-prompt
                      :submit t :no-focus t :shell-buffer buffer))

(cl-defun bndos-task-review-ai-run (&key directory base head config model context on-state on-done)
  "Review BASE..HEAD of DIRECTORY in a new agent-shell session with CONFIG.
MODEL is a model id, nil for the agent's default, or `ask' for the
session's picker.  CONTEXT is added to the prompt.  ON-STATE is called
as (STATE BUFFER), STATE `started', `paused' or `resumed'; ON-DONE with
the result.  Return a function that cancels the review, or signal an
error when BASE..HEAD changes nothing."
  (require 'agent-shell)
  (let* ((manifest (or (bndos-task-review-ai-manifest directory base head)
                       (user-error "No changes to review")))
         (ask-model (eq model 'ask))
         (on-state (or on-state #'ignore))
         (agent-shell-session-strategy 'new)
         (agent-shell-cwd-function (lambda () directory))
         (default-directory directory)
         (buffer (save-window-excursion
                   (agent-shell-start :config (bndos-task-review-ai--config config model))))
         (chunks nil)
         (tokens nil)
         (sent nil)
         (done nil))
    (cl-labels ((finish (result)
                  (unless done
                    (setq done t)
                    (dolist (token tokens) (ignore-errors (agent-shell-unsubscribe :subscription token)))
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer (setq bndos-task-review-ai-pause-reason nil)))
                    (unless bndos-task-review-ai-keep-buffers
                      (when (buffer-live-p buffer)
                        (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))
                    (let ((default-directory directory))
                      (funcall on-done result))))
                (pause (reason)
                  (with-current-buffer buffer (setq bndos-task-review-ai-pause-reason reason))
                  (funcall on-state 'paused buffer))
                (send-prompt ()
                  (unless (or done sent)
                    (setq sent t)
                    (agent-shell-insert :text (bndos-task-review-ai--prompt base head manifest context)
                                        :submit t :no-focus t :shell-buffer buffer)))
                (on (event fn)
                  (push (agent-shell-subscribe-to
                         :shell-buffer buffer :event event
                         :on-event (lambda (e)
                                     (condition-case err
                                         (funcall fn (map-elt e :data))
                                       (error (finish (list :status :error :emsg (error-message-string err)))))))
                        tokens)))
      (on 'init-finished
          (lambda (_)
            ;; Out of the process filter, so the model picker can use the minibuffer.
            (run-at-time 0 nil (lambda ()
                                 (if ask-model
                                     (bndos-task-review-ai--choose-model-then buffer #'send-prompt)
                                   (send-prompt))))))
      (on 'agent-message-chunk
          (lambda (data)
            (when-let* ((chunk (map-elt data :text-chunk)))
              (unless chunks
                (when (buffer-local-value 'bndos-task-review-ai-pause-reason buffer)
                  (funcall on-state 'resumed buffer)))
              (push chunk chunks))))
      (on 'turn-complete
          (lambda (data)
            ;; Each turn is judged on its own reply.
            (let* ((text (apply #'concat (reverse chunks)))
                   (result (bndos-task-review-ai--result directory head text (map-elt data :stop-reason))))
              (setq chunks nil)
              (if (eq (plist-get result :status) :ok)
                  (finish result)
                (pause (plist-get result :emsg))))))
      (on 'error
          (lambda (data) (finish (list :status :error :emsg (format "%s" (map-elt data :message))))))
      (on 'clean-up
          (lambda (_) (finish (list :status :error :emsg "the reviewer's agent-shell was closed")))))
    (funcall on-state 'started buffer)
    (lambda ()
      (unless done
        (setq done t)
        (when (buffer-live-p buffer)
          (ignore-errors (with-current-buffer buffer (agent-shell-interrupt t))))))))

(provide 'bndos-task-review-ai)
;;; bndos-task-review-ai.el ends here
