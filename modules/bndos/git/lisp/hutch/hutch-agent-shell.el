;;; hutch-agent-shell.el --- Reviews by an agent-shell agent -*- lexical-binding: t; -*-

;; bndos: added locally; not part of upstream magit-hutch.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; An alternative to Hutch's gptel tool loop (`hutch-review-transport'):
;; each scope is reviewed by a new agent-shell session with any ACP agent
;; (Claude Code, Codex, ...), started in the repository in a read-only mode
;; and with no memory of any other conversation.  The agent reads the diff
;; and the files with its own tools and ends with one fenced block in
;; submit_review's shape:
;;
;;   ```hutch-review
;;   {"findings": [{"file": ..., "title": ..., "description": ...,
;;                  "old_lines": ..., "new_lines": ... | "lines": ...}
;;                 | {"file": ..., "lgtm": true}]}
;;   ```
;;
;; The findings then go through the same normalisation and gates as
;; gptel's: a suggestion whose old_lines don't match the file exactly once
;; becomes a comment.  The reviewer's agent-shell stays open afterwards
;; (`hutch-agent-shell-keep-buffers') so you can ask it about a finding.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'hutch-agent)
(require 'hutch-findings)
(require 'hutch-gates)
(require 'hutch-git)

(declare-function agent-shell-start "ext:agent-shell")
(declare-function agent-shell-insert "ext:agent-shell")
(declare-function agent-shell-subscribe-to "ext:agent-shell")
(declare-function agent-shell-unsubscribe "ext:agent-shell")
(declare-function agent-shell-interrupt "ext:agent-shell")
(defvar agent-shell-session-strategy)
(defvar agent-shell-cwd-function)

(defvar hutch-agent-shell-config nil
  "agent-shell config (an alist) that reviews the next scopes.
Let-bind it around `hutch-review-run' with `hutch-review-transport' set to
`agent-shell'.")

(defvar hutch-agent-shell-model nil
  "Model id for the reviewer session, or nil for the agent's default.")

(defcustom hutch-agent-shell-read-only-modes '((claude-code . "plan") (codex . "read-only"))
  "Session mode a reviewer starts in, by agent-shell config identifier.
Reviewers only read.  Agents not listed start in their configured mode; the
prompt tells every reviewer not to change anything either way."
  :type '(alist :key-type symbol :value-type string)
  :group 'hutch)

(defcustom hutch-agent-shell-keep-buffers t
  "Keep a reviewer's agent-shell once it has answered, to ask it follow-ups."
  :type 'boolean
  :group 'hutch)

(defvar hutch-agent-shell-prompt-template
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

End your reply with exactly one fenced block of valid JSON, nothing after it:

```hutch-review
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
  "Prompt for an agent-shell reviewer: %s the diff command, %s the manifest.")

(defun hutch-agent-shell--config ()
  "A copy of `hutch-agent-shell-config' set up for one read-only review."
  (let* ((config (copy-alist (or hutch-agent-shell-config
                                 (error "Set `hutch-agent-shell-config' to review with agent-shell"))))
         (mode (alist-get (map-elt config :identifier) hutch-agent-shell-read-only-modes))
         (model hutch-agent-shell-model))
    (when mode (setf (alist-get :default-session-mode-id config) (lambda () mode)))
    (when model (setf (alist-get :default-model-id config) (lambda () model)))
    config))

(defun hutch-agent-shell--prompt (scope)
  (format hutch-agent-shell-prompt-template
          (string-join (cons "git diff"
                             (hutch--scope-diff-args (plist-get scope :scope)
                                                     (plist-get scope :head)
                                                     (plist-get scope :base)))
                       " ")
          (plist-get scope :manifest)))

(defun hutch-agent-shell-extract (text)
  "Raw findings from the last hutch-review block in TEXT.
Return (:ok FINDINGS), (:error REASON), or nil when there is no block."
  (let (block (start 0))
    (while (string-match "^```hutch-review[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n```" text start)
      (setq block (match-string 1 text)
            start (match-end 0)))
    (when block
      (condition-case err
          (let ((parsed (json-parse-string block :object-type 'plist :array-type 'list
                                           :null-object nil :false-object nil)))
            (if (and (plistp parsed) (plist-member parsed :findings) (listp (plist-get parsed :findings)))
                (list :ok (plist-get parsed :findings))
              (list :error "the hutch-review block has no findings list")))
        (error (list :error (format "invalid JSON in the hutch-review block: %s"
                                    (error-message-string err))))))))

(defun hutch-agent-shell--result (scope text stop-reason)
  "The result plist for SCOPE from the reviewer's TEXT."
  (if (member (format "%s" stop-reason) '("cancelled" "canceled"))
      (hutch--make-error-result scope 'cancelled "the reviewer's turn was cancelled")
    (pcase (hutch-agent-shell-extract text)
      (`(:ok ,raw)
       (hutch--make-result :ok scope
                           (hutch--run-gates scope (mapcar #'hutch--normalize-tool-finding raw))
                           nil))
      (`(:error ,reason) (hutch--make-error-result scope 'invalid-response reason))
      (_ (hutch--make-error-result scope 'no-submit "the reviewer's reply had no hutch-review block")))))

(defun hutch-agent-shell--review-scope (scope on-done _on-progress cancel-box)
  "Review SCOPE in a new agent-shell session; call ON-DONE with the result.
CANCEL-BOX receives a function that interrupts the reviewer."
  (require 'agent-shell)
  (let* ((directory default-directory)
         (config (hutch-agent-shell--config))
         (agent-shell-session-strategy 'new)
         (agent-shell-cwd-function (lambda () directory))
         (buffer (save-window-excursion (agent-shell-start :config config)))
         (chunks nil)
         (tokens nil)
         (done nil))
    (cl-labels ((finish (result)
                  (unless done
                    (setq done t)
                    (dolist (token tokens) (ignore-errors (agent-shell-unsubscribe :subscription token)))
                    (unless hutch-agent-shell-keep-buffers
                      (when (buffer-live-p buffer)
                        (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))
                    (let ((default-directory directory))
                      (funcall on-done (if (functionp result) (funcall result) result)))))
                (on (event fn)
                  (push (agent-shell-subscribe-to
                         :shell-buffer buffer :event event
                         :on-event (lambda (e)
                                     (condition-case err
                                         (funcall fn (map-elt e :data))
                                       (error (finish (hutch--make-error-result
                                                       scope 'agent-error (error-message-string err)))))))
                        tokens)))
      (hutch--result-box-set cancel-box
                             (lambda ()
                               (unless done
                                 (setq done t)
                                 (when (buffer-live-p buffer)
                                   (ignore-errors (with-current-buffer buffer (agent-shell-interrupt t)))))))
      (on 'init-finished
          (lambda (_) (agent-shell-insert :text (hutch-agent-shell--prompt scope)
                                          :submit t :no-focus t :shell-buffer buffer)))
      (on 'agent-message-chunk
          (lambda (data) (when-let* ((chunk (map-elt data :text-chunk))) (push chunk chunks))))
      (on 'turn-complete
          (lambda (data)
            (let ((text (apply #'concat (reverse chunks))))
              (finish (lambda () (hutch-agent-shell--result scope text (map-elt data :stop-reason)))))))
      (on 'error
          (lambda (data) (finish (hutch--make-error-result scope 'agent-error (format "%s" (map-elt data :message))))))
      (on 'clean-up
          (lambda (_) (finish (hutch--make-error-result scope 'agent-closed "the reviewer's agent-shell was closed")))))
    buffer))

(provide 'hutch-agent-shell)

;;; hutch-agent-shell.el ends here
