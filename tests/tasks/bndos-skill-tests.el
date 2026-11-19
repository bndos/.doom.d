;;; bndos-skill-tests.el --- The leader skill against bin/bndos-task -*- lexical-binding: t; -*-

;; The skill drives the CLI by its commands and options; a renamed or dropped
;; one leaves a leader with instructions that fail.  Its behaviour as a leader
;; is a model's, not tested here.

(require 'test-helper)
(require 'bndos-task-agent)

(defconst bndos-test-skill-file
  (expand-file-name "../../modules/bndos/tasks/skills/task-leader/SKILL.md" bndos-test-dir))

(defun bndos-test-skill-uses ()
  "Each `$bt COMMAND WORD... --option...' in the skill, as
(COMMAND FIRST-WORD . OPTIONS)."
  (with-temp-buffer
    (insert-file-contents bndos-test-skill-file)
    (let (uses)
      (while (re-search-forward "\\$bt \\([a-z][a-z-]*\\)\\([^`\n]*\\)" nil t)
        (let ((command (match-string 1)) (rest (match-string 2)) (start 0) options)
          (while (string-match "\\(?:^\\| \\)\\(--[a-z][a-z-]*\\)" rest start)
            (push (match-string 1 rest) options)
            (setq start (match-end 0)))
          (push (cons command (cons (and (string-match "\\` \\([a-z][a-z-]*\\)" rest)
                                         (match-string 1 rest))
                                    (nreverse options)))
                uses)))
      (nreverse uses))))

(defun bndos-test-cli-help (&rest args)
  (with-temp-buffer
    (apply #'call-process bndos-task-cli nil t nil (append args '("--help")))
    (buffer-string)))

(defun bndos-test-cli-choices (help)
  "The subcommands HELP lists, or nil."
  (and (string-match "{\\([^}]+\\)}" help)
       (split-string (match-string 1 help) ",")))

(ert-deftest bndos-skill-uses-only-commands-and-options-the-cli-has ()
  (let ((uses (bndos-test-skill-uses))
        (commands (with-temp-buffer
                    (call-process bndos-task-cli nil t nil "--help")
                    (goto-char (point-min))
                    (re-search-forward "{\\([^}]+\\)}")
                    (split-string (match-string 1) ","))))
    (should (> (length uses) 10))
    (dolist (use uses)
      (should (member (car use) commands))
      (let* ((help (bndos-test-cli-help (car use)))
             (subcommands (bndos-test-cli-choices help)))
        ;; A command of subcommands, named bare or with one: then options
        ;; are the subcommand's.
        (when (and subcommands (or (cadr use) (cddr use)))
          (should (member (cadr use) subcommands))
          (setq help (bndos-test-cli-help (car use) (cadr use))))
        (dolist (option (cddr use))
          (should (string-match-p (concat "\\_<" (regexp-quote option) "\\_>") help)))))))

(provide 'bndos-skill-tests)
;;; bndos-skill-tests.el ends here
