;;; bndos/tasks/config.el -*- lexical-binding: t; -*-

;; Parallel coding tasks: worktrees from worktrunk, agents as Hydra ACP
;; sessions (bin/bndos-task) that agent-shell attaches to, reviews in
;; diff-review.  Needs the git module
;; (diff-review), the ai module (agent-shell) and hydra-acp on PATH.

(add-load-path! "lisp")

;; A task's review file opened any way gets its scopes and agent commands.
(after! diff-review (require 'bndos-task-review))

;; A worktree reviewed from magit, against its task's parent.
(autoload 'bndos-task-git-saved-base-sync "bndos-task-git")
(setq bndos-diff-review-base-function #'bndos-task-git-saved-base-sync)

(use-package! bndos-task-dashboard
  :commands (bndos/task-dashboard bndos/task-select bndos/task-start bndos/task-review-here
             bndos/task-review-tree
             bndos/task-agent-here bndos/task-adopt bndos/task-set-parent bndos/task-add-project))

;; Tells of an agent waiting for a permission when no shell here shows it.
(use-package! bndos-task-watch
  :commands bndos-task-watch-mode
  :init (run-with-idle-timer 5 nil #'bndos-task-watch-mode 1))

(map! :leader
      (:prefix ("g w" . "worktree tasks")
       :desc "Dashboard"        "w" #'bndos/task-dashboard
       :desc "Select task"      "s" #'bndos/task-select
       :desc "Start task"       "n" #'bndos/task-start
       :desc "Review this tree" "r" #'bndos/task-review-here
       :desc "Review's file tree" "t" #'bndos/task-review-tree
       :desc "Agent of this tree" "a" #'bndos/task-agent-here
       :desc "Adopt a conversation" "i" #'bndos/task-adopt
       :desc "Set parent branch"  "b" #'bndos/task-set-parent
       :desc "Add project"        "p" #'bndos/task-add-project))

;; The dashboard is a read-only UI in evil's normal state, like magit's.  A
;; task's review is diff-review's (its overview and sidebar start in normal
;; state too); the agent flow adds C-c C-<key> to the overview and keys
;; under diff-review's prefix (C-c _) in its files.
(after! evil
  (evil-set-initial-state 'bndos-task-dashboard-mode 'normal))

(map! :after bndos-task-dashboard
      :map bndos-task-dashboard-mode-map
      :n "RET"   #'bndos/task-dashboard-review
      :n "TAB"   #'magit-section-toggle
      :n "q"     #'quit-window
      :n "g r"   #'bndos/task-dashboard-refresh
      :n "v"     #'bndos/task-dashboard-agent
      :n "o"     #'bndos/task-dashboard-workspace
      :n "i"     #'bndos/task-dashboard-instruct
      :n "+"     #'bndos/task-start
      :n "x"     #'bndos/task-dashboard-forget
      :n "w"     #'bndos/task-dashboard-restore)
