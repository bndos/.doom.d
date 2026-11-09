;;; bndos/tasks/config.el -*- lexical-binding: t; -*-

;; Parallel coding tasks: worktrees from worktrunk, agents as Hydra ACP
;; sessions (bin/bndos-task) that agent-shell attaches to, reviews in Hutch.
;; See README.org in this directory.  Needs the git module (hutch,
;; magit-treediff), the ai module (agent-shell) and hydra-acp on PATH.

(add-load-path! "lisp")

(use-package! bndos-task-dashboard
  :commands (bndos/task-dashboard bndos/task-select bndos/task-start bndos/task-review-here))

(map! :leader
      (:prefix ("g w" . "worktree tasks")
       :desc "Dashboard"        "w" #'bndos/task-dashboard
       :desc "Select task"      "s" #'bndos/task-select
       :desc "Start task"       "n" #'bndos/task-start
       :desc "Review this tree" "r" #'bndos/task-review-here))

;; Both buffers are read-only UIs: motion state keeps j/k and friends.  The
;; review's actions are on C-c C-<key> (like pr-review), so they also work in
;; visual state on a selected region.
(after! evil
  (evil-set-initial-state 'bndos-task-review-mode 'motion)
  (evil-set-initial-state 'bndos-task-dashboard-mode 'motion))

(map! :after bndos-task-review
      :map bndos-task-review-mode-map
      :m "RET" #'bndos/task-review-visit
      :m "TAB" #'magit-section-toggle
      :m "g r" #'bndos/task-review-refresh
      :m "g t" #'bndos/task-review-toggle-tree
      :m "g f" #'bndos/task-review-visit
      :m "]t"  #'bndos/task-review-next-thread
      :m "[t"  #'bndos/task-review-previous-thread
      :m "]]"  #'magit-section-forward-sibling
      :m "[["  #'magit-section-backward-sibling)

(map! :after bndos-task-dashboard
      :map bndos-task-dashboard-mode-map
      :m "RET" #'bndos/task-dashboard-review
      :m "TAB" #'magit-section-toggle
      :m "+"   #'bndos/task-start
      :m "g r" #'bndos/task-dashboard-refresh
      :m "v"   #'bndos/task-dashboard-agent
      :m "o"   #'bndos/task-dashboard-workspace)
