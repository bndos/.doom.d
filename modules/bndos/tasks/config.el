;;; bndos/tasks/config.el -*- lexical-binding: t; -*-

;; Parallel coding tasks: worktrees from worktrunk, agents as Hydra ACP
;; sessions (bin/bndos-task) that agent-shell attaches to, reviews in Hutch.
;; See README.org in this directory.  Needs the git module (hutch,
;; magit-treediff), the ai module (agent-shell) and hydra-acp on PATH.

(add-load-path! "lisp")

(use-package! bndos-task-dashboard
  :commands (bndos/task-dashboard bndos/task-select bndos/task-start bndos/task-review-here
             bndos/task-agent-here bndos/task-adopt bndos/task-set-parent bndos/task-add-project))

(map! :leader
      (:prefix ("g w" . "worktree tasks")
       :desc "Dashboard"        "w" #'bndos/task-dashboard
       :desc "Select task"      "s" #'bndos/task-select
       :desc "Start task"       "n" #'bndos/task-start
       :desc "Review this tree" "r" #'bndos/task-review-here
       :desc "Agent of this tree" "a" #'bndos/task-agent-here
       :desc "Adopt a conversation" "i" #'bndos/task-adopt
       :desc "Set parent branch"  "b" #'bndos/task-set-parent
       :desc "Add project"        "p" #'bndos/task-add-project))

;; Both buffers are read-only UIs in evil's normal state, like magit's: ESC,
;; visual selection and counts work as usual; the letter keys below are the
;; buffers' own, and the review's actions are on C-c C-<key> (like pr-review)
;; so they also work on a selected region.
(after! evil
  (evil-set-initial-state 'bndos-task-review-mode 'normal)
  (evil-set-initial-state 'bndos-task-dashboard-mode 'normal))

(map! :after bndos-task-review
      :map bndos-task-review-mode-map
      :n "RET"   #'bndos/task-review-visit
      :n "M-<return>" #'bndos/task-review-visit-worktree-other-window
      :n "TAB"   #'magit-section-toggle
      :n "q"     #'quit-window
      :n "g r"   #'bndos/task-review-refresh
      :n "g t"   #'bndos/task-review-toggle-tree
      :n "g f"   #'bndos/task-review-visit
      :n "]t"    #'bndos/task-review-next-thread
      :n "[t"    #'bndos/task-review-previous-thread
      :n "]]"    #'magit-section-forward-sibling
      :n "[["    #'magit-section-backward-sibling)

(map! :after bndos-task-dashboard
      :map bndos-task-dashboard-mode-map
      :n "RET"   #'bndos/task-dashboard-review
      :n "TAB"   #'magit-section-toggle
      :n "q"     #'quit-window
      :n "g r"   #'bndos/task-dashboard-refresh
      :n "v"     #'bndos/task-dashboard-agent
      :n "o"     #'bndos/task-dashboard-workspace
      :n "i"     #'bndos/task-dashboard-instruct
      :n "+"     #'bndos/task-start)
