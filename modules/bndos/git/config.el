;;; bndos/git/config.el -*- lexical-binding: t; -*-

(add-load-path! "lisp" "lisp/diff-review")

(after! transient
  (setq transient-display-buffer-action '(display-buffer-in-side-window (side . bottom))))

;;; diff-review: the worktree or some commits, file by file

(use-package! diff-review
  :commands (diff-review-branch diff-review-find-file)
  :config
  (add-hook! '(diff-review-mode-hook diff-review-sidebar-mode-hook) #'hl-line-mode)
  ;; The fringe is the vc gutter's: no hunk bars there, threads marked in
  ;; the margin, the diff's colours left as they are.
  (setq diff-review-fringe nil
        diff-review-thread-marker "▐"
        diff-review-tint-open-thread nil)
  ;; diff-review's suggested keys, without gc / zf / zd, which Doom uses.
  ;; Line comments are made in the diff view (g=); gC in a file opens it.
  (after! evil
    ;; Real lines, as relative numbers count them: removed lines and
    ;; threads are rows with no line of their own.
    (evil-define-minor-mode-key '(normal visual motion) 'diff-review-file-mode
      "j" #'evil-next-line
      "k" #'evil-previous-line)
    (evil-define-minor-mode-key 'normal 'diff-review-file-mode
      "]c" #'diff-review-next-hunk
      "[c" #'diff-review-previous-hunk
      "zv" #'diff-review-viewed-and-next
      "gC" #'diff-review-comment-in-view
      "g=" #'diff-review-toggle-view
      "]t" #'diff-review-next-comment
      "[t" #'diff-review-previous-comment
      "gL" #'diff-review-list-comments
      "gl" #'diff-review-overview
      "gq" #'diff-review-quit)
    (evil-define-minor-mode-key 'visual 'diff-review-file-mode
      "gC" #'diff-review-comment-in-view
      "g=" #'diff-review-toggle-view)
    (evil-set-initial-state 'diff-review-view-mode 'normal)
    (evil-define-key* '(normal visual) diff-review-view-mode-map
      "j" #'evil-next-line
      "k" #'evil-previous-line
      "gC" #'diff-review-view-comment
      "g=" #'diff-review-toggle-view
      "]c" #'diff-review-view-next-change
      "[c" #'diff-review-view-previous-change
      "]t" #'diff-review-next-comment
      "[t" #'diff-review-previous-comment
      "gL" #'diff-review-list-comments
      "gl" #'diff-review-overview
      "q"  #'diff-review-toggle-view)
    ;; Threads across the review's files from the overview and the sidebar too.
    (dolist (map (list diff-review-mode-map diff-review-sidebar-mode-map))
      (evil-define-key* 'normal map
        "]t" #'diff-review-next-comment
        "[t" #'diff-review-previous-comment
        "gL" #'diff-review-list-comments))))

(use-package! diff-review-magit
  :commands diff-review-magit-commits)

(map! :after magit
      :map magit-status-mode-map
      :nv "v" #'bndos/diff-review-dwim)

;; `d v' in magit's diff menu reviews the selected commits.
(after! magit
  (transient-append-suffix 'magit-diff 'magit-stash-show
    '("v" "Review" diff-review-magit-commits)))


;;; Pull request review

(use-package! pr-review-worktree
  :commands pr-review-open-worktree)

(use-package! pr-review-tree
  :commands pr-review-tree-toggle)

(after! pr-review
  (advice-add #'pr-review--ghub-common-request-args
              :filter-return #'bndos-pr-review-gh-auth-a))

(map! :after pr-review
      :map pr-review-mode-map
      "M-<return>" #'magit-diff-visit-worktree-file-other-window
      :n "gw" #'pr-review-open-worktree
      :n "gt" #'pr-review-tree-toggle
      :n "]t" #'bndos/pr-review-next-comment
      :n "[t" #'bndos/pr-review-previous-comment)

(use-package! code-review
  :defer t
  :config
  (setq code-review-auth-login-marker 'forge)
  (add-hook 'code-review-mode-hook #'emojify-mode)
  (map! :map code-review-mode-map
        "M-<return>" #'magit-diff-visit-worktree-file-other-window))


;;; consult-gh

(use-package! consult-gh
  :defer t
  :config
  (setq consult-gh-default-clone-directory "~/Projects"
        consult-gh-show-preview t
        consult-gh-preview-key "C-o"
        consult-gh-repo-action #'consult-gh--repo-browse-files-action
        consult-gh-large-file-warning-threshold 2500000
        consult-gh-confirm-name-before-fork nil
        consult-gh-confirm-before-clone t
        consult-gh-notifications-show-unread-only nil
        consult-gh-default-interactive-command #'consult-gh-transient
        consult-gh-prioritize-local-folder nil
        consult-gh-group-dashboard-by :reason
        consult-gh-repo-preview-major-mode nil
        consult-gh-preview-major-mode 'org-mode)
  (require 'consult-gh-transient)
  (consult-gh-enable-default-keybindings)
  (consult-gh-with-pr-review-mode 1))

(use-package! consult-gh-embark
  :after (consult-gh embark)
  :config (consult-gh-embark-mode 1))

(use-package! consult-gh-forge
  :after (consult-gh forge)
  :config
  (setq consult-gh-forge-timeout-seconds 20)
  (consult-gh-forge-mode 1))
