;;; bndos/git/config.el -*- lexical-binding: t; -*-

(add-load-path! "lisp")

(after! magit
  (setq transient-display-buffer-action '(display-buffer-in-side-window (side . bottom))))

(use-package! magit-treediff
  :commands magit-treediff
  :init
  (setq magit-treediff-tree-backend 'treemacs)
  (map! :after magit
        :map magit-status-mode-map
        :nv "v" #'magit-treediff))


;;; Pull request review

(use-package! pr-review-worktree
  :commands pr-review-open-worktree)

(use-package! pr-review-treediff
  :commands pr-review-treediff-toggle)

(map! :after pr-review
      :map pr-review-mode-map
      "M-RET" #'magit-diff-visit-worktree-file-other-window
      :n "gw" #'pr-review-open-worktree
      :n "gt" #'pr-review-treediff-toggle
      :n "]t" #'bndos/pr-review-next-comment
      :n "[t" #'bndos/pr-review-previous-comment)

(use-package! code-review
  :defer t
  :config
  (setq code-review-auth-login-marker 'forge)
  (add-hook 'code-review-mode-hook #'emojify-mode)
  (map! :map code-review-mode-map
        "M-RET" #'magit-diff-visit-worktree-file-other-window))


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
