;;; bndos/tools/config.el -*- lexical-binding: t; -*-

(map! "C-c C-n" #'bndos/open-file-manager
      (:leader
       "f o" #'bndos/open-externally
       "f ." #'find-name-dired
       "k k" #'kubed-prefix-map))


;;; Projectile

(after! projectile
  (setq projectile-indexing-method 'alien
        projectile-enable-caching nil
        projectile-generic-command nil
        projectile-git-use-fd nil
        projectile-async-indexing nil))


;;; Dired

(map! :after dired
      :map dired-mode-map
      "M-o" #'bndos/dired-open-externally)

;; Dired (and Dirvish, below) can leave dead or malformed entries in
;; `dired-buffers', which later breaks with (wrong-type-argument stringp nil).
(after! dired
  (advice-add #'dired-find-buffer-nocreate :before #'bndos-dired-prune-buffers-a)
  (advice-add #'dired-buffers-for-dir :before #'bndos-dired-prune-buffers-a))

(after! dirvish
  (setq dirvish-hide-details t
        dirvish-side-window-parameters '((no-delete-other-windows . t)))
  (advice-add #'dirvish-dired-noselect-a :around #'bndos-dirvish-noselect-safe-a))
