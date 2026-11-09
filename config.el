;;; config.el -*- lexical-binding: t; -*-

(when (featurep :system 'macos)
  (setq mac-command-modifier 'hyper))

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo")
      warning-minimum-level :emergency)

(dolist (dir '("~/.nix-profile/bin"
               "/nix/var/nix/profiles/default/bin"
               "/usr/lib/llvm-20/bin"))
  (let ((dir (expand-file-name dir)))
    (when (file-directory-p dir)
      (add-to-list 'exec-path dir)
      (setenv "PATH" (concat dir path-separator (getenv "PATH"))))))
