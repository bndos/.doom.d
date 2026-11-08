;;; bndos/debug/config.el -*- lexical-binding: t; -*-

;; Install the debug adapters with ./setup-debuggers.sh.

(after! dape
  (setq dape-request-timeout 60
        dape-buffer-window-arrangement 'gud
        dape-info-hide-mode-line nil
        dape-inlay-hints t
        dape-cwd-function #'projectile-project-root)

  ;; Breakpoints persist across sessions.
  (dape-breakpoint-load)
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  (dape-breakpoint-global-mode 1)

  (add-hook 'dape-start-hook #'dape-info)
  (add-hook 'dape-start-hook #'dape-repl)
  (add-hook 'dape-start-hook #'bndos-dape-save-buffers-h)
  (add-hook 'dape-update-ui-hook #'dape-info-update)
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  (advice-add #'dape :around #'bndos-dape-inject-env-a)

  (dolist (key '(debugpy debugpy-module))
    (when-let* ((entry (assq key dape-configs)))
      (setcdr entry (plist-put (cdr entry) 'command #'bndos-dape-python-command))))

  (add-to-list 'dape-configs
               '(go-test
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-cwd dape-command-cwd
                 command-args ("dap" "--listen" "127.0.0.1:55878")
                 host "127.0.0.1"
                 port 55878
                 :type "go"
                 :name "debug test"
                 :request "launch"
                 :mode "test"
                 :cwd dape-cwd
                 :program bndos-dape-go-package-dir)))
