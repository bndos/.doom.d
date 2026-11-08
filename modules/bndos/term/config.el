;;; bndos/term/config.el -*- lexical-binding: t; -*-

(map! "M-V"        #'bndos/vterm-project
      "H-<return>" #'bndos/terminal-project
      :leader "c n" #'bndos/compile-keep-previous-buffer)

(after! vterm
  (setq vterm-shell "zsh"
        vterm-environment '("COLORTERM=truecolor" "NO_COLOR")
        ;; Keep buffer names stable for persp-mode.  Agent CLIs emit transient
        ;; or blank titles; naming buffers after them makes restored window
        ;; configs point at buffers that no longer exist.  The title goes in
        ;; the header line instead.
        vterm-buffer-name-string nil)

  (add-hook 'vterm-mode-hook #'bndos-vterm-header-line-h)
  (advice-add #'vterm--set-title :before #'bndos-vterm-record-title-a)
  ;; Shells inside vterm get EMACS_SERVER_NAME, so zsh can call back into this
  ;; Emacs (see `bndos/compile-from-file').
  (advice-add #'vterm-mode :around #'bndos-vterm-emacsclient-env-a)

  (map! :map vterm-mode-map
        "M-RET"      #'bndos/vterm-send-alt-return
        "M-<return>" #'bndos/vterm-send-alt-return
        "C-<return>" #'bndos/vterm-send-alt-return))
