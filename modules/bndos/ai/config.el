;;; bndos/ai/config.el -*- lexical-binding: t; -*-

;;; Copilot

(defvar bndos-copilot-enabled t
  "Whether Copilot auto-enables in supported buffers.  See `bndos/copilot-toggle'.")

(map! :leader :desc "Toggle Copilot globally" "t C" #'bndos/copilot-toggle)
;; Global, so major-mode S-RET bindings (e.g. org tables) still win.
(map! "S-<return>" #'copilot-accept-completion)

;; The EIN test daemon shouldn't spawn a Copilot server.
(unless (getenv "EIN_REGRESSION_TEST")
  (add-hook! '(prog-mode-hook org-mode-hook markdown-mode-hook gfm-mode-hook)
             #'bndos-copilot-maybe-enable-h))

(after! copilot
  (setq copilot-server-executable
        (file-name-concat copilot-install-dir "bin" "copilot-language-server")
        copilot-indent-offset-warning-disable t
        copilot-max-char-warning-disable t
        copilot-lsp-settings
        '(:github (:copilot (:enable (:* t :plaintext t :markdown t :rust t)))))

  (add-to-list 'copilot-disable-predicates #'bndos-copilot-buffer-too-large-p)
  (add-hook 'copilot-mode-hook #'bndos-copilot-disable-if-too-large-h)
  (advice-add #'copilot--inline-completion-params
              :filter-return #'bndos-copilot-add-doc-version-a)

  (map! :map copilot-mode-map
        "S-<tab>"    #'copilot-accept-completion-by-word
        :map copilot-completion-map
        "TAB"        #'copilot-accept-completion
        "S-<return>" #'copilot-accept-completion
        "C-<tab>"    #'copilot-accept-completion-by-word
        "H-<tab>"    #'copilot-accept-completion-by-line
        "M-n"        #'copilot-next-completion
        "M-p"        #'copilot-previous-completion))


;;; gptel

(map! "C-c c g" #'gptel-menu
      "C-c c c" #'gptel)

(after! gptel
  (setq gptel-default-mode 'org-mode)
  (auth-source-pass-enable))


;;; agent-shell

(after! agent-shell
  (setq agent-shell-openai-codex-acp-command
        '("codex-acp"
          "-c" "approval_policy=\"never\""
          "-c" "sandbox_mode=\"danger-full-access\"")
        agent-shell-session-strategy 'prompt
        agent-shell-google-authentication (agent-shell-google-make-authentication :login t)
        ;; Keep ACP streaming responsive: every streamed update renders on
        ;; Emacs' UI thread.
        agent-shell-show-busy-indicator nil
        agent-shell-transcript-file-path-function nil
        agent-shell-highlight-blocks nil)
  (map! :map agent-shell-mode-map
        :n "RET"      #'bndos/agent-shell-ret-dwim
        :n "<return>" #'bndos/agent-shell-ret-dwim))

(after! agent-shell-markdown
  (setq agent-shell-markdown-prettify-tables t))
