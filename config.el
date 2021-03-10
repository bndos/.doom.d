(setq display-line-numbers-type nil)

;; Theme
(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "Monego" :size 18)
      doom-variable-pitch-font (font-spec :family "Monego" :size 18)
      doom-big-font (font-spec :family "Monego" :size 24))

(add-to-list 'default-frame-alist
             '(font . "Monego-10"))
(set-fontset-font "fontset-default" '(#xf000 . #xf23a) "all-the-icons")

(after! solaire-mode
  (solaire-global-mode -1))

(setq-default header-line-format " ")
(set-fringe-mode 25)
(setq-default left-fringe-width  25)
(setq-default right-fringe-width 25)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic nil)

(after! doom-modeline
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))

(after! ivy
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil :background "#ff79c6" :foreground "#000000")
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil :background "#ff79c6" :foreground "#000000")
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil :background "#ff79c6" :foreground "#000000")
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil :background "#ff79c6" :foreground "#000000")
  )

(custom-set-faces!
  '(default :background "#000000" :foreground "#ffffff")
  '(mode-line :background nil)
  '(header-line :background nil)
  '(magit-header-line :background nil :box nil)
  '(match :background nil)
  '(org-block-begin-line :background nil)
  '(org-block :background nil)
  '(org-block-end-line :background nil)
  '(whitespace-tab :background nil)
  '(whitespace-space :background nil)
  '(solaire-mode-line-face :background nil)
  '(solaire-mode-line-inactive-face :background nil)
  '(mode-line-inactive :background nil)
  '(ivy-virtual :foreground "#444444" :italic nil)
  '(ivy-current-match :background "#bd93f9" :foreground "#000000" :inherit bold)
  '(font-lock-comment-face :foreground "#444444")
  '(font-lock-variable-name-face :foreground "#ffb86c")
  '(hl-line :background "#171717")
  '(region :background "#2f2157")
  )

(setq-local MODELINE '(getenv "MODELINE"))
(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position selection-info)
    '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker "  ")))

(setq window-divider-default-bottom-width 0)

(after! git-gutter-fringe
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "........"
    "..XXXX.."
    "..XXXX.."
    "..XXXX.."
    "..XXXX.."
    "..XXXX.."
    "..XXXX.."
    "........")
  (define-fringe-bitmap 'git-gutter-fr:deleted [224]
      nil nil '(center repeated)))

;; evil
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

;; counsel-projectile
(after! counsel-projectile
  (ivy-set-display-transformer
   'counsel-projectile-find-file
   'counsel-projectile-find-file-transformer))

;; lsp/flycheck
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(setq lsp-disabled-clients '(angular-ls))

(after! lsp-clangd
  (set-lsp-priority! 'clangd 1))  ; ccls has priority 0

(add-hook 'lsp-mode-hook (lambda ()
                           (setq header-line-format nil)
                           (lsp-headerline-breadcrumb-mode)))

(defvar-local my/flycheck-local-cache nil)

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (python-pylint)))))))))


(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (or (derived-mode-p 'typescript-mode)
                      (string-equal "tsx" (file-name-extension buffer-file-name)))
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (typescript-tslint)))))))))

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'js-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (javascript-eslint)))))))))

;; dap-mode
(after! dap-mode
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (setq dap-output-buffer-filter '("stdout"))
  (map! :leader
        :desc "Dap debug"
        "d d" #'dap-debug)
  (map! :leader
        :desc "Dap toggle breakpoint"
        "d b" #'dap-breakpoint-toggle)
  (map! :leader
        :desc "Dap debug"
        "d h" #'dap-hydra))

;; treemacs
(setq treemacs-is-never-other-window nil)
;; lsp-treemacs
(map! :leader
      :desc "Lsp symbols"
      "o s" #'lsp-treemacs-symbols)

;; smartparens
(after! smartparens
  (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp))

;; dired
(after! dired-x
  (defun dired-open-in-external-app ()
    "Open the file(s) at point with an external application."
    (interactive)
    (let ((file-list (dired-get-marked-files)))
      (mapc
       (lambda (file-path)
         (let ((process-connection-type nil))
           (start-process "" nil "gio" "open" file-path)))
       file-list)))

  (define-key dired-mode-map (kbd "M-o")
    (lambda () (interactive) (dired-open-in-external-app))))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)))

;; window-rules
(defvar parameters
  '(window-parameters . ((no-delete-other-windows . t))))

(setq
 display-buffer-alist
 `(("\\*Buffer List\\*" display-buffer-in-side-window
    (side . bottom) (slot . 0) (window-height . fit-window-to-buffer)
    (preserve-size . (nil . t)) ,parameters)
   ("\\*Tags List\\*" display-buffer-in-side-window
    (side . right) (slot . 0) (window-width . fit-window-to-buffer)
    (preserve-size . (t . nil)) ,parameters)
   ("^magit:" display-buffer-in-side-window
    (side . left) (slot . 3) (window-width . 0.2)
    (preserve-size . (t . nil)) ,parameters)
   ("\\*\\(?:help\\|grep\\|Completions\\)\\*\\|^*compilation"
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . top) (slot . -1) (preserve-size . (nil . t)) (window-height . 0.15)
    ,parameters)
   ("\\*\\(?:shell\\|vterm\\)\\*"
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . top) (slot . 1) (preserve-size . (nil . t)) (window-height . 0.15)
    ,parameters)))

(map! :leader
      :desc "Toggle side windows"
      "w x" #'window-toggle-side-windows)

;; vterm
(defun projectile-vterm ()
  (interactive)
  (if (projectile-project-p)
      (let* ((project (projectile-project-root)))
        (unless (require 'vterm nil 'noerror)
          (error "Package 'vterm' is not available"))
        (projectile-with-default-dir project
          (vterm "*vterm*")
          (vterm-send-string "cd .")
          (vterm-send-return)))
    (unless (require 'vterm nil 'noerror)
      (error "Package 'vterm' is not available"))
    (vterm "*vterm*")
    (vterm-send-string "cd .")
    (vterm-send-return)))

(map! :desc "Open vterm in project root or current dir"
      "M-V" #'projectile-vterm)

(setq vterm-buffer-name-string "*vterm %s*")

;; custom binds
(global-set-key (kbd "H-!") (lambda()
                              (interactive)
                              (display-buffer-in-side-window (get-buffer (buffer-name)) '((side . top) (slot . -1) (window-height . 0.15)))))
(global-set-key (kbd "H-@") (lambda()
                              (interactive)
                              (display-buffer-in-side-window (get-buffer (buffer-name)) '((side . top) (slot . 1) (window-height . 0.15)))))
(global-set-key (kbd "H-#") (lambda()
                              (interactive)
                              (display-buffer-in-side-window (get-buffer (buffer-name)) '((side . right) (slot . 1) (window-width . 0.35)))))

(defun open-nautilus ()
  (interactive)
  (call-process "nautilus" nil 0 nil "."))

(map! :desc "Open nautilus in current dir"
      "C-c C-n" #'open-nautilus)

(map! :desc "Redo"
      :i
      "C-?" #'undo-fu-only-redo)

(map! :desc "Redo all"
      :i
      "C-M-/" #'undo-fu-only-redo-all)

(global-set-key (kbd "H-d") (lambda ()
                              (interactive)
                              (scroll-up 4)
                              (setq this-command 'next-line)
                              (forward-line 4)))
(global-set-key (kbd "H-u") (lambda ()
                              (interactive)
                              (scroll-down 4)
                              (setq this-command 'previous-line)
                              (forward-line -4)))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "H-<tab>") 'switch-to-previous-buffer)

(defun my-ivy-read (prompt)
  (ivy-read prompt (seq-filter
                    (lambda (x) (and (or (string-match-p "^*compilation" x)
                                         (string-match-p "^*vterm" x)
                                         (string-match-p "^magit:" x))
                                     (not (string-equal (buffer-name) x))))
                    (mapcar #'buffer-name (buffer-list)))))

(defun ivy-compilation-buffers (&optional name)
  "Read desktop with a name."
  (interactive)
  (unless name
    (setq name (my-ivy-read "compilation buffers: ")))
  (switch-to-buffer name))

(global-set-key (kbd "H-x b") 'ivy-compilation-buffers)

(defun my-make-room-for-new-compilation-buffer ()
  "Renames existing *compilation* buffer to something unique so
         that a new compilation job can be run."
  (interactive)
  (let ((cbuf (get-buffer (concat "*compilation*<" (projectile-project-name) ">")))
        (more-cbufs t)
        (n 1)
        (new-cbuf-name ""))
    (when cbuf
      (while more-cbufs
        (setq new-cbuf-name (concat (format "*compilation %d*<" n) compile-command " " (projectile-project-name) ">"))
        (setq n (1+ n))
        (setq more-cbufs (get-buffer new-cbuf-name)))
      (with-current-buffer cbuf
        (rename-buffer new-cbuf-name)))))

(map! :leader
      :desc "Rename compile buffer"
      "c n" #'my-make-room-for-new-compilation-buffer)

;; remaping
(after! company
  (define-key company-active-map (kbd "<backtab>") 'counsel-company))

(global-set-key (kbd "H-h") 'windmove-left)
(global-set-key (kbd "H-l") 'windmove-right)
(global-set-key (kbd "H-k") 'windmove-up)
(global-set-key (kbd "H-j") 'windmove-down)

(global-set-key (kbd "H-M-h") 'shrink-window-horizontally)
(global-set-key (kbd "H-M-l") 'enlarge-window-horizontally)
(global-set-key (kbd "H-M-k") 'enlarge-window)
(global-set-key (kbd "H-M-j") 'shrink-window)

(use-package! buffer-move
  :bind (("H-K" . buf-move-up)
         ("H-J" . buf-move-down)
         ("H-H" . buf-move-left)
         ("H-L" . buf-move-right)))

(global-set-key (kbd "H-/") 'winner-undo)
(global-set-key (kbd "H-?") 'winner-redo)

(map! :leader
      :desc "Open file externally"
      "f o" #'counsel-find-file-extern)
