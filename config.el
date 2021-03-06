;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq doom-font (font-spec :family "Monego" :size 11)
      doom-variable-pitch-font (font-spec :family "Monego" :size 18)
      doom-big-font (font-spec :family "Monego" :size 18))

(add-to-list 'default-frame-alist
             '(font . "Monego-10"))
(set-fontset-font "fontset-default" '(#xf000 . #xf23a) "all-the-icons")

(after! solaire-mode
  (solaire-global-mode -1))

(setq-default header-line-format " ")
(set-fringe-mode 25)
(setq-default left-fringe-width  25)
(setq-default right-fringe-width 25)

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic nil)

(custom-set-faces
 '(default ((t (:background "#000000" :foreground "#ffffff"))))
 '(mode-line ((t (:background nil))))
 '(header-line ((t (:background nil))))
 '(magit-header-line ((t (:background nil :box nil))))
 '(match ((t (:background nil))))
 '(font-lock-comment-face ((t (:foreground "#444444"))))
 '(ivy-virtual ((t (:foreground "#444444" :italic nil))))
 '(ivy-current-match ((t (:background "#bd93f9" :foreground "#000000" :inherit bold))))
 '(org-block-begin-line ((t (:background nil))))
 '(org-block ((t (:background nil))))
 '(org-block-end-line ((t (:background nil))))
 '(whitespace-tab ((t (:background nil))))
 '(whitespace-space ((t (:background nil))))
 '(lazy-highlight ((t (:background "#29422d"))))
 '(lsp-face-highlight-read ((t (:background "#29422d"))))
 '(lsp-face-highlight-write ((t (:background "#29422d"))))
 '(lsp-face-highlight-textual ((t (:background "#49322d"))))
 '(solaire-mode-line-face ((t (:background nil))))
 '(solaire-mode-line-inactive-face ((t (:background nil))))
 '(mode-line-inactive ((t (:background nil))))
 '(hl-line ((t (:background "#171717")))))

(setq window-divider-default-bottom-width 0)
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)


(after! counsel-projectile
  (ivy-set-display-transformer
 'counsel-projectile-find-file
 'counsel-projectile-find-file-transformer))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'mhtml-mode-hook 'lsp)
(add-hook 'css-mode-hook 'lsp)

(setq lsp-disabled-clients '(angular-ls))

(add-hook 'lsp-mode-hook (lambda ()
                          (setq header-line-format nil)
                          (lsp-headerline-breadcrumb-mode)))

(defvar-local my/flycheck-local-cache nil)

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

(global-set-key (kbd "H-h") 'windmove-left)
(global-set-key (kbd "H-l") 'windmove-right)
(global-set-key (kbd "H-k") 'windmove-up)
(global-set-key (kbd "H-j") 'windmove-down)

(global-set-key (kbd "H-M-h") 'shrink-window-horizontally)
(global-set-key (kbd "H-M-l") 'enlarge-window-horizontally)
(global-set-key (kbd "H-M-k") 'enlarge-window)
(global-set-key (kbd "H-M-j") 'shrink-window)

(global-set-key (kbd "H-K") 'buf-move-up)
(global-set-key (kbd "H-J") 'buf-move-down)
(global-set-key (kbd "H-H") 'buf-move-left)
(global-set-key (kbd "H-L") 'buf-move-right)

(global-set-key (kbd "H-/") 'winner-undo)
(global-set-key (kbd "H-?") 'winner-redo)

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "H-<tab>") 'switch-to-previous-buffer)


(setq-local MODELINE '(getenv "MODELINE"))
(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position selection-info)
    '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker "  ")))

(global-set-key (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(defun dired-open-in-external-app ()
  "Open the file(s) at point with an external application."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (mapc
     (lambda (file-path)
       (let ((process-connection-type nil))
         (start-process "" nil "xdg-open" file-path)))
     file-list)))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-o")
              (lambda () (interactive) (dired-open-in-external-app)))))

(map! :leader
      :desc "Open file externally"
      "f o" #'counsel-find-file-extern)


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

(global-set-key (kbd "C-x w") 'window-toggle-side-windows)

(global-set-key (kbd "H-!") (lambda()
                              (interactive)
                              (display-buffer-in-side-window (get-buffer (buffer-name)) '((side . top) (slot . -1) (window-height . 0.15)))))
(global-set-key (kbd "H-@") (lambda()
                              (interactive)
                              (display-buffer-in-side-window (get-buffer (buffer-name)) '((side . top) (slot . 1) (window-height . 0.15)))))
(global-set-key (kbd "H-#") (lambda()
                              (interactive)
                              (display-buffer-in-side-window (get-buffer (buffer-name)) '((side . right) (slot . 1) (window-width . 0.35)))))

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

(global-set-key (kbd "M-V") 'projectile-vterm)

(setq vterm-buffer-name-string "*vterm %s*")

(map! :leader
      :desc "Lsp symbols"
      "o s" #'lsp-treemacs-symbols)
