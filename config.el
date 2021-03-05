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
      doom-variable-pitch-font (font-spec :family "Monego" :size 20)
      doom-big-font (font-spec :family "Monego" :size 15))

(add-to-list 'default-frame-alist
             '(font . "Monego-10"))
(set-fontset-font "fontset-default" '(#xf000 . #xf23a) "all-the-icons")

(after! solaire-mode
  (solaire-global-mode -1))

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(custom-set-faces
 '(default ((t (:background "#000000"))))
 '(mode-line ((t (:background "#000000"))))
 '(header-line ((t (:background "#000000"))))
 '(match ((t (:background "#000000"))))
 '(lazy-highlight ((t (:background "#29422d"))))
 '(lsp-face-highlight-read ((t (:background "#29422d"))))
 '(lsp-face-highlight-write ((t (:background "#29422d"))))
 '(lsp-face-highlight-textual ((t (:background "#49322d"))))
 '(solaire-mode-line-face ((t (:background "#000000"))))
 '(solaire-mode-line-inactive-face ((t (:background "#000000"))))
 '(mode-line-inactive ((t (:background "#000000"))))
 '(hl-line ((t (:background "#171717")))))

(setq window-divider-default-bottom-width 0)
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'mhtml-mode-hook 'lsp)
(add-hook 'css-mode-hook 'lsp)
(setq lsp-disabled-clients '(angular-ls))
(setq lsp-headerline-breadcrumb-enable t)

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
