;;; bndos/windows/config.el -*- lexical-binding: t; -*-

;;; Display rules

(setq switch-to-buffer-obey-display-actions t
      display-buffer-alist
      '(("\\*Buffer List\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . top) (slot . 1) (window-height . 0.15)
         (preserve-size . (nil . t)) (dedicated . side))

        ("\\*Tag List\\*\\|^\\*julia"
         display-buffer-in-side-window
         (side . right) (slot . 0) (window-width . 0.2)
         (preserve-size . (t . nil)) (dedicated . side))

        ("^\\*magit:\\|^magit-diff"
         display-buffer-in-side-window
         (side . left) (slot . 3) (window-width . 0.2)
         (preserve-size . (t . nil)) (dedicated . side))

        ("COMMIT_EDITMSG"
         display-buffer-in-side-window
         (side . left) (slot . 2) (window-width . 0.2)
         (preserve-size . (t . nil)) (dedicated . side))

        ("\\*\\(?:help\\|grep\\|Completions\\|org-python-session\\|Python\\)\\*\\|^\\*compilation\\|^\\*Flutter\\|^\\*pytest\\|^\\*docker-build-output\\|^\\*ein:[^ ]\\|^\\* docker container"
         display-buffer-in-side-window
         (side . top) (slot . 4) (window-height . 0.15)
         (preserve-size . (t . nil)) (dedicated . side))

        ("\\*\\(?:shell\\|vterm\\)\\*"
         display-buffer-in-side-window
         (side . top) (slot . 5) (window-height . 0.15)
         (preserve-size . (nil . t)) (dedicated . side))))

(setq-default switch-to-buffer-in-dedicated-window 'pop)

;; Give ediff its own frame instead of rearranging the current one.
(add-hook 'ediff-before-setup-hook #'bndos-ediff-new-frame-h)


;;; Keys

(map! "H-h" #'windmove-left
      "H-l" #'windmove-right
      "H-k" #'windmove-up
      "H-j" #'windmove-down
      "H-H" #'buf-move-left
      "H-L" #'buf-move-right
      "H-K" #'buf-move-up
      "H-J" #'buf-move-down
      "H-M-h" (cmd! (shrink-window-horizontally 10))
      "H-M-l" (cmd! (enlarge-window-horizontally 10))
      "H-M-j" (cmd! (shrink-window 10))
      "H-M-k" (cmd! (enlarge-window 10))
      "H-/" #'winner-undo
      "H-?" #'winner-redo
      "H-<tab>" #'mode-line-other-buffer

      (:leader
       "w a"   #'ace-window
       "w x"   #'window-toggle-side-windows
       "w <"   (cmd! (shrink-window-horizontally 10))
       "w >"   (cmd! (enlarge-window-horizontally 10))
       "w +"   (cmd! (enlarge-window 10))
       "w -"   (cmd! (shrink-window 10))
       "w M-J" (cmd! (bndos/window-move-to-side 'top -1 0.15))
       "w M-K" (cmd! (bndos/window-move-to-side 'top 1 0.15))
       "w M-L" (cmd! (bndos/window-move-to-side 'right 1 0.35))
       "w M-H" (cmd! (bndos/window-move-to-side 'left 1 0.20))
       "TAB TAB" #'bndos/workspace-other
       "TAB '"   #'+workspace/display))
