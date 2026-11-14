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

        ;; Help apart from command output, so neither takes the other's window.
        ("\\*help\\*"
         display-buffer-in-side-window
         (side . top) (slot . 3) (window-height . 0.15)
         (preserve-size . (t . nil)) (dedicated . side))

        ("\\*\\(?:grep\\|Completions\\|org-python-session\\|Python\\)\\*\\|^\\*compilation\\|^\\*Flutter\\|^\\*pytest\\|^\\*docker-build-output\\|^\\*ein:[^ ]\\|^\\* docker container"
         display-buffer-in-side-window
         (side . top) (slot . 4) (window-height . 0.15)
         (preserve-size . (t . nil)) (dedicated . side))

        ("\\*\\(?:shell\\|vterm\\)\\*"
         display-buffer-in-side-window
         (side . top) (slot . 5) (window-height . 0.15)
         (preserve-size . (nil . t)) (dedicated . side))))

(setq-default switch-to-buffer-in-dedicated-window 'pop)

;; Doom makes a new workspace's empty layout by cutting the live frame down to
;; its selected window: from a side window that leaves a lone side window, and
;; the next buffer shown pops up in a new frame.  Built as data instead, while
;; Doom's `+workspace-new' is still the one this replaces.
(defconst bndos-workspace-new-replaces "56d5f52bf8176614bece0e3593be01ca"
  "`bndos-workspace-new-upstream-hash' of the Doom version replaced.")

(defun bndos-workspace-blank-state (buffer &optional frame)
  "One window showing BUFFER, as window-state data for FRAME's workspaces.
Taken from one of FRAME's regular windows; FRAME is left as it is."
  (let ((window (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                          (window-list frame 'never))))
    (mapcar (lambda (entry)
              (if (eq (car-safe entry) 'buffer)
                  (cons 'buffer
                        (cons (buffer-name buffer)
                              (delq nil (mapcar (lambda (p)
                                                  (pcase (car-safe p)
                                                    ((or 'start 'point) (cons (car p) 1))
                                                    ('dedicated nil)
                                                    (_ p)))
                                                (cddr entry)))))
                entry))
            (seq-remove (lambda (entry) (eq (car-safe entry) 'parameters))
                        (window-state-get window t)))))

(defun bndos-workspace-new (name)
  "Doom's `+workspace-new', the empty layout built as data, not cut from the frame."
  (when (+workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (let ((persp (persp-add-new name)))
    ;; Not `setf': its expansion needs persp-mode loaded when this file is read.
    (aset persp (cl-struct-slot-offset 'perspective 'window-conf)
          (bndos-workspace-blank-state (doom-fallback-buffer)))
    persp))

(defvar bndos-workspace-new--upstream nil)
(defvar bndos-workspace-new--warned nil)

(defun bndos-workspace-new-upstream-hash ()
  "A hash of the source of Doom's `+workspace-new', read once."
  (with-memoization bndos-workspace-new--upstream
    (when-let* ((library (ignore-errors (cdr (find-function-library '+workspace-new))))
                (source (ignore-errors (find-library-name library))))
      (with-temp-buffer
        (insert-file-contents source)
        (when (re-search-forward "^(defun \\+workspace-new " nil t)
          (goto-char (match-beginning 0))
          (md5 (prin1-to-string (read (current-buffer)))))))))

(defun bndos-workspace-new-warn ()
  "Say once that Doom's `+workspace-new' changed, so its replacement stands down."
  (unless bndos-workspace-new--warned
    (setq bndos-workspace-new--warned t)
    (display-warning 'bndos "Doom's `+workspace-new' changed: `bndos-workspace-new' stands down; review it")))

(defadvice! bndos-workspace-new-a (fn name)
  :around #'+workspace-new
  (if (equal (bndos-workspace-new-upstream-hash) bndos-workspace-new-replaces)
      (bndos-workspace-new name)
    (bndos-workspace-new-warn)
    (funcall fn name)))

;; persp-mode writes a workspace to a file from the layout it stored when last
;; leaving it, so one on screen goes in as it was when entered.  Its whole-session
;; save stores the current layouts first; this does the same for any save (upstream
;; persp-mode too, as of 4.1).
(defadvice! bndos-persp-save-current-layouts-a (&optional phash names-regexp)
  :before #'persps-to-savelist
  (dolist (persp (persp-persps (or phash *persp-hash*) names-regexp))
    (when persp (persp-save-state persp))))

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
