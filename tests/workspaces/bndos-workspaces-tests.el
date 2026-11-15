;;; bndos-workspaces-tests.el --- Workspaces in a GUI Emacs with the config -*- lexical-binding: t; -*-

;; Run by `tests/tasks/run-tests.sh --gui', in a daemon running the whole
;; config with a frame, its workspace files under $WS_TEST_DIR.  Each test
;; starts from one workspace, one window, one frame, and compares layouts
;; window by window: buffer, side, slot, dedication, point.

(require 'ert)
(require 'cl-lib)

(defvar bndos-ws-dir (expand-file-name "files/" (getenv "WS_TEST_DIR")))

(defun bndos-ws-gui-frame-p (frame)
  "FRAME is a frame of its own on the screen, not a child (posframes, popups)."
  (and (eq (framep frame) 'ns) (not (frame-parameter frame 'parent-frame))))

(defvar bndos-ws-main-frame (seq-find #'bndos-ws-gui-frame-p (frame-list))
  "The frame the tests run in, taken once: the frame list's order changes.")

(defun bndos-ws-frame () bndos-ws-main-frame)
(defun bndos-ws-frames () (seq-count #'bndos-ws-gui-frame-p (frame-list)))

(defun bndos-ws-file (name)
  "A buffer visiting file NAME under `bndos-ws-dir', made if needed."
  (make-directory bndos-ws-dir t)
  (let ((path (expand-file-name name bndos-ws-dir)))
    (unless (file-exists-p path)
      (with-temp-file path (dotimes (i 200) (insert (format "%s line %d\n" name i)))))
    (find-file-noselect path)))

(defun bndos-ws-layout ()
  "The frame's windows from the first: (BUFFER SIDE SLOT DEDICATED POINT)."
  (mapcar (lambda (w)
            (list (buffer-name (window-buffer w))
                  (window-parameter w 'window-side)
                  (window-parameter w 'window-slot)
                  (window-dedicated-p w)
                  (and (not (window-parameter w 'window-side)) (window-point w))))
          (window-list (bndos-ws-frame) 'never (frame-first-window (bndos-ws-frame)))))

(defun bndos-ws-regular-window ()
  (seq-find (lambda (w) (not (window-parameter w 'window-side))) (window-list (bndos-ws-frame) 'never)))

(defun bndos-ws-blank-p ()
  (and (= (length (window-list (bndos-ws-frame) 'never)) 1)
       (not (window-parameter (frame-first-window (bndos-ws-frame)) 'window-side))))

(defun bndos-ws-side (name)
  "Show buffer NAME through `display-buffer-alist'."
  (display-buffer (get-buffer-create name)))

(defun bndos-ws-window-of (prefix)
  (seq-find (lambda (w) (string-prefix-p prefix (buffer-name (window-buffer w))))
            (window-list (bndos-ws-frame) 'never)))

(defun bndos-ws-reset ()
  "Only the main workspace, one window on *scratch*, one frame, no saved workspaces."
  (select-frame (bndos-ws-frame))
  (dolist (f (frame-list))
    (when (and (bndos-ws-gui-frame-p f) (not (eq f (bndos-ws-frame)))) (delete-frame f)))
  (+workspace-switch +workspaces-main t)
  (dolist (n (+workspace-list-names)) (unless (equal n +workspaces-main) (+workspace-kill n)))
  (unless (string-prefix-p (getenv "WS_TEST_DIR") persp-save-dir)
    (error "persp-save-dir isn't the test's: %s" persp-save-dir))
  (when (file-directory-p persp-save-dir) (delete-directory persp-save-dir t))
  (make-directory persp-save-dir t)
  ;; ERT puts back the windows from before each test (`save-window-excursion'),
  ;; into whichever workspace is current then: start from one plain window.
  (let ((w (or (bndos-ws-regular-window) (frame-first-window (bndos-ws-frame))))
        (ignore-window-parameters t))
    (select-window w)
    (delete-other-windows w)
    (dolist (param '(window-side window-slot))
      (set-window-parameter w param nil))
    (set-window-dedicated-p w nil))
  (switch-to-buffer "*scratch*")
  (evil-normal-state)
  (redisplay t))

(defun bndos-ws-keys (keys)
  "Press KEYS in the frame as typed."
  (with-current-buffer (window-buffer (selected-window)) (evil-normal-state))
  (execute-kbd-macro (kbd keys))
  (redisplay t))

(defmacro bndos-ws-test (name &rest body)
  "Define test NAME: BODY from a reset frame, ending with one frame."
  (declare (indent 1))
  `(ert-deftest ,name ()
     (with-selected-frame (bndos-ws-frame)
       (bndos-ws-reset)
       ,@body
       (should (= (bndos-ws-frames) 1)))))

(defun bndos-ws-busy-layout ()
  "Main area split on two files, magit on the left, help and vterm on top."
  (switch-to-buffer (bndos-ws-file "main-a.txt"))
  (goto-char (point-min)) (forward-line 40)
  (select-window (split-window-right))
  (switch-to-buffer (bndos-ws-file "main-b.txt"))
  (goto-char (point-min)) (forward-line 80)
  (bndos-ws-side "*magit: repo")
  (bndos-ws-side "*help*")
  (bndos-ws-side "*vterm*"))

;;; Creating

(defun bndos-ws-create-from (select)
  (bndos-ws-busy-layout)
  (funcall select)
  (let ((before (bndos-ws-layout)))
    (+workspace/new "fresh")
    (should (equal (+workspace-current-name) "fresh"))
    (should (bndos-ws-blank-p))
    (should (= (bndos-ws-frames) 1))
    (+workspace-switch +workspaces-main)
    (should (equal (bndos-ws-layout) before))))

(bndos-ws-test bndos-ws-create-from-a-regular-window
  (bndos-ws-create-from (lambda () (select-window (bndos-ws-regular-window)))))

(bndos-ws-test bndos-ws-create-from-the-left-side-window
  (bndos-ws-create-from (lambda () (select-window (get-buffer-window "*magit: repo")))))

(bndos-ws-test bndos-ws-create-from-a-top-side-window
  (bndos-ws-create-from (lambda () (select-window (get-buffer-window "*help*")))))

(bndos-ws-test bndos-ws-create-from-the-other-top-side-window
  (bndos-ws-create-from (lambda () (select-window (get-buffer-window "*vterm*")))))

(bndos-ws-test bndos-ws-switch-project-from-a-side-window
  (bndos-ws-busy-layout)
  (select-window (get-buffer-window "*magit: repo"))
  (let ((before (bndos-ws-layout)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p coll &rest _)
                 (let ((all (all-completions "" coll)))
                   (or (seq-find (lambda (c) (string-match-p "init\\.el\\'" c)) all) (car all))))))
      (projectile-switch-project-by-name "~/.config/doom/"))
    (should (equal (+workspace-current-name) "doom"))
    (should (equal (buffer-name (window-buffer (selected-window))) "init.el"))
    (should (= (bndos-ws-frames) 1))
    (+workspace-switch +workspaces-main)
    (should (equal (bndos-ws-layout) before))
    ;; Again: the workspace exists now.
    (select-window (get-buffer-window "*help*"))
    (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (car (all-completions "" coll)))))
      (projectile-switch-project-by-name "~/.config/doom/"))
    (should (equal (+workspace-current-name) "doom"))
    (should (= (seq-count (lambda (n) (string-prefix-p "doom" n)) (+workspace-list-names)) 1))))

;;; Switching keeps everything

(bndos-ws-test bndos-ws-twelve-workspaces-keep-their-layouts
  (let (recorded)
    (dotimes (i 12)
      (let ((name (format "ws%02d" i)))
        (+workspace/new name)
        (switch-to-buffer (bndos-ws-file (format "%s.txt" name)))
        (goto-char (point-min)) (forward-line (* 3 i))
        (when (cl-evenp i)
          (select-window (split-window-below))
          (switch-to-buffer (bndos-ws-file (format "%s-other.txt" name))))
        (pcase (% i 3)
          (0 (bndos-ws-side "*magit: repo"))
          (1 (bndos-ws-side "*vterm*")))
        (push (cons name (bndos-ws-layout)) recorded)))
    (dotimes (_ 2)
      (dolist (entry (reverse recorded))
        (+workspace-switch (car entry))
        (should (equal (cons (car entry) (bndos-ws-layout)) entry))))
    (+workspace-switch "ws00")
    (dotimes (_ 13)
      (bndos-ws-keys "SPC TAB ]")
      (when-let* ((want (assoc (+workspace-current-name) recorded)))
        (should (equal (cons (car want) (bndos-ws-layout)) want))))))

(bndos-ws-test bndos-ws-side-windows-stay-side-windows
  (bndos-ws-busy-layout)
  (let ((before (bndos-ws-layout)))
    (+workspace/new "other")
    (switch-to-buffer (bndos-ws-file "other.txt"))
    (bndos-ws-side "*Tag List*")
    (let ((other (bndos-ws-layout)))
      (dotimes (_ 5)
        (+workspace-switch +workspaces-main)
        (should (equal (bndos-ws-layout) before))
        (dolist (w (window-list (bndos-ws-frame) 'never))
          (when (window-parameter w 'window-side)
            (should (eq (window-dedicated-p w) 'side))))
        (+workspace-switch "other")
        (should (equal (bndos-ws-layout) other))))))

;;; Pop-ups

(defun bndos-ws-main-buffers ()
  (mapcar (lambda (w) (buffer-name (window-buffer w)))
          (seq-remove (lambda (w) (window-parameter w 'window-side)) (window-list (bndos-ws-frame) 'never))))

(bndos-ws-test bndos-ws-pop-ups-take-their-own-side-slot
  (switch-to-buffer (bndos-ws-file "main-a.txt"))
  (select-window (split-window-right))
  (switch-to-buffer (bndos-ws-file "main-b.txt"))
  (bndos-ws-side "*magit: repo")
  (bndos-ws-side "*vterm*")
  (let ((main (bndos-ws-main-buffers)))
    (bndos-ws-side "*help*")
    (should (eq (window-parameter (get-buffer-window "*help*") 'window-side) 'top))
    (should (get-buffer-window "*vterm*"))
    (should (get-buffer-window "*magit: repo"))
    (should (equal (bndos-ws-main-buffers) main))
    ;; A file opened or popped from a side window goes to the main area.
    (select-window (get-buffer-window "*magit: repo"))
    (switch-to-buffer (bndos-ws-file "opened-from-side.txt"))
    (should (get-buffer-window "*magit: repo"))
    (should-not (window-parameter (get-buffer-window "opened-from-side.txt") 'window-side))
    (select-window (get-buffer-window "*vterm*"))
    (pop-to-buffer (bndos-ws-file "popped-from-side.txt"))
    (should (get-buffer-window "*vterm*"))
    (should-not (window-parameter (get-buffer-window "popped-from-side.txt") 'window-side))
    (let ((before (bndos-ws-layout)))
      (+workspace/new "elsewhere")
      (bndos-ws-side "*help*")
      (+workspace-switch +workspaces-main)
      (should (equal (bndos-ws-layout) before)))))

(bndos-ws-test bndos-ws-real-help-magit-and-compile-windows
  (switch-to-buffer (bndos-ws-file "real.txt"))
  (let ((default-directory "~/.config/doom/"))
    (magit-status-setup-buffer default-directory))
  (let ((default-directory bndos-ws-dir)) (compile "echo hi"))
  (sleep-for 1)
  (describe-function 'car)
  (let ((before (bndos-ws-layout)))
    (should (eq (window-parameter (bndos-ws-window-of "*magit:") 'window-side) 'left))
    (should (eq (window-parameter (get-buffer-window "*Help*") 'window-side) 'top))
    (should (eq (window-parameter (get-buffer-window "*compilation*") 'window-side) 'top))
    (select-window (bndos-ws-window-of "*magit:"))
    (+workspace/new "from-magit")
    (should (bndos-ws-blank-p))
    (describe-function 'cdr)
    (+workspace-switch +workspaces-main)
    (should (equal (bndos-ws-layout) before))
    (+workspace-switch "from-magit")
    (should (get-buffer-window "*Help*"))
    (bndos-ws-keys "SPC TAB d")
    (should (equal (+workspace-current-name) +workspaces-main))
    (should (equal (bndos-ws-layout) before))))

;;; Removing

(bndos-ws-test bndos-ws-killing-workspaces-leaves-the-others-whole
  (let (recorded)
    (dolist (name '("ka" "kb" "kc"))
      (+workspace/new name)
      (switch-to-buffer (bndos-ws-file (concat name ".txt")))
      (bndos-ws-side "*magit: repo")
      (push (cons name (bndos-ws-layout)) recorded))
    (switch-to-buffer (bndos-ws-file "shared.txt"))
    (setf (alist-get "kc" recorded nil nil #'equal) (bndos-ws-layout))
    (+workspace-switch "kb")
    (select-window (split-window-right))
    (switch-to-buffer (bndos-ws-file "shared.txt"))
    (+workspace-switch "ka")
    (+workspace-kill "kb")
    (should-not (+workspace-exists-p "kb"))
    (should (equal (+workspace-current-name) "ka"))
    (should (equal (bndos-ws-layout) (cdr (assoc "ka" recorded))))
    (should (get-buffer "shared.txt"))   ; kc shows it too
    (+workspace-switch "kc")
    (should (equal (bndos-ws-layout) (cdr (assoc "kc" recorded))))
    (bndos-ws-keys "SPC TAB d")
    (should-not (+workspace-exists-p "kc"))
    (should (+workspace-exists-p (+workspace-current-name)))
    (+workspace/new "kb")
    (should (bndos-ws-blank-p))
    (+workspace-switch +workspaces-main)
    (bndos-ws-keys "SPC TAB d")
    (should (member (+workspace-current-name) (+workspace-list-names)))
    (dolist (n (+workspace-list-names)) (+workspace-switch n) (bndos-ws-keys "SPC TAB d"))
    (should (= (length (+workspace-list-names)) 1))
    (should (member (+workspace-current-name) (+workspace-list-names)))))

;;; The workspace prompt

(bndos-ws-test bndos-ws-prompt-never-changes-a-workspace-it-doesnt-switch-to
  (+workspace/new "alpha")
  (switch-to-buffer (bndos-ws-file "alpha.txt"))
  (split-window-right)
  (bndos-ws-side "*vterm*")
  (let ((alpha (bndos-ws-layout)))
    (+workspace/new "beta")
    (switch-to-buffer (bndos-ws-file "beta.txt"))
    (let ((beta (bndos-ws-layout)))
      (dolist (keys '("SPC TAB . beta C-SPC C-g" "SPC TAB . beta C-SPC RET" "SPC TAB . beta C-M-SPC RET"))
        (+workspace-switch "alpha")
        (ignore-errors (bndos-ws-keys keys))
        (should (equal (cons keys (+workspace-list-names)) (list keys +workspaces-main "alpha" "beta")))
        (+workspace-switch "alpha")
        (should (equal (cons keys (bndos-ws-layout)) (cons keys alpha)))
        (+workspace-switch "beta")
        (should (equal (cons keys (bndos-ws-layout)) (cons keys beta))))
      (+workspace-switch "alpha")
      (bndos-ws-keys "SPC TAB . beta RET")
      (should (equal (+workspace-current-name) "beta")))))

(bndos-ws-test bndos-ws-preview-only-shows
  (bndos-ws-file "preview-me.txt")
  (kill-buffer "preview-me.txt")
  (let ((default-directory bndos-ws-dir))
    (ignore-errors (bndos-ws-keys "SPC . preview-me.txt C-SPC C-g")))
  (should (get-buffer "preview-me.txt"))
  (let ((ran nil))
    (defalias 'bndos-ws-probe-command (lambda () (interactive) (setq ran t)))
    (ignore-errors (bndos-ws-keys "M-x bndos-ws-probe-command C-SPC C-g"))
    (should-not ran)))

;;; Saving and loading

(bndos-ws-test bndos-ws-save-the-current-workspace-as-it-is
  (+workspace/new "saved")
  (switch-to-buffer (bndos-ws-file "saved-a.txt"))
  (select-window (split-window-right))
  (switch-to-buffer (bndos-ws-file "saved-b.txt"))
  (let ((before (mapcar #'car (bndos-ws-layout))))
    (+workspace-save "saved")            ; while it is the current one
    (+workspace-switch +workspaces-main)
    (+workspace-kill "saved")
    (+workspace-load "saved")
    (+workspace-switch "saved")
    (should (equal (mapcar #'car (bndos-ws-layout)) before))))

(defun bndos-ws-run (file selector)
  "Run the tests SELECTOR names, ERT's batch report into FILE; return the unexpected count."
  (let* (lines
         (stats (cl-letf (((symbol-function 'message)
                           (lambda (format &rest args)
                             (when format
                               (car (push (apply #'format-message format args) lines))))))
                  (ert-run-tests-batch selector))))
    (with-temp-file file (insert (string-join (nreverse lines) "\n") "\n"))
    (ert-stats-completed-unexpected stats)))

(provide 'bndos-workspaces-tests)
;;; bndos-workspaces-tests.el ends here
