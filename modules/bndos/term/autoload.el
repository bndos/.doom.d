;;; bndos/term/autoload.el -*- lexical-binding: t; -*-

(defvar bndos-vterm--compile-frame nil
  "Frame that last sent Alt-Return from vterm; compile output shows there.")

(defvar-local bndos-vterm--title nil)

(defun bndos--project-root ()
  (and (not (derived-mode-p 'dired-mode))
       (projectile-project-root)))

;;;###autoload
(defun bndos/vterm-project ()
  "Open vterm in the project root, or `default-directory' outside a project."
  (interactive)
  (let ((default-directory (or (bndos--project-root) default-directory)))
    (vterm "*vterm*")))

;;;###autoload
(defun bndos/terminal-project ()
  "Open an external kitty terminal in the project root."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (call-process "kitty" nil 0 nil)))

;;;###autoload
(defun bndos/vterm-send-alt-return ()
  "Send Alt-Return to the shell; zsh turns it into `bndos/compile-from-file'."
  (interactive)
  (setq bndos-vterm--compile-frame (selected-frame))
  (vterm-send-string "\e\r"))

;;;###autoload
(defun bndos/compile-from-file (file)
  "Run the command payload in FILE through `compile'.
FILE is written by a zsh ZLE widget: the first line is the working directory,
the rest is the command."
  (interactive "fCommand payload: ")
  (let* ((payload (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
         (newline (string-search "\n" payload))
         (directory (if newline (substring payload 0 newline) default-directory))
         (command (string-trim (if newline (substring payload (1+ newline)) payload))))
    (ignore-errors (delete-file file))
    (unless (file-directory-p directory)
      (user-error "Compile directory does not exist: %s" directory))
    (when (string-empty-p command)
      (user-error "No command to compile"))
    (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
           (buffer (progn
                     (setq compile-command command)
                     (add-to-history 'compile-history command)
                     (compile command))))
      (when (frame-live-p bndos-vterm--compile-frame)
        (with-selected-frame bndos-vterm--compile-frame
          (display-buffer buffer)))
      buffer)))

;;;###autoload
(defun bndos/compile-keep-previous-buffer ()
  "Rename this project's *compilation* buffer so the next compile gets a new one."
  (interactive)
  (when-let* ((project (projectile-project-name))
              (buffer (get-buffer (format "*compilation*<%s>" project))))
    (with-current-buffer buffer
      (rename-buffer (generate-new-buffer-name
                      (format "*compilation*<%s %s>" compile-command project))))))


;;; vterm internals

;;;###autoload
(defun bndos-vterm-header-line-h ()
  (setq-local header-line-format
              '(:eval (propertize (format "  %s  " (or bndos-vterm--title (buffer-name)))
                                  'face 'mode-line-inactive))))

;;;###autoload
(defun bndos-vterm-record-title-a (title)
  "Remember TITLE for the header line instead of renaming the buffer."
  (let ((title (string-trim
                ;; Drop braille spinner prefixes emitted by agent CLIs.
                (replace-regexp-in-string (rx bos (any (#x2800 . #x28FF)) " ")
                                          "" (or title "")))))
    (unless (string-empty-p title)
      (setq bndos-vterm--title title)
      (force-mode-line-update))))

(defun bndos-vterm--ensure-server ()
  "Start a server for this Emacs if needed and return its name."
  (require 'server)
  (unless (process-live-p server-process)
    (when (server-running-p server-name)
      (setq server-name (format "emacs-vterm-%s" (emacs-pid))))
    (server-start))
  server-name)

;;;###autoload
(defun bndos-vterm-emacsclient-env-a (fn &rest args)
  "Start vterm with EMACS_SERVER_NAME/PID pointing at this Emacs."
  (let ((vterm-environment (append (list (format "EMACS_SERVER_NAME=%s" (bndos-vterm--ensure-server))
                                         (format "EMACS_SERVER_PID=%s" (emacs-pid)))
                                   vterm-environment)))
    (apply fn args)))
