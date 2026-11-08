;;; bndos/tools/autoload.el -*- lexical-binding: t; -*-

;;; External applications

(defun bndos--linux-opener (path)
  "Return (PROGRAM . ARGS) that opens PATH with the desktop's default app."
  (cond ((executable-find "xdg-open") (list "xdg-open" path))
        ((executable-find "gio") (list "gio" "open" path))
        ((executable-find "gnome-open") (list "gnome-open" path))
        ((executable-find "kde-open5") (list "kde-open5" path))
        ((executable-find "kde-open") (list "kde-open" path))
        ((user-error "No opener found (tried xdg-open/gio/gnome-open/kde-open)"))))

;;;###autoload
(defun bndos/open-externally (path)
  "Open PATH with the OS default application."
  (interactive "fOpen externally: ")
  (pcase system-type
    ('darwin (start-process "open-file" nil "open" path))
    ('windows-nt (w32-shell-execute "open" (convert-standard-filename path)))
    ('gnu/linux (apply #'start-process "open-file" nil (bndos--linux-opener path)))
    (_ (user-error "Unsupported system-type: %s" system-type))))

;;;###autoload
(defun bndos/open-file-manager (&optional dir)
  "Open the platform file manager at DIR (or `default-directory')."
  (interactive)
  (let ((dir (file-name-as-directory (or dir default-directory))))
    (pcase system-type
      ('darwin (start-process "open-fm" nil "open" "-a" "Finder" dir))
      ('windows-nt (w32-shell-execute "explore" (convert-standard-filename dir)))
      ('gnu/linux
       (apply #'start-process "open-fm" nil
              (if-let* ((fm (seq-find #'executable-find
                                      '("nautilus" "nemo" "dolphin" "thunar"
                                        "pcmanfm" "krusader"))))
                  (list fm dir)
                (bndos--linux-opener dir))))
      (_ (user-error "Unsupported system-type: %s" system-type)))))

;;;###autoload
(defun bndos/dired-open-externally ()
  "Open the marked files in their default external applications."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (bndos/open-externally file)
    (message "Opening: %s" file)))


;;; Dired buffer-list hygiene

(defun bndos-dired--valid-entry-p (entry)
  (and (consp entry)
       (stringp (car entry))
       (buffer-live-p (cdr entry))))

;;;###autoload
(defun bndos-dired-prune-buffers-a (&rest _)
  "Drop malformed or dead entries from `dired-buffers'."
  (setq dired-buffers (seq-filter #'bndos-dired--valid-entry-p dired-buffers)))

(defun bndos-dired--register-buffer (directory buffer)
  "Record live BUFFER as the Dired buffer for DIRECTORY."
  (when (and (stringp directory) (buffer-live-p buffer))
    (let ((key (file-name-as-directory (expand-file-name directory))))
      (bndos-dired-prune-buffers-a)
      (if-let* ((entry (assoc key dired-buffers)))
          (setcdr entry buffer)
        (push (cons key buffer) dired-buffers)))))

;;;###autoload
(defun bndos-dirvish-noselect-safe-a (fn dired-noselect dir-or-list &optional flags re)
  "Recover when Dirvish fails to create a buffer.
Dirvish records the directory in `dired-buffers' before it has a live buffer.
If creation then fails, later Dired calls signal (wrong-type-argument stringp
nil).  Prune the bad entry and fall back to plain DIRED-NOSELECT."
  (let ((directory (if (consp dir-or-list) (car dir-or-list) dir-or-list)))
    (condition-case err
        (prog1 (funcall fn dired-noselect dir-or-list flags re)
          (bndos-dired-prune-buffers-a))
      (wrong-type-argument
       (bndos-dired-prune-buffers-a)
       (unless (and (equal (cdr err) '(stringp nil)) (stringp directory))
         (signal (car err) (cdr err)))
       (let ((buffer (funcall dired-noselect dir-or-list flags)))
         (bndos-dired--register-buffer directory buffer)
         buffer))
      (error
       (bndos-dired-prune-buffers-a)
       (signal (car err) (cdr err))))))
