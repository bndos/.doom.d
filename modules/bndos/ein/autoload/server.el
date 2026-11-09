;;; bndos/ein/autoload/server.el -*- lexical-binding: t; -*-

;;;###autoload
(defun bndos-ein-load ()
  "Load the EIN features the managed-server commands rely on."
  (dolist (feature '(ein ein-log ein-jupyter ein-notebook ein-notebooklist
                     ein-worksheet ein-cell ein-query ein-websocket))
    (require feature)))

(defun bndos-ein--default-directory ()
  "Return the current project root, or `default-directory'."
  (file-name-as-directory
   (expand-file-name (or (when-let* ((project (project-current)))
                           (project-root project))
                         default-directory))))

;;;###autoload
(defun bndos-ein-add-auth-header-a (fn url settings &rest args)
  "Send `bndos-ein-managed-token' with requests to the managed server."
  (let ((settings (apply fn url settings args)))
    (if (and bndos-ein-managed-url
             bndos-ein-managed-token
             (string-prefix-p bndos-ein-managed-url url))
        (plist-put settings :headers
                   (cons (cons "Authorization" (format "token %s" bndos-ein-managed-token))
                         (assoc-delete-all "Authorization"
                                           (copy-sequence (plist-get settings :headers)))))
      settings)))


;;; Server process

(defun bndos-ein--free-port ()
  "Return a free localhost TCP port."
  (let ((probe (make-network-process :name "bndos-ein-port-probe"
                                     :family 'ipv4 :host "127.0.0.1" :service 0
                                     :server t :noquery t)))
    (unwind-protect (process-contact probe :service)
      (delete-process probe))))

(defun bndos-ein--jupyter-command ()
  (or (executable-find ein:jupyter-server-command)
      (executable-find "jupyter")
      (user-error "Could not find jupyter in exec-path")))

(defun bndos-ein--process-live-p ()
  (when-let* ((proc (ein:jupyter-server-process)))
    (process-live-p proc)))

(defun bndos-ein--current-url ()
  (or bndos-ein-managed-url
      (ignore-errors (ein:jupyter-my-url-or-port))))

(defun bndos-ein--managed-live-p ()
  "Non-nil when EIN's live server is the one we started."
  (and bndos-ein-managed-url
       (bndos-ein--process-live-p)
       (equal (bndos-ein--current-url) bndos-ein-managed-url)))

(defun bndos-ein--kill-server-buffer ()
  (when-let* ((buffer (get-buffer *ein:jupyter-server-buffer-name*)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun bndos-ein--stop-process ()
  "Stop EIN's Jupyter process, escalating to SIGKILL after 2 seconds."
  (when-let* ((proc (ein:jupyter-server-process)))
    (let ((pid (process-id proc)))
      (ignore-errors
        (if-let* ((url (bndos-ein--current-url)))
            (ein:jupyter-server-stop nil url)
          (delete-process proc)))
      (cl-loop repeat 20
               while (bndos-ein--process-live-p)
               do (sleep-for 0.1))
      (when (bndos-ein--process-live-p)
        (ignore-errors (delete-process proc))
        (when pid (ignore-errors (signal-process pid 'kill))))))
  (bndos-ein--kill-server-buffer))

(defun bndos-ein--open-notebooklist ()
  (unless (and bndos-ein-managed-url bndos-ein-managed-token)
    (user-error "No managed EIN Jupyter server is recorded"))
  (ein:notebooklist-open* bndos-ein-managed-url nil nil
                          (lambda (buffer _url-or-port) (pop-to-buffer buffer))
                          nil))

(defun bndos-ein--read-directory (default)
  (bndos-ein-load)
  (list (read-directory-name "Notebook directory: " default default t)))


;;; Commands

;;;###autoload
(defun bndos/ein-start-jupyter (directory)
  "Start a Jupyter server for DIRECTORY on a free port and open its notebooks."
  (interactive (bndos-ein--read-directory (bndos-ein--default-directory)))
  (bndos-ein-load)
  (unless (bndos-ein--managed-live-p)
    (bndos-ein--stop-process))
  (bndos-ein--kill-server-buffer)
  (let* ((port (bndos-ein--free-port))
         (token (secure-hash 'sha256 (format "%s:%s:%s:%s"
                                             (float-time) (random) (emacs-pid) (user-uid))))
         (ein:jupyter-server-args
          (append ein:jupyter-server-args
                  (list (format "--IdentityProvider.token=%s" token)
                        (format "--NotebookApp.token=%s" token)))))
    (setq bndos-ein-managed-url (format "http://127.0.0.1:%s" port)
          bndos-ein-managed-token token)
    (ein:jupyter-server-start (bndos-ein--jupyter-command)
                              (file-name-as-directory (expand-file-name directory))
                              t nil port)
    (cl-loop repeat 40
             until (bndos-ein--process-live-p)
             do (sleep-for 0.1))
    (bndos-ein--open-notebooklist)))

;;;###autoload
(defun bndos/ein-open-server ()
  "Open the managed server's notebook list, starting a server if needed."
  (interactive)
  (bndos-ein-load)
  (if (bndos-ein--managed-live-p)
      (bndos-ein--open-notebooklist)
    (bndos/ein-start-jupyter (bndos-ein--default-directory))))

;;;###autoload
(defun bndos/ein-restart-jupyter (directory)
  "Stop any EIN Jupyter server, then start a fresh one for DIRECTORY."
  (interactive (bndos-ein--read-directory
                (or (ignore-errors (ein:jupyter-running-notebook-directory))
                    (bndos-ein--default-directory))))
  (bndos/ein-stop-jupyter)
  (bndos/ein-start-jupyter directory))

;;;###autoload
(defun bndos/ein-stop-jupyter ()
  "Stop the managed Jupyter server and forget its credentials."
  (interactive)
  (bndos-ein-load)
  (setq bndos-ein-managed-url nil
        bndos-ein-managed-token nil)
  (bndos-ein--stop-process)
  (message "Stopped EIN Jupyter server"))
