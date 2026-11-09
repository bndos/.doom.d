;;; bndos-proc.el --- Asynchronous subprocess helper -*- lexical-binding: t; -*-

;;; Commentary:

;; One way to run external programs for the tasks module:
;; argv lists (never a shell), an explicit working directory, stdout and
;; stderr captured separately, the exit code reported, and cancellation.
;; The callback always runs exactly once, asynchronously, with a result plist:
;;
;;   (:exit N :stdout STR :stderr STR :program P :args ARGS :directory DIR
;;    :cancelled BOOL :error MSG)
;;
;; :error is set when the program could not be started at all (missing
;; executable or directory); :exit is then nil.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar bndos-proc-debug nil
  "When non-nil, log every command and its result to *bndos-proc*.")

(defun bndos-proc--log (fmt &rest args)
  (when bndos-proc-debug
    (with-current-buffer (get-buffer-create "*bndos-proc*")
      (goto-char (point-max))
      (insert (format-time-string "[%T] ") (apply #'format fmt args) "\n"))))

(defun bndos-proc--deliver (callback result)
  "Call CALLBACK with RESULT, reporting (not propagating) its errors."
  (bndos-proc--log "%s %S -> exit %S%s" (plist-get result :program)
                   (plist-get result :args) (plist-get result :exit)
                   (if-let* ((err (plist-get result :error))) (format " (%s)" err) ""))
  (condition-case err
      (funcall callback result)
    (error (message "bndos-proc: callback for %s failed: %s"
                    (plist-get result :program) (error-message-string err)))))

(defun bndos-proc--buffer-string (buffer)
  (if (buffer-live-p buffer)
      (with-current-buffer buffer (buffer-string))
    ""))

(cl-defun bndos-proc-run (program args &key directory env stdin callback name)
  "Run PROGRAM with argument list ARGS asynchronously in DIRECTORY.
ENV is a list of \"NAME=VALUE\" strings prepended to `process-environment'.
STDIN, a string, is written to the process's standard input.  CALLBACK
receives the result plist described in the Commentary.  NAME labels the
process.  Return the process (for `bndos-proc-cancel'), or nil when the
program could not be started."
  (let* ((directory (file-name-as-directory (expand-file-name (or directory default-directory))))
         (base (list :program program :args args :directory directory))
         (callback (or callback #'ignore)))
    (if (not (file-directory-p directory))
        (progn (run-at-time 0 nil #'bndos-proc--deliver callback
                            (append (list :exit nil :stdout "" :stderr ""
                                          :error (format "No such directory: %s" directory))
                                    base))
               nil)
      (let* ((default-directory directory)
             (process-environment (append env process-environment))
             (stdout (generate-new-buffer (format " *bndos-proc %s*" program) t))
             (stderr (generate-new-buffer (format " *bndos-proc %s stderr*" program) t))
             (pending 2)
             (delivered nil)
             (fallback nil)
             proc errproc)
        (cl-labels
            ((finish ()
               ;; Runs once, after both the process and its stderr pipe are
               ;; done (or the fallback below fires).  Never re-enters.
               (unless delivered
                 (setq delivered t)
                 (when (timerp fallback) (cancel-timer fallback))
                 (let ((result (append
                                (list :exit (process-exit-status proc)
                                      :stdout (bndos-proc--buffer-string stdout)
                                      :stderr (bndos-proc--buffer-string stderr)
                                      :cancelled (process-get proc 'bndos-cancelled))
                                base)))
                   (when (process-live-p errproc) (delete-process errproc))
                   (kill-buffer stdout)
                   (kill-buffer stderr)
                   (bndos-proc--deliver callback result))))
             (part-done ()
               (when (zerop (cl-decf pending))
                 (finish))))
          (condition-case err
              (progn
                (setq errproc (make-pipe-process
                               :name (format "%s stderr" (or name program))
                               :buffer stderr :noquery t :coding 'utf-8-unix
                               :sentinel (lambda (p _event)
                                           (unless (process-live-p p) (part-done)))))
                (setq proc
                      (make-process
                       :name (or name program)
                       :command (cons program args)
                       :buffer stdout
                       :stderr errproc
                       :connection-type 'pipe
                       :noquery t
                       :coding 'utf-8-unix
                       :sentinel
                       (lambda (process _event)
                         (unless (process-live-p process)
                           (part-done)
                           ;; A grandchild can keep stderr open; don't wait on it forever.
                           (unless delivered
                             (setq fallback (run-at-time 2 nil #'finish)))))))
                ;; A fast child may already have exited; that's not a failure.
                (when (process-live-p proc)
                  (condition-case nil
                      (progn
                        (when stdin (process-send-string proc stdin))
                        (process-send-eof proc))
                    (error nil)))
                proc)
            (error
             (when (process-live-p errproc) (delete-process errproc))
             (kill-buffer stdout)
             (kill-buffer stderr)
             (run-at-time 0 nil #'bndos-proc--deliver callback
                          (append (list :exit nil :stdout "" :stderr ""
                                        :error (error-message-string err))
                                  base))
             nil)))))))

(defun bndos-proc-cancel (process)
  "Cancel PROCESS started by `bndos-proc-run'.  Its callback sees :cancelled t."
  (when (and (processp process) (process-live-p process))
    (process-put process 'bndos-cancelled t)
    (delete-process process)))

(defun bndos-proc-ok-p (result)
  "Non-nil when RESULT is a normal, successful exit."
  (and (eql (plist-get result :exit) 0)
       (not (plist-get result :cancelled))
       (not (plist-get result :error))))

(defun bndos-proc-error-message (result)
  "Return a one-line description of why RESULT failed."
  (cond
   ((plist-get result :error))
   ((plist-get result :cancelled) "cancelled")
   (t (let ((text (string-trim (or (plist-get result :stderr) ""))))
        (format "%s exited %s%s" (plist-get result :program) (plist-get result :exit)
                (if (string-empty-p text)
                    ""
                  (concat ": " (car (last (split-string text "\n" t))))))))))

(defun bndos-proc-wait (predicate &optional timeout)
  "Process events until PREDICATE returns non-nil or TIMEOUT seconds pass.
For tests and batch use only.  Return PREDICATE's last value."
  (let ((deadline (+ (float-time) (or timeout 10)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    value))

(provide 'bndos-proc)
;;; bndos-proc.el ends here
