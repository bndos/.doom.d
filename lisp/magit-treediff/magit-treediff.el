;;; magit-treediff.el --- Tree diff viewer  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2026 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.4.0"))
;; Keywords: git, tools, vc
;; URL: https://github.com/magit/magit

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library implements a tree diff viewer.  The default layout shows the
;; file tree on the left and a standard magit-diff buffer on the right, giving
;; full access to magit's staging/unstaging/visiting machinery.  The tree can
;; be rendered either by the built-in special-mode backend or by a Treemacs
;; backend.

;;; Code:

(require 'seq)
(require 'magit-diff)

(declare-function magit-find-file-noselect "magit-files"
                  (rev file &optional no-restore-position volatile))
(declare-function magit-current-section "magit-section" ())
(declare-function magit-hunk-section-p "magit-diff" (section) t)

(defconst magit-treediff--viewer-backends
  '((normal
     :render magit-treediff--render-normal
     :focus magit-treediff--focus-normal-viewer
     :cleanup magit-treediff--cleanup-normal-viewer))
  "Viewer backend dispatch table for treediff.")

(defconst magit-treediff--tree-backends
  '((builtin
     :ensure magit-treediff--ensure-builtin-tree-buffer
     :display magit-treediff--display-builtin-tree
     :sync magit-treediff--sync-builtin-tree)
    (treemacs
     :ensure magit-treediff--ensure-treemacs-tree-buffer
     :display magit-treediff--display-treemacs-tree
     :sync magit-treediff--sync-treemacs-tree))
  "Tree backend dispatch table for treediff.")

(defgroup magit-treediff nil
  "Inspect Git diffs in a tree-driven layout."
  :link '(info-link "(magit)Diffing")
  :group 'magit-diff)

(defcustom magit-treediff-mode-hook nil
  "Hook run after entering `magit-treediff-mode'."
  :group 'magit-treediff
  :type 'hook)

(defcustom magit-treediff-tree-width 32
  "Preferred width of the treediff file tree window."
  :group 'magit-treediff
  :type 'natnum)

(defcustom magit-treediff-render-row-functions nil
  "Hook run after inserting a rendered row.

Each function is called with five arguments:

1. START, the row beginning position.
2. END, the row end position.
3. ROW, a plist describing the rendered row.
4. FILE, the file model plist for the current file.
5. HUNK, the hunk model plist for the current hunk."
  :group 'magit-treediff
  :type 'hook)

(defcustom magit-treediff-enable-syntax-highlighting nil
  "Deprecated compatibility option.

Treediff no longer manages syntax highlighting."
  :group 'magit-treediff
  :type 'boolean)

(defcustom magit-treediff-tree-backend 'builtin
  "Tree backend used by treediff.

`builtin' uses the internal special-mode tree buffer.  Future backends, such as
Treemacs integration, can provide the same backend slots without changing the
controller."
  :group 'magit-treediff
  :type '(choice (const :tag "Builtin" builtin)
                 (const :tag "Treemacs" treemacs)))

(defface magit-treediff-separator
  '((t :inherit magit-dimmed))
  "Face used for column separators."
  :group 'magit-treediff)

(defface magit-treediff-tree-current
  '((t :inherit magit-section-highlight))
  "Face used for the selected file in the tree buffer."
  :group 'magit-treediff)

(defface magit-treediff-current-line
  '((t :inherit hl-line))
  "Face used to highlight the current row in treediff helper buffers."
  :group 'magit-treediff)

(defface magit-treediff-current-hunk
  '((t :inherit magit-section-highlight :weight bold))
  "Face used to highlight the current hunk in treediff helper buffers."
  :group 'magit-treediff)

(defvar-local magit-treediff-range nil)
(put 'magit-treediff-range 'permanent-local t)

(defvar-local magit-treediff-typearg nil)
(put 'magit-treediff-typearg 'permanent-local t)

(defvar-local magit-treediff-type nil)
(put 'magit-treediff-type 'permanent-local t)

(defvar-local magit-treediff-source-kind nil)
(put 'magit-treediff-source-kind 'permanent-local t)

(defvar-local magit-treediff-source-origin nil)
(put 'magit-treediff-source-origin 'permanent-local t)

(defvar-local magit-treediff-capabilities nil)
(put 'magit-treediff-capabilities 'permanent-local t)

(defvar-local magit-treediff-args nil)
(put 'magit-treediff-args 'permanent-local t)

(defvar-local magit-treediff-files nil)
(put 'magit-treediff-files 'permanent-local t)

(defvar-local magit-treediff-selected-file nil)
(put 'magit-treediff-selected-file 'permanent-local t)

(defvar-local magit-treediff--model nil)
(put 'magit-treediff--model 'permanent-local t)

(defvar-local magit-treediff--tree-buffer nil)
(put 'magit-treediff--tree-buffer 'permanent-local t)

(defvar-local magit-treediff--left-buffer nil)
(put 'magit-treediff--left-buffer 'permanent-local t)

(defvar-local magit-treediff--right-buffer nil)
(put 'magit-treediff--right-buffer 'permanent-local t)

(defvar-local magit-treediff--diff-buffer nil)
(put 'magit-treediff--diff-buffer 'permanent-local t)

(defvar-local magit-treediff--highlight-file nil
  "When non-nil, the file whose syntax highlighting is applied to this diff buffer.")
(put 'magit-treediff--highlight-file 'permanent-local t)

(defvar-local magit-treediff-parent-buffer nil)
(put 'magit-treediff-parent-buffer 'permanent-local t)

(defvar-local magit-treediff-tree-buffer-p nil)
(put 'magit-treediff-tree-buffer-p 'permanent-local t)

(defvar-local magit-treediff-side nil)
(put 'magit-treediff-side 'permanent-local t)

(defvar-local magit-treediff--override-mode nil)
(put 'magit-treediff--override-mode 'permanent-local t)

(defvar-local magit-treediff--font-lock-cache nil)
(put 'magit-treediff--font-lock-cache 'permanent-local t)

(defvar-local magit-treediff--syntax-cache nil)
(put 'magit-treediff--syntax-cache 'permanent-local t)

(defvar-local magit-treediff--fontify-buffer nil)
(put 'magit-treediff--fontify-buffer 'permanent-local t)

(defvar-local magit-treediff-previous-winconf nil)
(put 'magit-treediff-previous-winconf 'permanent-local t)

(defvar-local magit-treediff--current-line-overlay nil)
(put 'magit-treediff--current-line-overlay 'permanent-local t)

(defvar-local magit-treediff--current-hunk-overlay nil)
(put 'magit-treediff--current-hunk-overlay 'permanent-local t)

(defvar-local magit-treediff--rehighlight-timer nil)
(put 'magit-treediff--rehighlight-timer 'permanent-local t)

(defvar-local magit-treediff--rehighlight-range nil)
(put 'magit-treediff--rehighlight-range 'permanent-local t)

(defvar-local magit-treediff--last-focused-hunk nil)
(put 'magit-treediff--last-focused-hunk 'permanent-local t)

(defvar-local magit-treediff--last-visible-range nil)
(put 'magit-treediff--last-visible-range 'permanent-local t)

(defvar-local magit-treediff--syntaxify-timer nil)
(put 'magit-treediff--syntaxify-timer 'permanent-local t)

(defvar-local magit-treediff--syntaxify-range nil)
(put 'magit-treediff--syntaxify-range 'permanent-local t)

(defvar magit-treediff--inhibit-sync nil)
(defvar magit-treediff--inhibit-window-sync nil)
(defvar magit-treediff-treemacs-root nil)
(defvar magit-treediff-treemacs-node nil)

(defun magit-treediff--viewer-backend ()
  "Return the backend plist for the current treediff viewer."
  (alist-get 'normal magit-treediff--viewer-backends))

(defun magit-treediff--viewer-call (slot)
  "Call the current viewer backend function in SLOT."
  (when-let ((fn (plist-get (magit-treediff--viewer-backend) slot)))
    (funcall fn)))

(defun magit-treediff--tree-backend ()
  "Return the backend plist for `magit-treediff-tree-backend'."
  (or (alist-get magit-treediff-tree-backend
                 magit-treediff--tree-backends)
      (alist-get 'builtin magit-treediff--tree-backends)))

(defun magit-treediff--tree-call (slot &rest args)
  "Call the current tree backend function in SLOT with ARGS."
  (when-let ((fn (plist-get (magit-treediff--tree-backend) slot)))
    (apply fn args)))

(defun magit-treediff--magit-diff-dwim ()
  "Compatibility wrapper around Magit's diff target resolution."
  (magit-diff--dwim))

(defun magit-treediff--section-entry (section slot)
  "Return SECTION's SLOT if it is bound, else nil."
  (and section
       (slot-boundp section slot)
       (slot-value section slot)))

(defun magit-treediff--section-start (section)
  "Return SECTION's start position."
  (magit-treediff--section-entry section 'start))

(defun magit-treediff--section-content (section)
  "Return SECTION's content start position."
  (magit-treediff--section-entry section 'content))

(defun magit-treediff--section-end (section)
  "Return SECTION's end position."
  (magit-treediff--section-entry section 'end))

(defun magit-treediff--section-for-file (file)
  "Return the Magit section for FILE in the current diff buffer."
  (magit-get-section `((file . ,file) (diffbuf))))

(defun magit-treediff--controller-buffer ()
  (cond
   ((derived-mode-p 'magit-treediff-mode)
    (current-buffer))
   ((buffer-live-p magit-treediff-parent-buffer)
    magit-treediff-parent-buffer)
   ((buffer-live-p (magit-treediff--existing-viewer))
    (magit-treediff--existing-viewer))
   (t
    nil)))

(defun magit-treediff--prepare-internal-buffer (buffer)
  (with-current-buffer buffer
    (setq-local buffer-offer-save nil)
    (setq-local buffer-file-name nil)
    (set-buffer-modified-p nil))
  buffer)

(defun magit-treediff--syntax-scope ()
  "Return the current syntax-highlighting scope for this diff buffer."
  (or (bound-and-true-p magit-treediff--highlight-file)
      :dynamic))

(defun magit-treediff--syntax-buffer-p ()
  "Return non-nil if the current buffer supports diff syntax mode."
  (or (derived-mode-p 'magit-diff-mode)
      (derived-mode-p 'magit-status-mode)))

(defun magit-treediff--valid-auxiliary-buffer-p (buffer)
  "Return non-nil if BUFFER is a tree/pane buffer tied to a live controller."
  (with-current-buffer buffer
    (and (or magit-treediff-tree-buffer-p
             (derived-mode-p 'magit-treediff-pane-mode))
         (buffer-live-p magit-treediff-parent-buffer)
         (with-current-buffer magit-treediff-parent-buffer
           (derived-mode-p 'magit-treediff-mode)))))

(defun magit-treediff--cleanup-invalid-auxiliary-buffers ()
  "Delete windows and kill invalid treediff helper buffers."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (or magit-treediff-tree-buffer-p
                     (derived-mode-p 'magit-treediff-pane-mode)))
               (not (magit-treediff--valid-auxiliary-buffer-p buffer)))
      (dolist (window (get-buffer-window-list buffer nil t))
        (set-window-dedicated-p window nil)
        (delete-window window))
      (kill-buffer buffer))))

(defun magit-treediff--format-header ()
  (concat
   (pcase magit-treediff-source-kind
     ('staged "Tree diff: staged")
     ('unstaged "Tree diff: unstaged")
     ('range (format "Tree diff: %s" magit-treediff-range))
     (_ "Tree diff"))
   (pcase magit-treediff-source-origin
     ('diff-buffer " (from diff)")
     ('status-buffer " (from status)")
     (_ ""))
   (pcase (length magit-treediff-files)
     (0 "")
     (1 (format " – %s" (car magit-treediff-files)))
     (_ (format " – %d files" (length magit-treediff-files))))))

(defun magit-treediff--source-origin ()
  (cond
   ((derived-mode-p 'magit-diff-mode) 'diff-buffer)
   ((derived-mode-p 'magit-status-mode) 'status-buffer)
   (t 'command)))

(defun magit-treediff--source-kind (range typearg type)
  (cond
   ((eq type 'unstaged) 'unstaged)
   ((eq type 'staged) 'staged)
   ((equal typearg "--cached") 'staged)
   (range 'range)
   (t 'range)))

(defun magit-treediff--capabilities-for-source (source-kind)
  (pcase source-kind
    ('unstaged '(:stage t :unstage nil :discard t))
    ('staged '(:stage nil :unstage t :discard t))
    (_ '(:stage nil :unstage nil :discard nil))))

(defun magit-treediff--effective-source-kind ()
  (or magit-treediff-source-kind
      (magit-treediff--source-kind
       magit-treediff-range
       magit-treediff-typearg
       magit-treediff-type)))

(defun magit-treediff--capable-p (capability)
  (plist-get
   (or magit-treediff-capabilities
       (magit-treediff--capabilities-for-source
        (magit-treediff--effective-source-kind)))
   capability))

(defun magit-treediff--git-args (&optional file)
  (flatten-tree
   (list "diff"
         magit-treediff-range
         "-p"
         "--no-ext-diff"
         "--no-prefix"
         magit-treediff-typearg
         magit-treediff-args
         "--"
         (or file magit-treediff-files))))

(defun magit-treediff--collect-diff (&optional file)
  (let ((args (magit-treediff--git-args file)))
    (with-temp-buffer
      (apply #'magit-git-insert args)
      (buffer-string))))

(defun magit-treediff--parse-hunk-header (line)
  (when (string-match
         "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@\\(?: \\(.*\\)\\)?$"
         line)
    (list :header line
          :from (list (string-to-number (match-string 1 line))
                      (string-to-number (or (match-string 2 line) "1")))
          :to (list (string-to-number (match-string 3 line))
                    (string-to-number (or (match-string 4 line) "1")))
          :about (match-string 5 line))))

(defun magit-treediff--parse-row-groups (lines from-line to-line)
  (let (rows)
    (while lines
      (if (string-empty-p (car lines))
          (pop lines)
      (pcase (aref (car lines) 0)
        (?\s
         (let ((text (substring (pop lines) 1)))
           (push (list :type 'context
                       :left-line from-line
                       :right-line to-line
                       :left-text text
                       :right-text text)
                 rows)
           (cl-incf from-line)
           (cl-incf to-line)))
        (?-
         (let (left-group right-group)
           (while (and lines (not (string-empty-p (car lines))) (eq (aref (car lines) 0) ?-))
             (push (substring (pop lines) 1) left-group))
           (while (and lines (not (string-empty-p (car lines))) (eq (aref (car lines) 0) ?+))
             (push (substring (pop lines) 1) right-group))
           (setq left-group (nreverse left-group)
                 right-group (nreverse right-group))
           (dotimes (i (max (length left-group) (length right-group)))
             (let ((left (nth i left-group))
                   (right (nth i right-group)))
               (push (list :type (cond ((and left right) 'changed)
                                       (left 'removed)
                                       (t 'added))
                           :left-line (and left from-line)
                           :right-line (and right to-line)
                           :left-text left
                           :right-text right)
                     rows)
               (when left
                 (cl-incf from-line))
               (when right
                 (cl-incf to-line))))))
        (?+
         (while (and lines (not (string-empty-p (car lines))) (eq (aref (car lines) 0) ?+))
           (let ((text (substring (pop lines) 1)))
             (push (list :type 'added
                         :left-line nil
                         :right-line to-line
                         :left-text nil
                         :right-text text)
                   rows)
             (cl-incf to-line))))
        (?\\
         (setq lines (cdr lines)))
        (_
         (setq lines (cdr lines))))))
    (nreverse rows)))

(defun magit-treediff--parse-file (lines)
  (let ((file (list :status "modified"
                    :source nil
                    :binary nil
                    :hunks nil))
        (hunks nil))
    (while (and lines (not (string-prefix-p "diff --git " (car lines))))
      (let ((line (pop lines)))
        (cond
         ((string-match "^rename from \\(.+\\)$" line)
          (plist-put file :source (match-string 1 line))
          (plist-put file :status "renamed"))
         ((string-match "^rename to \\(.+\\)$" line)
          (plist-put file :file (match-string 1 line)))
         ((string-match "^new file " line)
          (plist-put file :status "new file"))
         ((string-match "^deleted file " line)
          (plist-put file :status "deleted"))
         ((string-match "^--- \\(.+?\\)\t?$" line)
          (unless (equal (match-string 1 line) "/dev/null")
            (plist-put file :source (match-string 1 line))))
         ((string-match "^\\+\\+\\+ \\(.+?\\)\t?$" line)
          (unless (equal (match-string 1 line) "/dev/null")
            (plist-put file :file (match-string 1 line))))
         ((or (string-match "^Binary files " line)
              (string-match "^Binary files differ" line))
          (plist-put file :binary t))
         ((string-prefix-p "@@ " line)
          (let* ((header (magit-treediff--parse-hunk-header line))
                 (body nil)
                 (from-line (car (plist-get header :from)))
                 (to-line (car (plist-get header :to))))
            (while (and lines
                        (not (string-prefix-p "diff --git " (car lines)))
                        (not (string-prefix-p "@@ " (car lines))))
              (push (pop lines) body))
            (setq body (nreverse body))
            (push (list :header (plist-get header :header)
                        :about (plist-get header :about)
                        :from (plist-get header :from)
                        :to (plist-get header :to)
                        :rows (magit-treediff--parse-row-groups
                               body from-line to-line))
                  hunks))))))
    (plist-put file :hunks (nreverse hunks))
    (unless (plist-get file :file)
      (plist-put file :file (or (plist-get file :source) "unknown")))
    (list file lines)))

(defun magit-treediff--parse-diff (text)
  (let ((lines (split-string text "\n"))
        files)
    (while lines
      (let ((line (pop lines)))
        (when (string-prefix-p "diff --git " line)
          (pcase-let ((`(,file ,rest) (magit-treediff--parse-file lines)))
            (push file files)
            (setq lines rest)))))
    (nreverse files)))

(defun magit-treediff--model ()
  (magit-treediff--parse-diff (magit-treediff--collect-diff)))

(defun magit-treediff--select-default-file ()
  (or (and magit-treediff-selected-file
           (magit-treediff--file-model magit-treediff-selected-file)
           magit-treediff-selected-file)
      (car magit-treediff-files)
      (plist-get (car magit-treediff--model) :file)))

(defun magit-treediff--file-model (file)
  (seq-find (lambda (it)
              (equal (plist-get it :file) file))
            magit-treediff--model))

(defun magit-treediff--line-width (file)
  (let ((max-line 0))
    (dolist (hunk (plist-get file :hunks))
      (dolist (row (plist-get hunk :rows))
        (setq max-line (max max-line
                            (or (plist-get row :left-line) 0)
                            (or (plist-get row :right-line) 0)))))
    (max 2 (length (number-to-string max-line)))))

(defun magit-treediff--text-width (file)
  (or (plist-get file :text-width)
      (let ((width 8))
        (dolist (hunk (plist-get file :hunks))
          (dolist (row (plist-get hunk :rows))
            (setq width (max width
                             (string-width (or (plist-get row :left-text) ""))
                             (string-width (or (plist-get row :right-text) ""))))))
        (plist-put file :text-width width)
        width)))

(defun magit-treediff--get-fontify-buffer ()
  (let* ((controller (magit-treediff--controller-buffer))
         (buffer (or (and (buffer-live-p magit-treediff--fontify-buffer)
                          magit-treediff--fontify-buffer)
                     (with-current-buffer controller
                       (or (and (buffer-live-p magit-treediff--fontify-buffer)
                                magit-treediff--fontify-buffer)
                           (setq magit-treediff--fontify-buffer
                                 (get-buffer-create
                                  (format " *Magit Treediff Fontify: %s*"
                                          (buffer-name controller)))))))))
    (with-current-buffer buffer
      (setq magit-treediff-parent-buffer controller)
      (magit-treediff--prepare-internal-buffer buffer))
    buffer))

(defun magit-treediff--font-lock-mode (file)
  (or (alist-get file magit-treediff--font-lock-cache nil nil #'equal)
      (let ((mode
             (with-current-buffer (magit-treediff--get-fontify-buffer)
               (let ((inhibit-read-only t))
                 (erase-buffer))
               (let ((buffer-file-name
                      (expand-file-name file (magit-toplevel))))
                 (delay-mode-hooks
                   (set-auto-mode)))
               (set-buffer-modified-p nil)
               major-mode)))
        (push (cons file mode) magit-treediff--font-lock-cache)
        mode)))

(defun magit-treediff--merge-face (face base-face)
  (cond ((and face base-face) (list face base-face))
        (face face)
        (base-face base-face)
        (t 'default)))

(defun magit-treediff--syntaxify-snippet (text file)
  (let ((text (or text "")))
    (if (string-empty-p text)
        ""
      (or (alist-get (cons file text) magit-treediff--syntax-cache nil nil #'equal)
          (let ((result
                 (with-current-buffer (magit-treediff--get-fontify-buffer)
                   (let ((inhibit-read-only t))
                     (erase-buffer))
                   (insert text)
                   (let ((buffer-file-name (expand-file-name file (magit-toplevel))))
                     (delay-mode-hooks
                       (funcall (magit-treediff--font-lock-mode file))))
                   (font-lock-ensure)
                   (let ((snippet (copy-sequence (buffer-string))))
                     (dotimes (i (length snippet))
                       (let* ((pos (1+ i))
                              (face (or (get-text-property pos 'face)
                                        (get-text-property pos 'font-lock-face))))
                         (when (and face (not (eq face 'default)))
                           (put-text-property i (1+ i) 'face face snippet))))
                     (set-buffer-modified-p nil)
                     snippet))))
            (push (cons (cons file text) result) magit-treediff--syntax-cache)
            result)))))

(defun magit-treediff--fontify-snippet (text file base-face)
  (let* ((syntaxified (magit-treediff--syntaxify-snippet text file))
         (result (copy-sequence syntaxified)))
    (dotimes (i (length result))
      (let ((face (get-text-property i 'face result)))
        (put-text-property i (1+ i) 'face
                           (magit-treediff--merge-face face base-face)
                           result)))
    result))

(defun magit-treediff--cell (line text file num-width text-width face)
  (let* ((num (if line
                  (format (format "%%%dd " num-width) line)
                (make-string (1+ num-width) ? )))
         (body (magit-treediff--fontify-snippet
                text file face))
         (padding (make-string (max 0 (- text-width (string-width (or text "")))) ? )))
    (concat (propertize num 'face (magit-treediff--merge-face 'magit-dimmed face))
            body
            (propertize padding 'face (or face 'default)))))

(defun magit-treediff--line-number (line width &optional face)
  (let ((text (if line
                  (format (format "%%%dd " width) line)
                (make-string (1+ width) ? ))))
    (propertize text 'face (or face 'magit-dimmed))))

(defun magit-treediff--row-face (type side)
  ;; Use the -highlight variants: Doom and many themes only give visible colors
  ;; to these (the faces used when the cursor is inside a diff section).
  (cond
   ((and (memq type '(added changed)) (eq side 'right)) 'magit-diff-added-highlight)
   ((and (memq type '(removed changed)) (eq side 'left)) 'magit-diff-removed-highlight)
   (t nil)))

(defun magit-treediff--insert-row (file hunk row)
  (let* ((num-width (magit-treediff--line-width file))
         (text-width (magit-treediff--text-width file))
         (type (plist-get row :type))
         (bol (point))
         (left-face (magit-treediff--row-face type 'left))
         (right-face (magit-treediff--row-face type 'right))
         (left-cell (magit-treediff--cell
                     (plist-get row :left-line)
                     (plist-get row :left-text)
                     (plist-get file :file)
                     num-width text-width left-face))
         (right-cell (magit-treediff--cell
                      (plist-get row :right-line)
                      (plist-get row :right-text)
                      (plist-get file :file)
                      num-width text-width right-face))
         (sep (propertize " | " 'face 'magit-treediff-separator)))
    (insert left-cell)
    (insert sep)
    (insert right-cell)
    (insert "\n")
    (add-text-properties
     bol (point)
     `(magit-treediff-row ,row
       magit-treediff-file ,(plist-get file :file)
       magit-treediff-hunk ,hunk))
    (run-hook-with-args 'magit-treediff-render-row-functions
                        bol (point) row file hunk)))

(defun magit-treediff--insert-file (file)
  ;; Use sbs-file / sbs-hunk types (not 'file / 'hunk) so that magit-section-paint
  ;; (which dispatches on magit-hunk-section and magit-file-section) does not run on
  ;; our content and overwrite our face text properties with magit-diff-context-highlight.
  (magit-insert-section (sbs-file (plist-get file :file))
    (magit-insert-heading
      (magit-format-file 'diff
                         (plist-get file :file)
                         'magit-diff-file-heading
                         (plist-get file :status)
                         (plist-get file :source)))
    (if (plist-get file :binary)
        (insert (propertize "Binary files differ\n" 'face 'magit-dimmed))
      (dolist (hunk (plist-get file :hunks))
        (magit-insert-section (sbs-hunk hunk)
          (magit-insert-heading
            (propertize (plist-get hunk :header)
                        'face 'magit-diff-hunk-heading))
          (dolist (row (plist-get hunk :rows))
            (magit-treediff--insert-row file hunk row)))))
    (insert "\n")))

(defun magit-treediff--display-name (buffer)
  "Return the base display name for treediff BUFFER."
  (let ((name (buffer-name buffer)))
    (cond
     ((string-prefix-p "Magit Treediff: " name)
      (string-remove-prefix "Magit Treediff: " name))
     ((string-prefix-p "Magit Treediff: " name)
      (string-remove-prefix "Magit Treediff: " name))
     (t
      name))))

(defun magit-treediff--tree-buffer-name (buffer)
  (format "Magit Treediff Tree: %s" (magit-treediff--display-name buffer)))

(defun magit-treediff--pane-buffer-name (buffer side)
  (format "Magit Treediff %s: %s"
          (capitalize (symbol-name side))
          (magit-treediff--display-name buffer)))

(defun magit-treediff--build-tree (file-paths)
  "Build a directory tree from FILE-PATHS.
Returns a list of nodes, each a plist with :type (dir or file),
:name, :path (files only), :children (dirs only)."
  (let ((root nil))
    (dolist (path (sort (copy-sequence file-paths) #'string<))
      (setq root (magit-treediff--tree-insert
                  root (split-string path "/" t) path)))
    root))

(defun magit-treediff--tree-insert (nodes parts full-path)
  (if (null (cdr parts))
      (append nodes (list (list :type 'file :name (car parts) :path full-path)))
    (let* ((dir-name (car parts))
           (found nil)
           (new-nodes
            (mapcar (lambda (node)
                      (if (and (eq (plist-get node :type) 'dir)
                               (equal (plist-get node :name) dir-name))
                          (progn (setq found t)
                                 (list :type 'dir :name dir-name
                                       :children (magit-treediff--tree-insert
                                                  (plist-get node :children)
                                                  (cdr parts) full-path)))
                        node))
                    nodes)))
      (if found
          new-nodes
        (append nodes (list (list :type 'dir :name dir-name
                                  :children (magit-treediff--tree-insert
                                             nil (cdr parts) full-path))))))))

(defun magit-treediff--render-tree-nodes (nodes depth selected)
  (dolist (node nodes)
    (let ((type (plist-get node :type)))
      (cond
       ((eq type 'dir)
        (insert (make-string (* depth 2) ?\s)
                (propertize (concat (plist-get node :name) "/")
                            'face 'magit-section-heading)
                "\n")
        (magit-treediff--render-tree-nodes
         (plist-get node :children) (1+ depth) selected))
       ((eq type 'file)
        (let* ((path (plist-get node :path))
               (name (plist-get node :name))
               (is-selected (equal path selected))
               (indent (make-string (* depth 2) ?\s))
               (start (point))
               (map (make-sparse-keymap)))
          (define-key map (kbd "RET") #'magit-treediff-tree-visit-file)
          (insert indent (if is-selected "> " "  ")
                  (propertize name
                              'mouse-face 'highlight
                              'face (if is-selected
                                        'magit-treediff-tree-current
                                      'default)
                              'magit-treediff-file path
                              'keymap map)
                  "\n")
          (add-text-properties start (point)
                               `(magit-treediff-file ,path))))))))

(defun magit-treediff--goto-selected-tree-file ()
  "Move point to the selected file in the current tree buffer."
  (let* ((parent magit-treediff-parent-buffer)
         (selected (and (buffer-live-p parent)
                        (with-current-buffer parent
                          magit-treediff-selected-file))))
    (goto-char (point-min))
    (if selected
        (let (found)
          (while (and (not found) (not (eobp)))
            (when (equal (magit-treediff--tree-file-at-point) selected)
              (setq found (point)))
            (unless found
              (forward-line 1)
              (beginning-of-line)))
          (if found
              (goto-char found)
            (goto-char (point-min))))
      (goto-char (point-min)))
    (setq magit-treediff-tree--last-file selected)))

(defun magit-treediff--tree-buffer-common-setup (controller)
  "Apply tree-buffer local state shared by all tree backends."
  (magit-treediff--prepare-internal-buffer (current-buffer))
  (setq-local magit-treediff-parent-buffer controller)
  (setq-local magit-treediff-tree-buffer-p t)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (setq-local window-selection-change-functions nil)
  ;; Treemacs already provides its own current-node focus cue.  Leaving
  ;; `hl-line-mode' active in the tree buffer creates a second, offset-looking
  ;; highlight that makes the selected line hard to track.
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1))
  (magit-treediff--install-override-map
   magit-treediff-tree-mode-override-map))

(defun magit-treediff--tree-follow-point ()
  "Update the diff viewer from the current tree point."
  (when-let ((file (magit-treediff--tree-file-at-point)))
    (unless (equal file magit-treediff-tree--last-file)
      (setq magit-treediff-tree--last-file file)
      (when (buffer-live-p magit-treediff-parent-buffer)
        (with-current-buffer magit-treediff-parent-buffer
          (magit-treediff--select-file file))))))

(defun magit-treediff--file-change-summary (file)
  "Return a short UI summary for FILE."
  (let ((added 0)
        (removed 0))
    (dolist (hunk (plist-get file :hunks))
      (dolist (row (plist-get hunk :rows))
        (pcase (plist-get row :type)
          ('added
           (cl-incf added))
          ('removed
           (cl-incf removed))
          ('changed
           (cl-incf added)
           (cl-incf removed)))))
    (list :status (or (plist-get file :status) "modified")
          :hunks (length (plist-get file :hunks))
          :added added
          :removed removed)))

(defun magit-treediff--file-status-token (file)
  "Return a short status token for FILE."
  (pcase (plist-get file :status)
    ("new file" "A")
    ("deleted" "D")
    ("renamed" "R")
    (_ "M")))

(defun magit-treediff--builtin-file-label (file selected)
  "Return the built-in tree label for FILE.
If SELECTED is non-nil, use the selected face."
  (let* ((summary (magit-treediff--file-change-summary file))
         (name (file-name-nondirectory (plist-get file :file)))
         (status (magit-treediff--file-status-token file))
         (face (if selected
                   'magit-treediff-tree-current
                 'default)))
    (concat
     (propertize name 'face face)
     (propertize
      (format "  [%s] %dh +%d -%d"
              status
              (plist-get summary :hunks)
              (plist-get summary :added)
              (plist-get summary :removed))
      'face 'magit-dimmed))))

(defun magit-treediff--render-builtin-tree-nodes (nodes depth selected file-table)
  "Render built-in tree NODES at DEPTH.
SELECTED is the selected file path.  FILE-TABLE maps paths to file models."
  (dolist (node nodes)
    (pcase (plist-get node :type)
      ('dir
       (insert (make-string (* depth 2) ?\s)
               (propertize (concat (plist-get node :name) "/")
                           'face 'magit-section-heading)
               "\n")
       (magit-treediff--render-builtin-tree-nodes
        (plist-get node :children) (1+ depth) selected file-table))
      ('file
       (let* ((path (plist-get node :path))
              (file (alist-get path file-table nil nil #'equal))
              (is-selected (equal path selected))
              (indent (make-string (* depth 2) ?\s))
              (start (point))
              (map (make-sparse-keymap)))
         (define-key map (kbd "RET") #'magit-treediff-tree-visit-file)
         (insert indent (if is-selected "> " "  "))
         (insert (propertize
                  (magit-treediff--builtin-file-label file is-selected)
                  'mouse-face 'highlight
                  'magit-treediff-file path
                  'keymap map))
         (insert "\n")
         (add-text-properties start (point)
                              `(magit-treediff-file ,path)))))))

(defun magit-treediff--render-tree ()
  (let* ((inhibit-read-only t)
         (parent magit-treediff-parent-buffer)
         (selected (with-current-buffer parent magit-treediff-selected-file))
         (files (with-current-buffer parent magit-treediff--model))
         (file-paths (mapcar (lambda (f) (plist-get f :file)) files))
         (tree (magit-treediff--build-tree file-paths))
         (file-table (mapcar (lambda (f)
                               (cons (plist-get f :file) f))
                             files)))
    (erase-buffer)
    (magit-treediff--render-builtin-tree-nodes tree 0 selected file-table)
    (magit-treediff--goto-selected-tree-file)
    (set-buffer-modified-p nil)))

(defun magit-treediff--line-pos (buffer line)
  "Return position of LINE in BUFFER, clamped to the accessible range."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (max 0 (1- line)))
      (point))))

(defun magit-treediff--existing-viewer ()
  "Return an existing treediff controller for the current repository."
  (car (magit-treediff--viewer-buffers)))

(defvar magit-treediff--treemacs-defined nil)

(defun magit-treediff--treemacs-available-p ()
  "Return non-nil when Treemacs can be loaded."
  (and (require 'treemacs nil t)
       (require 'treemacs-treelib nil t)))

(defun magit-treediff--treemacs-file-children ()
  "Return top-level diff tree nodes for the current Treemacs tree buffer."
  (if-let ((controller (and (buffer-live-p magit-treediff-parent-buffer)
                            magit-treediff-parent-buffer)))
      (with-current-buffer controller
        (magit-treediff--build-tree
         (mapcar (lambda (f) (plist-get f :file))
                 magit-treediff--model)))
    nil))

(defun magit-treediff--treemacs-node-label (item)
  "Return the display label for Treemacs ITEM."
  (if (eq (plist-get item :type) 'dir)
      (propertize (concat (plist-get item :name) "/")
                  'face 'treemacs-directory-face)
    (let* ((path (plist-get item :path))
           (controller (and (buffer-live-p magit-treediff-parent-buffer)
                            magit-treediff-parent-buffer))
           (file (and controller
                      (with-current-buffer controller
                        (magit-treediff--file-model path))))
           (selected (and controller
                          (with-current-buffer controller
                            (equal magit-treediff-selected-file path))))
           (name-face (if selected
                          'magit-treediff-tree-current
                        'treemacs-git-modified-face))
           (summary (and file (magit-treediff--file-change-summary file))))
      (concat
       (propertize (plist-get item :name) 'face name-face)
       (when summary
         (propertize
          (format "  [%s] %dh +%d -%d"
                  (magit-treediff--file-status-token file)
                  (plist-get summary :hunks)
                  (plist-get summary :added)
                  (plist-get summary :removed))
          'face 'treemacs-git-untracked-face))))))

(defun magit-treediff--treemacs-ret-action (&optional _)
  "Visit or toggle the Treemacs node at point."
  (if-let* ((btn (treemacs-current-button))
            (file (treemacs-button-get btn :magit-treediff-file)))
      (magit-treediff-tree-visit-file)
    (treemacs-toggle-node)))

(defun magit-treediff--tree-move-to-file (delta)
  "Move DELTA file entries in the current tree buffer, skipping non-file nodes."
  (let ((step (if (< delta 0) -1 1))
        (remaining (abs delta)))
    (while (> remaining 0)
      (let ((origin (point))
            (start (point))
            (found nil))
        (forward-line step)
        (beginning-of-line)
        (while (and (not found)
                    (/= (point) start))
          (if (magit-treediff--tree-file-at-point)
              (setq found t)
            (setq start (point))
            (forward-line step)
            (beginning-of-line)))
        (if found
            (setq remaining (1- remaining))
          (goto-char origin)
          (setq remaining 0))))))

(defun magit-treediff--tree-call-with-follow (command)
  "Call tree COMMAND interactively, then update the diff from point."
  (funcall command)
  (magit-treediff--tree-follow-point))

(defun magit-treediff-tree-next-line ()
  (interactive)
  (magit-treediff--tree-call-with-follow
   (lambda () (magit-treediff--tree-move-to-file 1))))

(defun magit-treediff-tree-previous-line ()
  (interactive)
  (magit-treediff--tree-call-with-follow
   (lambda () (magit-treediff--tree-move-to-file -1))))

(defun magit-treediff-tree-next-line-fallback ()
  (interactive)
  (magit-treediff-tree-next-line))

(defun magit-treediff-tree-previous-line-fallback ()
  (interactive)
  (magit-treediff-tree-previous-line))

(defun magit-treediff--treemacs-install-local-keys ()
  "Install treediff-specific local keys into a Treemacs tree buffer."
  (local-set-key (kbd "j") #'magit-treediff-tree-next-line)
  (local-set-key (kbd "k") #'magit-treediff-tree-previous-line)
  (local-set-key (kbd "n") #'magit-treediff-tree-next-line-fallback)
  (local-set-key (kbd "p") #'magit-treediff-tree-previous-line-fallback)
  (local-set-key [down] #'magit-treediff-tree-next-line)
  (local-set-key [up] #'magit-treediff-tree-previous-line)
  (local-set-key (kbd "q") #'magit-treediff-tree-quit)
  (local-set-key (kbd "g") #'magit-treediff-tree-refresh)
  (local-set-key (kbd "s") #'magit-treediff-stage)
  (local-set-key (kbd "u") #'magit-treediff-unstage)
  (local-set-key (kbd "x") #'magit-treediff-discard)
  (local-set-key (kbd "RET") #'treemacs-RET-action))

(defun magit-treediff--ensure-treemacs-definitions ()
  "Define Treemacs extension nodes for treediff."
  (unless magit-treediff--treemacs-defined
    (unless (magit-treediff--treemacs-available-p)
      (user-error "Treemacs is not available on load-path"))
    (eval
     '(progn
        (treemacs-define-variadic-entry-node-type magit-treediff-treemacs-root
          :key 'magit-treediff-treemacs-root
          :children (magit-treediff--treemacs-file-children)
          :child-type 'magit-treediff-treemacs-node)
        (treemacs-define-expandable-node-type magit-treediff-treemacs-node
          :open-icon (if (eq (plist-get item :type) 'dir) "▾ " "  ")
          :closed-icon (if (eq (plist-get item :type) 'dir) "▸ " "  ")
          :label (magit-treediff--treemacs-node-label item)
          :key (or (plist-get item :path)
                   (concat (plist-get item :name) "/"))
          :children (and (eq (plist-get item :type) 'dir)
                         (plist-get item :children))
          :child-type 'magit-treediff-treemacs-node
          :more-properties
          (append
           (when (eq (plist-get item :type) 'file)
             `(:leaf t :magit-treediff-file ,(plist-get item :path)))
           `(:magit-treediff-node ,item))
          :ret-action #'magit-treediff--treemacs-ret-action)))
    (setq magit-treediff--treemacs-defined t)))

(defun magit-treediff--render-treemacs-tree ()
  "Render the current controller's file tree using Treemacs."
  (let ((controller magit-treediff-parent-buffer))
    (magit-treediff--ensure-treemacs-definitions)
    (eval
     `(treemacs-initialize magit-treediff-treemacs-root
        :with-expand-depth 'all
        :and-do
        (progn
          (magit-treediff--tree-buffer-common-setup ,controller)
          (magit-treediff--treemacs-install-local-keys))))
    (magit-treediff--goto-selected-tree-file)
    (set-buffer-modified-p nil)))

(defun magit-treediff--ensure-treemacs-tree-buffer ()
  "Ensure and return the Treemacs tree buffer for the current controller."
  (let ((controller (magit-treediff--controller-buffer)))
    (unless (buffer-live-p controller)
      (error "No treediff controller available"))
    (with-current-buffer controller
      (let ((tree (or (and (buffer-live-p magit-treediff--tree-buffer)
                           magit-treediff--tree-buffer)
                      (get-buffer-create
                       (magit-treediff--tree-buffer-name controller)))))
        (setq magit-treediff--tree-buffer tree)
        (with-current-buffer tree
          (magit-treediff--prepare-internal-buffer tree)
          (setq magit-treediff-parent-buffer controller)
          (magit-treediff--render-treemacs-tree))
        tree))))

(defun magit-treediff--ensure-builtin-tree-buffer ()
  (let ((controller (magit-treediff--controller-buffer)))
    (unless (buffer-live-p controller)
      (error "No treediff controller available"))
    (with-current-buffer controller
      (let ((tree (or (and (buffer-live-p magit-treediff--tree-buffer)
                           magit-treediff--tree-buffer)
                      (get-buffer-create
                       (magit-treediff--tree-buffer-name controller)))))
        (setq magit-treediff--tree-buffer tree)
        (with-current-buffer tree
          (magit-treediff--prepare-internal-buffer tree)
          (magit-treediff-tree-mode)
          (magit-treediff--tree-buffer-common-setup controller)
          (magit-treediff--render-tree))
        tree))))

(defun magit-treediff--ensure-tree-buffer ()
  "Ensure and return the tree buffer for the current tree backend."
  (magit-treediff--tree-call :ensure))

(defun magit-treediff--visible-tree-window ()
  "Return an existing visible tree window, if any."
  (seq-find
   (lambda (window)
     (with-current-buffer (window-buffer window)
       magit-treediff-tree-buffer-p))
   (window-list nil 'no-minibuf)))

(defun magit-treediff--delete-stale-tree-windows (buffer)
  "Delete visible treediff tree windows not showing BUFFER."
  (dolist (window (window-list nil 'no-minibuf))
    (when (and (not (eq (window-buffer window) buffer))
               (with-current-buffer (window-buffer window)
                 magit-treediff-tree-buffer-p))
      (set-window-dedicated-p window nil)
      (delete-window window))))

(defun magit-treediff--display-builtin-tree (buffer)
  (magit-treediff--delete-stale-tree-windows buffer)
  (let* ((existing (get-buffer-window buffer t))
         (stale (and (not existing)
                     (magit-treediff--visible-tree-window))))
    (let ((window (or existing
                      stale
                      (display-buffer-in-side-window
                       buffer
                       `((side . left)
                         (slot . 0)
                         (window-width . ,magit-treediff-tree-width))))))
      (set-window-dedicated-p window nil)
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)
      window)))

(defun magit-treediff--display-treemacs-tree (buffer)
  "Display Treemacs tree BUFFER in the standard treediff tree slot."
  (magit-treediff--display-builtin-tree buffer))

(defun magit-treediff--display-tree (buffer)
  "Display tree BUFFER using the active tree backend."
  (magit-treediff--tree-call :display buffer))

(defun magit-treediff--usable-base-window ()
  "Return a non-side window suitable for displaying pane buffers."
  (let* ((selected (selected-window))
         (windows (seq-filter
                   (lambda (window)
                     (and (not (window-minibuffer-p window))
                          (not (window-parameter window 'window-side))))
                   (window-list nil 'no-minibuf))))
    (cond
     ((not (window-parameter selected 'window-side))
      selected)
     (windows
      (car (sort windows
                 (lambda (a b)
                   (> (* (window-total-width a) (window-total-height a))
                      (* (window-total-width b) (window-total-height b)))))))
     (t
      selected))))

(defun magit-treediff--sync-builtin-tree ()
  (when-let ((buffer (and (buffer-live-p magit-treediff--tree-buffer)
                          magit-treediff--tree-buffer)))
    (with-current-buffer buffer
      (magit-treediff--render-tree))))

(defun magit-treediff--sync-treemacs-tree ()
  "Refresh the Treemacs tree backend for the current controller."
  (when-let ((buffer (and (buffer-live-p magit-treediff--tree-buffer)
                          magit-treediff--tree-buffer)))
    (with-current-buffer buffer
      (setq magit-treediff-parent-buffer
            (or (magit-treediff--controller-buffer)
                magit-treediff-parent-buffer))
      (magit-treediff--render-treemacs-tree))))

(defun magit-treediff--sync-tree ()
  "Refresh the active tree backend for the current controller."
  (magit-treediff--tree-call :sync))

(defun magit-treediff--ensure-pane-buffer (side)
  (let* ((controller (current-buffer))
         (slot (pcase side
                 ('left 'magit-treediff--left-buffer)
                 ('right 'magit-treediff--right-buffer)
                 (_ (error "Invalid pane side: %S" side))))
         (buffer (or (and (buffer-live-p (symbol-value slot))
                          (symbol-value slot))
                     (get-buffer-create
                      (magit-treediff--pane-buffer-name controller side)))))
    (set slot buffer)
    (with-current-buffer buffer
      (magit-treediff--prepare-internal-buffer buffer)
      (magit-treediff-pane-mode)
      (setq magit-treediff-parent-buffer controller)
      (setq magit-treediff-side side))
    buffer))

(defun magit-treediff--pane-face (side row)
  (magit-treediff--row-face (plist-get row :type) side))

(defun magit-treediff--other-pane-buffer (&optional buffer)
  "Return the pane buffer opposite BUFFER."
  (let* ((buffer (or buffer (current-buffer)))
         (controller (with-current-buffer buffer
                      (magit-treediff--controller-buffer))))
    (when (buffer-live-p controller)
      (with-current-buffer controller
        (cond
         ((eq buffer magit-treediff--left-buffer)
          magit-treediff--right-buffer)
         ((eq buffer magit-treediff--right-buffer)
          magit-treediff--left-buffer)
         (t nil))))))

(defun magit-treediff--overlay-at-point (var face &optional priority)
  (or (and (overlayp (symbol-value var))
           (symbol-value var))
      (let ((ov (make-overlay (point-min) (point-min) nil t t)))
        (overlay-put ov 'priority (or priority 30))
        (when face
          (overlay-put ov 'face face))
        (set var ov)
        ov)))

(defun magit-treediff--line-range-at-point ()
  (cons (line-beginning-position)
        (min (point-max) (1+ (line-end-position)))))

(defun magit-treediff--property-at-point (prop)
  (or (get-text-property (point) prop)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) prop))))

(defun magit-treediff--find-hunk-start (hunk)
  (let ((pos (point-min))
        found)
    (while (and (< pos (point-max)) (not found))
      (if (equal (get-text-property pos 'magit-treediff-hunk) hunk)
          (setq found pos)
        (setq pos (or (next-single-property-change
                       pos 'magit-treediff-hunk nil (point-max))
                      (point-max)))))
    found))

(defun magit-treediff--hunk-range (hunk)
  (when-let ((start (magit-treediff--find-hunk-start hunk)))
    (cons start
          (or (next-single-property-change start 'magit-treediff-hunk nil (point-max))
              (point-max)))))

(defun magit-treediff--sync-scroll (source-window target-window)
  "Mirror SOURCE-WINDOW scroll position into TARGET-WINDOW by line number."
  (let* ((source-buffer (window-buffer source-window))
         (target-buffer (window-buffer target-window))
         (source-start-line
          (with-current-buffer source-buffer
            (line-number-at-pos (window-start source-window))))
         (target-start
          (with-current-buffer target-buffer
            (magit-treediff--line-pos target-buffer source-start-line))))
    (let ((magit-treediff--inhibit-window-sync t))
      (set-window-start target-window target-start t))))

(defun magit-treediff--apply-pane-highlight-state (&optional buffer source-point)
  "Make the current row and hunk visible in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'magit-treediff-pane-mode)
      (save-excursion
        (when source-point
          (goto-char source-point))
        (let ((line-range (magit-treediff--line-range-at-point))
              (hunk (magit-treediff--property-at-point 'magit-treediff-hunk)))
          (move-overlay
           (magit-treediff--overlay-at-point
            'magit-treediff--current-line-overlay
            'magit-treediff-current-line
            40)
           (car line-range)
           (cdr line-range))
          (if-let ((hunk-range (and hunk (magit-treediff--hunk-range hunk))))
              (let ((ov (magit-treediff--overlay-at-point
                         'magit-treediff--current-hunk-overlay
                         nil
                         10)))
                (move-overlay ov (car hunk-range) (cdr hunk-range))
                (overlay-put ov 'face nil)
                (overlay-put ov 'line-prefix
                             (propertize "▎" 'face 'magit-treediff-current-hunk)))
            (let ((ov (magit-treediff--overlay-at-point
                       'magit-treediff--current-hunk-overlay
                       nil
                       10)))
              (overlay-put ov 'line-prefix nil)
              (delete-overlay ov))))))))

(defun magit-treediff--sync-pane-state ()
  "Mirror point and window start into the opposite pane."
  (unless magit-treediff--inhibit-sync
    (when-let* ((source (current-buffer))
                (target (and (derived-mode-p 'magit-treediff-pane-mode)
                             (magit-treediff--other-pane-buffer source)))
                (source-window (get-buffer-window source t))
                (target-window (get-buffer-window target t)))
      (let* ((source-point (window-point source-window))
             (line (line-number-at-pos source-point))
             (column (save-excursion
                       (goto-char source-point)
                       (current-column)))
             (target-point (magit-treediff--line-pos target line))
             (magit-treediff--inhibit-sync t))
        (magit-treediff--apply-pane-highlight-state source)
        (with-current-buffer target
          (save-excursion
            (goto-char target-point)
            (move-to-column column)
            (setq target-point (point)))
          (set-window-point target-window target-point)
          (magit-treediff--apply-pane-highlight-state target target-point))
        (magit-treediff--sync-scroll source-window target-window)))))

(defun magit-treediff--sync-pane-window (source-window)
  "Mirror SOURCE-WINDOW state into the opposite pane window."
  (unless magit-treediff--inhibit-window-sync
    (when-let* ((_ (window-live-p source-window))
                (_ (eq source-window (selected-window)))
                (source-buffer (window-buffer source-window))
                (_ (buffer-live-p source-buffer))
                (_ (with-current-buffer source-buffer
                     (derived-mode-p 'magit-treediff-pane-mode)))
                (target-buffer (with-current-buffer source-buffer
                                 (magit-treediff--other-pane-buffer source-buffer)))
                (target-window (get-buffer-window target-buffer t)))
      (let ((magit-treediff--inhibit-window-sync t))
        (with-current-buffer source-buffer
          (magit-treediff--sync-pane-state))
        (magit-treediff--sync-scroll source-window target-window)))))

(defun magit-treediff--window-scroll-sync (window _start)
  "Keep the opposite pane aligned when WINDOW scrolls."
  (magit-treediff--sync-pane-window window))

(defun magit-treediff--call-with-sync (command)
  "Run COMMAND interactively and mirror the resulting pane state."
  (call-interactively command)
  (when (derived-mode-p 'magit-treediff-pane-mode)
    (redisplay)
    (magit-treediff--sync-pane-state)))

(defun magit-treediff-next-line ()
  (interactive)
  (magit-treediff--call-with-sync #'next-line))

(defun magit-treediff-previous-line ()
  (interactive)
  (magit-treediff--call-with-sync #'previous-line))

(defun magit-treediff-scroll-up ()
  (interactive)
  (magit-treediff--call-with-sync #'scroll-up-command))

(defun magit-treediff-scroll-down ()
  (interactive)
  (magit-treediff--call-with-sync #'scroll-down-command))

(defun magit-treediff--current-file ()
  "Return the file associated with point or the controller selection."
  (or (get-text-property (point) 'magit-treediff-file)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'magit-treediff-file))
      (when-let ((controller (magit-treediff--controller-buffer)))
        (with-current-buffer controller
          magit-treediff-selected-file))))

(defun magit-treediff--tree-file-at-point ()
  "Return the selected tree file at point for any tree backend."
  (or (get-text-property (point) 'magit-treediff-file)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'magit-treediff-file))
      (when (and magit-treediff-tree-buffer-p
                 (fboundp 'treemacs-current-button)
                 (fboundp 'treemacs-button-get))
        (when-let ((btn (ignore-errors (treemacs-current-button))))
          (treemacs-button-get btn :magit-treediff-file)))))

(defun magit-treediff--with-controller (fn)
  "Call FN with the live treediff controller current."
  (if-let ((controller (magit-treediff--controller-buffer)))
      (with-current-buffer controller
        (funcall fn))
    (user-error "No treediff controller available")))

(defun magit-treediff-stage ()
  "Stage the current file from an unstaged treediff view."
  (interactive)
  (let ((file (magit-treediff--current-file))
        (scope (magit-treediff--current-scope))
        (hunk (and (derived-mode-p 'magit-treediff-pane-mode)
                   (magit-treediff--current-hunk))))
    (unless file
      (user-error "No file at point"))
    (magit-treediff--with-controller
     (lambda ()
       (unless (magit-treediff--capable-p :stage)
         (user-error "Staging is not available for this treediff source"))
       (pcase scope
         ('hunk
          (let ((file-model (or (magit-treediff--current-file-model)
                                (user-error "No file at point")))
                (hunk (or hunk
                          (user-error "No hunk at point"))))
            (magit-treediff--apply-patch
             (list file)
             "stage"
             '("--cached")
             (magit-treediff--patch-for-hunk file-model hunk))))
         (_
          (magit-stage-files (list file))))))
    (magit-treediff--with-controller
     #'magit-treediff--rerender-controller)))

(defun magit-treediff-unstage ()
  "Unstage the current file from a staged treediff view."
  (interactive)
  (let ((file (magit-treediff--current-file))
        (scope (magit-treediff--current-scope))
        (hunk (and (derived-mode-p 'magit-treediff-pane-mode)
                   (magit-treediff--current-hunk))))
    (unless file
      (user-error "No file at point"))
    (magit-treediff--with-controller
     (lambda ()
       (unless (magit-treediff--capable-p :unstage)
         (user-error "Unstaging is not available for this treediff source"))
       (pcase scope
         ('hunk
          (let ((file-model (or (magit-treediff--current-file-model)
                                (user-error "No file at point")))
                (hunk (or hunk
                          (user-error "No hunk at point"))))
            (magit-treediff--apply-patch
             (list file)
             "unstage"
             '("--reverse" "--cached")
             (magit-treediff--patch-for-hunk file-model hunk))))
         (_
          (magit-unstage-files (list file))))))
    (magit-treediff--with-controller
     #'magit-treediff--rerender-controller)))

(defun magit-treediff-discard ()
  "Discard the current file for the active treediff diff type."
  (interactive)
  (let ((file (magit-treediff--current-file))
        (scope (magit-treediff--current-scope))
        (hunk (and (derived-mode-p 'magit-treediff-pane-mode)
                   (magit-treediff--current-hunk))))
    (unless file
      (user-error "No file at point"))
    (unless (yes-or-no-p (format "Discard changes in %s? " file))
      (user-error "Abort"))
    (magit-treediff--with-controller
     (lambda ()
       (unless (magit-treediff--capable-p :discard)
         (user-error "Discard is not available for this treediff source"))
       (pcase scope
         ('hunk
          (let ((file-model (or (magit-treediff--current-file-model)
                                (user-error "No file at point")))
                (hunk (or hunk
                          (user-error "No hunk at point"))))
            (pcase (magit-treediff--effective-source-kind)
              ('unstaged
               (magit-treediff--apply-patch
                (list file) "discard" '("--reverse")
                (magit-treediff--patch-for-hunk file-model hunk)))
              ('staged
               (magit-treediff--apply-patch
                (list file) "discard" '("--reverse" "--cached")
                (magit-treediff--patch-for-hunk file-model hunk)))
              (_
               (user-error "Discard is not available for this treediff source")))))
         (_
          (pcase (magit-treediff--effective-source-kind)
            ('unstaged
             (magit-call-git "restore" "--worktree" "--" file))
            ('staged
             (magit-call-git "restore" "--staged" "--source=HEAD" "--" file))
            (_
             (user-error "Discard is not available for this treediff source")))))))
    (magit-treediff--with-controller
     #'magit-treediff--rerender-controller)))

(defun magit-treediff--pane-line-number (row side width face)
  (let ((line (pcase side
                ('left (plist-get row :left-line))
                ('right (plist-get row :right-line)))))
    (propertize
     (if line
         (format (format "%%%dd " width) line)
       (make-string (1+ width) ? ))
     'face (magit-treediff--merge-face 'magit-dimmed face))))

(defun magit-treediff--insert-pane-row (file hunk row side num-width)
  (let* ((face (magit-treediff--pane-face side row))
         (text (pcase side
                 ('left (plist-get row :left-text))
                 ('right (plist-get row :right-text))))
         (start (point)))
    (insert (magit-treediff--pane-line-number row side num-width face))
    (insert (magit-treediff--fontify-snippet text (plist-get file :file) face))
    (insert "\n")
    (add-text-properties
     start (point)
     `(magit-treediff-row ,row
       magit-treediff-hunk ,hunk
       magit-treediff-file ,(plist-get file :file)))))

(defun magit-treediff--render-pane-buffer (buffer file side)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq mode-name (format "Magit Treediff %s" (capitalize (symbol-name side))))
      (if (not file)
          (insert (propertize "No diff available\n" 'face 'magit-dimmed))
        (let ((num-width (magit-treediff--line-width file)))
          (insert (propertize
                   (magit-format-file 'diff
                                      (plist-get file :file)
                                      'magit-diff-file-heading
                                      (plist-get file :status)
                                      (plist-get file :source))
                   'face 'magit-diff-file-heading)
                  "\n")
          (if (plist-get file :binary)
              (insert (propertize "Binary files differ\n" 'face 'magit-dimmed))
            (dolist (hunk (plist-get file :hunks))
              (let ((header-start (point)))
                (insert (propertize (plist-get hunk :header)
                                    'face 'magit-diff-hunk-heading)
                        "\n")
                (add-text-properties
                 header-start (point)
                 `(magit-treediff-hunk ,hunk
                   magit-treediff-file ,(plist-get file :file))))
              (dolist (row (plist-get hunk :rows))
                (magit-treediff--insert-pane-row file hunk row side num-width)))
            (insert "\n"))))
      (goto-char (point-min))
      (set-buffer-modified-p nil))))

(defun magit-treediff--save-window-configuration ()
  (unless magit-treediff-previous-winconf
    (setq magit-treediff-previous-winconf
          (current-window-configuration))))

(defun magit-treediff--restore-window-configuration ()
  (when magit-treediff-previous-winconf
    (set-window-configuration magit-treediff-previous-winconf)
    (setq magit-treediff-previous-winconf nil)))

(defun magit-treediff--display-panes ()
  (let* ((left-buffer (magit-treediff--ensure-pane-buffer 'left))
         (right-buffer (magit-treediff--ensure-pane-buffer 'right))
         (left-window (or (get-buffer-window left-buffer t)
                          (let ((base (magit-treediff--usable-base-window)))
                            (magit-treediff--save-window-configuration)
                            (delete-other-windows base)
                            base)))
         (right-window (or (get-buffer-window right-buffer t)
                           (split-window left-window nil 'right))))
    (set-window-buffer left-window left-buffer)
    (set-window-buffer right-window right-buffer)
    (set-window-dedicated-p left-window t)
    (set-window-dedicated-p right-window t)
    (list left-window right-window)))

(defun magit-treediff--render-panes ()
  (let ((file (magit-treediff--file-model magit-treediff-selected-file)))
    (magit-treediff--render-pane-buffer
     (magit-treediff--ensure-pane-buffer 'left) file 'left)
    (magit-treediff--render-pane-buffer
     (magit-treediff--ensure-pane-buffer 'right) file 'right)
    (magit-treediff--display-panes)
    (with-current-buffer (magit-treediff--ensure-pane-buffer 'left)
      (magit-treediff--apply-pane-highlight-state))
    (with-current-buffer (magit-treediff--ensure-pane-buffer 'right)
      (magit-treediff--apply-pane-highlight-state))))

(defun magit-treediff--configure-controller (range typearg args files type
                                                       &optional origin)
  (let ((source-kind (magit-treediff--source-kind range typearg type)))
    (setq magit-treediff-range range
          magit-treediff-typearg typearg
          magit-treediff-type type
          magit-treediff-source-kind source-kind
          magit-treediff-source-origin origin
          magit-treediff-capabilities
          (magit-treediff--capabilities-for-source source-kind)
          magit-treediff-args args
          magit-treediff-files files)))

(defun magit-treediff--rerender-controller ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-treediff-refresh-buffer)))

(defvar magit-treediff-mode-map)
(defvar magit-treediff-pane-mode-map)
(defvar magit-treediff-tree-mode-map)
(defvar magit-treediff-mode-override-map (make-sparse-keymap))
(defvar magit-treediff-pane-mode-override-map (make-sparse-keymap))
(defvar magit-treediff-tree-mode-override-map (make-sparse-keymap))

(define-derived-mode magit-treediff-mode special-mode "Magit Treediff"
  "Major mode for the treediff controller buffer."
  :interactive nil
  :group 'magit-treediff
  (magit-treediff--install-override-map
   magit-treediff-mode-override-map)
  (setq-local face-remapping-alist
              (seq-remove
               (lambda (entry)
                 (memq (car-safe entry)
                       '(magit-diff-context-highlight
                         magit-diff-added
                         magit-diff-added-highlight
                         magit-diff-removed
                         magit-diff-removed-highlight)))
               face-remapping-alist))
  (setq-local magit-section-highlight-current nil)
  (setq-local hl-line-face 'magit-treediff-current-line)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (setq-local window-selection-change-functions nil)
  (setq magit-treediff--font-lock-cache nil)
  (setq magit-treediff--fontify-buffer nil)
  (setq magit--imenu-item-types '(sbs-file)))

(define-derived-mode magit-treediff-pane-mode special-mode "Magit Treediff Pane"
  "Helper mode for internal treediff pane buffers."
  :interactive nil
  (magit-treediff--install-override-map
   magit-treediff-pane-mode-override-map)
  (setq truncate-lines t)
  (setq-local pre-redisplay-functions nil)
  (setq-local mode-line-format nil)
  (setq-local window-selection-change-functions nil)
  (setq-local hl-line-face 'magit-treediff-current-line)
  (add-hook 'post-command-hook #'magit-treediff--sync-pane-state nil t)
  (add-hook 'window-scroll-functions #'magit-treediff--window-scroll-sync nil t)
  (hl-line-mode 1)
  (magit-treediff--apply-pane-highlight-state))

(defun magit-treediff--mode-setup ()
  "Apply treediff-specific setup after mode hooks ran."
  (when (bound-and-true-p magit-delta-mode)
    (magit-delta-mode -1))
  (setq-local face-remapping-alist
              (seq-remove
               (lambda (entry)
                 (memq (car-safe entry)
                       '(magit-diff-context-highlight
                         magit-diff-added
                         magit-diff-added-highlight
                         magit-diff-removed
                         magit-diff-removed-highlight)))
               face-remapping-alist))
  (magit-treediff--prepare-internal-buffer (current-buffer)))

(add-hook 'magit-treediff-mode-hook
          #'magit-treediff--mode-setup
          t)

;;; Normal (magit-diff) view

(defun magit-treediff--setup-diff-buffer ()
  "Create or update the magit-diff-mode buffer for normal view.
The diff is filtered to `magit-treediff-selected-file' so that only
the selected file is shown in the right pane.  Window changes are
suppressed via `save-window-excursion' so the caller controls display."
  (let* ((files (if magit-treediff-selected-file
                    (list magit-treediff-selected-file)
                  magit-treediff-files))
         (controller (current-buffer))
         (file magit-treediff-selected-file)
         (buffer
          (save-window-excursion
            (magit-diff-setup-buffer
             magit-treediff-range
             magit-treediff-typearg
             (or magit-treediff-args nil)
             files
             magit-treediff-type))))
    ;; Tag the diff buffer so that:
    ;; - magit-treediff--get-fontify-buffer can reach the controller (and
    ;;   thus its font-lock-mode cache and fontify scratch buffer).
    ;; - the post-refresh advice knows which file to re-highlight.
    (with-current-buffer buffer
      (setq-local magit-treediff-parent-buffer controller)
      (setq-local magit-treediff--highlight-file file)
      (magit-diff-syntax-local-mode -1))
    (setq magit-treediff--diff-buffer buffer)
    buffer))

(defun magit-treediff--goto-file-in-diff (file)
  "In the diff buffer, move point to the section for FILE and recenter."
  (when (and (buffer-live-p magit-treediff--diff-buffer) file)
    (with-current-buffer magit-treediff--diff-buffer
      (when-let ((section (magit-treediff--section-for-file file)))
        (let ((pos (magit-treediff--section-start section)))
          (goto-char pos)
          (when-let ((win (get-buffer-window (current-buffer) t)))
            (set-window-point win pos)
            (with-selected-window win
              (recenter 0))))))))

(defun magit-treediff--display-diff (buffer)
  "Display diff BUFFER in the usable base window."
  (let* ((existing (get-buffer-window buffer t))
         (window (or existing
                     (magit-treediff--usable-base-window))))
    (unless existing
      (magit-treediff--save-window-configuration))
    (set-window-buffer window buffer)
    (set-window-dedicated-p window nil)
    window))

(defun magit-treediff--diff-line-file (position &optional default-file)
  "Return the file for the diff line at POSITION.
DEFAULT-FILE is used when the buffer is already narrowed to a known file."
  (or default-file
      (save-excursion
        (goto-char position)
        (ignore-errors (magit-file-at-point)))))

(defun magit-treediff--apply-diff-syntax-highlighting
    (buffer &optional file start end)
  "Apply source-language syntax highlighting to diff content lines in BUFFER.
Each `+', `-', or context line has its content (after the prefix character)
fontified using cached syntax faces, preserving the underlying diff faces."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (save-excursion
          (goto-char (or start (point-min)))
          (while (and (not (eobp))
                      (< (point) (or end (point-max))))
            (let* ((bol (line-beginning-position))
                   (eol (line-end-position))
                   (prefix (and (> eol bol) (char-after bol))))
              (when (memq prefix '(?+ ?- ?\s))
                (let* ((content-start (1+ bol))
                       (line-file (magit-treediff--diff-line-file bol file))
                       (content (buffer-substring-no-properties content-start eol)))
                  (when (and line-file
                             (not (string-empty-p content)))
                    (let* ((base-face (or (get-text-property content-start 'face)
                                          (get-text-property content-start 'font-lock-face)))
                           (syntaxified
                            (magit-treediff--syntaxify-snippet content line-file)))
                      (dotimes (i (length syntaxified))
                        (let ((syntax-face (get-text-property i 'face syntaxified)))
                          (when syntax-face
                            (put-text-property
                             (+ content-start i) (+ content-start i 1)
                             'magit-treediff-syntax-face syntax-face)
                            (put-text-property
                             (+ content-start i) (+ content-start i 1)
                             'face
                             (magit-treediff--merge-face syntax-face
                                                              base-face)))))))))
              (forward-line 1))))))))

(defun magit-treediff--restore-diff-syntax-highlighting (buffer &optional start end)
  "Restore merged syntax faces in BUFFER from cached syntax properties."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (save-excursion
          (goto-char (or start (point-min)))
          (while (and (not (eobp))
                      (< (point) (or end (point-max))))
            (let* ((bol (line-beginning-position))
                   (eol (line-end-position))
                   (prefix (and (> eol bol) (char-after bol))))
              (when (memq prefix '(?+ ?- ?\s))
                (let ((content-start (1+ bol)))
                  (let ((pos content-start))
                    (while (< pos eol)
                      (let ((syntax-face
                             (get-text-property pos 'magit-treediff-syntax-face))
                            (next-pos
                             (or (next-single-property-change
                                  pos 'magit-treediff-syntax-face nil eol)
                                 eol)))
                        (when syntax-face
                          (let ((base-face
                                 (or (get-text-property pos 'font-lock-face)
                                     (let ((face (get-text-property pos 'face)))
                                       (unless (get-text-property
                                                pos
                                                'magit-treediff-syntax-face)
                                         face)))))
                            (put-text-property
                             pos next-pos 'face
                             (magit-treediff--merge-face
                              syntax-face base-face))))
                        (setq pos next-pos))))))
              (forward-line 1))))))))

(defun magit-treediff--clear-diff-syntax-highlighting (buffer &optional start end)
  "Remove treediff syntax overlay properties from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (save-excursion
          (goto-char (or start (point-min)))
          (while (and (not (eobp))
                      (< (point) (or end (point-max))))
            (let* ((bol (line-beginning-position))
                   (eol (line-end-position))
                   (prefix (and (> eol bol) (char-after bol))))
              (when (memq prefix '(?+ ?- ?\s))
                (let ((content-start (1+ bol)))
                  (remove-text-properties
                   content-start eol
                   '(magit-treediff-syntax-face nil
                     face nil))))
              (forward-line 1))))))))

(defun magit-treediff--uncached-syntax-ranges
    (buffer &optional file start end)
  "Return visible diff subranges in BUFFER that still need syntax faces.
Each returned element is a list (START END) spanning one or more consecutive
diff lines whose content does not yet have the cached
`magit-treediff-syntax-face' property."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((ranges nil)
            range-start
            range-end)
        (save-excursion
          (goto-char (or start (point-min)))
          (while (and (not (eobp))
                      (< (point) (or end (point-max))))
            (let* ((bol (line-beginning-position))
                   (eol (line-end-position))
                   (prefix (and (> eol bol) (char-after bol)))
                   (content-start (1+ bol))
                   (line-file (and (memq prefix '(?+ ?- ?\s))
                                   (magit-treediff--diff-line-file bol file)))
                   (needs-syntax
                    (and line-file
                         (> eol content-start)
                         (not (get-text-property
                               content-start
                               'magit-treediff-syntax-face)))))
              (if needs-syntax
                  (progn
                    (setq range-start (or range-start bol))
                    (setq range-end eol))
                (when range-start
                  (push (list range-start range-end) ranges)
                  (setq range-start nil)
                  (setq range-end nil))))
            (forward-line 1))
          (when range-start
            (push (list range-start range-end) ranges)))
        (nreverse ranges)))))

(define-minor-mode magit-diff-syntax-local-mode
  "Deprecated no-op local mode kept for compatibility."
  :init-value nil
  :lighter ""
  (setq magit-diff-syntax-local-mode nil)
  (when (timerp magit-treediff--syntaxify-timer)
    (cancel-timer magit-treediff--syntaxify-timer))
  (setq magit-treediff--syntaxify-timer nil)
  (setq magit-treediff--syntaxify-range nil)
  (when (timerp magit-treediff--rehighlight-timer)
    (cancel-timer magit-treediff--rehighlight-timer))
  (setq magit-treediff--rehighlight-timer nil)
  (setq magit-treediff--rehighlight-range nil)
  (setq magit-treediff--last-focused-hunk nil)
  (setq magit-treediff--last-visible-range nil)
  (magit-treediff--clear-diff-syntax-highlighting (current-buffer)))

(defun magit-diff-syntax-mode--maybe-enable ()
  "Deprecated no-op compatibility helper for `magit-diff-syntax-mode'."
  nil)

(define-globalized-minor-mode magit-diff-syntax-mode
  magit-diff-syntax-local-mode
  magit-diff-syntax-mode--maybe-enable)

(define-obsolete-variable-alias 'magit-diff-syntax-global-mode
  'magit-diff-syntax-mode "0.1.0")

(defun magit-treediff--merge-ranges (existing start end)
  "Merge EXISTING range with START and END.
Nil means the whole buffer and dominates narrower ranges."
  (cond
   ((or (null start) (null end))
    nil)
   ((null existing)
    (list start end))
   (t
   (pcase-let ((`(,existing-start ,existing-end) existing))
      (list (min existing-start start)
            (max existing-end end))))))

(defun magit-treediff--schedule-diff-syntax-apply
    (buffer file &optional start end)
  "Apply uncached syntax faces in BUFFER once Emacs is idle."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq magit-treediff--syntaxify-range
            (magit-treediff--merge-ranges
             magit-treediff--syntaxify-range start end))
      (when (timerp magit-treediff--syntaxify-timer)
        (cancel-timer magit-treediff--syntaxify-timer))
      (setq magit-treediff--syntaxify-timer
            (run-with-idle-timer
             0 nil
             (lambda (buf scope)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((range magit-treediff--syntaxify-range)
                         (default-file (unless (eq scope :dynamic) scope)))
                     (setq magit-treediff--syntaxify-range nil)
                     (setq magit-treediff--syntaxify-timer nil)
                     (when (and (magit-treediff--syntax-buffer-p)
                                (bound-and-true-p magit-diff-syntax-local-mode))
                       (if range
                           (pcase-let ((`(,from ,to) range))
                             (dolist (subrange
                                      (magit-treediff--uncached-syntax-ranges
                                       buf default-file from to))
                               (pcase-let ((`(,sub-start ,sub-end) subrange))
                                 (magit-treediff--apply-diff-syntax-highlighting
                                  buf default-file sub-start sub-end)))
                             (magit-treediff--schedule-diff-syntax-highlighting
                              buf scope from to))
                         (progn
                           (dolist (subrange
                                    (magit-treediff--uncached-syntax-ranges
                                     buf default-file))
                             (pcase-let ((`(,sub-start ,sub-end) subrange))
                               (magit-treediff--apply-diff-syntax-highlighting
                                buf default-file sub-start sub-end)))
                           (magit-treediff--schedule-diff-syntax-highlighting
                            buf scope))))))))
             buffer file)))))

(defun magit-treediff--schedule-diff-syntax-highlighting
    (buffer file &optional start end)
  "Restore regular-view syntax highlighting once BUFFER has settled."
  (ignore file)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq magit-treediff--rehighlight-range
            (magit-treediff--merge-ranges
             magit-treediff--rehighlight-range start end))
      (when (timerp magit-treediff--rehighlight-timer)
        (cancel-timer magit-treediff--rehighlight-timer))
      (setq magit-treediff--rehighlight-timer
            (run-with-idle-timer
             0 nil
             (lambda (buf _expected-scope)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((range magit-treediff--rehighlight-range))
                     (setq magit-treediff--rehighlight-range nil)
                     (setq magit-treediff--rehighlight-timer nil)
                     (when (and (magit-treediff--syntax-buffer-p)
                                (bound-and-true-p magit-diff-syntax-local-mode))
                       (if range
                           (pcase-let ((`(,from ,to) range))
                             (magit-treediff--restore-diff-syntax-highlighting
                              buf from to))
                         (magit-treediff--restore-diff-syntax-highlighting
                           buf)))))))
              buffer (magit-treediff--syntax-scope))))))

(defun magit-treediff--post-refresh-highlight (&rest _)
  "Compatibility no-op."
  nil)

(advice-add 'magit-diff-refresh-buffer :after
            #'magit-treediff--post-refresh-highlight)
(advice-add 'magit-status-refresh-buffer :after
            #'magit-treediff--post-refresh-highlight)

(defun magit-treediff--section-range (section)
  "Return the content range for SECTION as (START END)."
  (list (or (magit-treediff--section-content section)
            (magit-treediff--section-start section))
        (magit-treediff--section-end section)))

(defun magit-treediff--visible-range ()
  "Return the selected window's visible range for the current buffer."
  (let ((window (selected-window)))
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
      (list (window-start window)
            (window-end window t)))))

(defun magit-treediff--post-command-highlight ()
  "Compatibility no-op."
  nil)

(advice-add 'magit-section-post-command-hook :after
            #'magit-treediff--post-command-highlight)

(defun magit-treediff--render-normal ()
  "Set up the normal view: a real magit-diff buffer beside the tree."
  (let* ((buffer (magit-treediff--setup-diff-buffer))
         (file magit-treediff-selected-file))
    (magit-treediff--display-diff buffer)
    (magit-treediff--goto-file-in-diff file)))

(defun magit-treediff--focus-normal-viewer ()
  "Select the normal diff viewer window, if visible."
  (when-let ((window (and (buffer-live-p magit-treediff--diff-buffer)
                          (get-buffer-window magit-treediff--diff-buffer t))))
    (select-window window)))

(defun magit-treediff--cleanup-normal-viewer ()
  "Dispose of resources belonging to the normal viewer backend."
  (when (buffer-live-p magit-treediff--diff-buffer)
    (dolist (win (get-buffer-window-list magit-treediff--diff-buffer nil t))
      (set-window-dedicated-p win nil)
      (delete-window win))
    (kill-buffer magit-treediff--diff-buffer)
    (setq magit-treediff--diff-buffer nil)))

;;; File selection

(defun magit-treediff--select-file (file)
  "Select FILE in the controller and update the diff display."
  (let ((from-tree (and (boundp 'magit-treediff-tree-buffer-p)
                        magit-treediff-tree-buffer-p
                        (equal (magit-treediff--tree-file-at-point) file))))
    (setq magit-treediff-selected-file file)
    (magit-set-header-line-format (magit-treediff--format-header))
    (magit-treediff--viewer-call :render)
    ;; When selection originates from the tree itself, re-rendering the tree
    ;; immediately causes Treemacs/builtin point churn and makes movement feel
    ;; like it needs two keypresses. The tree is already on the selected file,
    ;; so only external selection changes need a tree sync.
    (unless from-tree
      (magit-treediff--sync-tree))))

(defvar-local magit-treediff-tree--last-file nil
  "The last file visited via point-motion auto-update.")

(defun magit-treediff-tree--auto-visit ()
  "Update the diff panel when the cursor moves onto a different file node."
  (when-let ((file (magit-treediff--tree-file-at-point)))
    (unless (equal file magit-treediff-tree--last-file)
      (setq magit-treediff-tree--last-file file)
      (when (buffer-live-p magit-treediff-parent-buffer)
        (with-current-buffer magit-treediff-parent-buffer
          (magit-treediff--select-file file))))))

(define-derived-mode magit-treediff-tree-mode special-mode "Magit Treediff Tree"
  "Mode used for the treediff file tree."
  :interactive nil
  (magit-treediff--install-override-map
   magit-treediff-tree-mode-override-map)
  (setq-local magit-treediff-tree-buffer-p t)
  (setq truncate-lines t)
  (setq-local mode-line-format nil)
  (setq-local window-selection-change-functions nil)
  (add-hook 'post-command-hook #'magit-treediff-tree--auto-visit nil t))

(defun magit-treediff--initialize-keymaps ()
  "Initialize or refresh treediff mode keymaps."
  (set-keymap-parent magit-treediff-mode-map special-mode-map)
  (keymap-set magit-treediff-mode-map "g"
              #'magit-treediff-controller-refresh)
  (keymap-set magit-treediff-mode-map "q"
              #'magit-treediff-quit)
  (keymap-set magit-treediff-mode-map "t"
              #'magit-treediff-toggle-tree)
  (keymap-set magit-treediff-mode-map "<remap> <magit-visit-thing>"
              #'magit-treediff-visit-file)

  (set-keymap-parent magit-treediff-pane-mode-map special-mode-map)
  (keymap-set magit-treediff-pane-mode-map "n" #'magit-treediff-next-line)
  (keymap-set magit-treediff-pane-mode-map "p" #'magit-treediff-previous-line)
  (keymap-set magit-treediff-pane-mode-map "SPC" #'magit-treediff-scroll-up)
  (keymap-set magit-treediff-pane-mode-map "S-SPC" #'magit-treediff-scroll-down)
  (keymap-set magit-treediff-pane-mode-map "DEL" #'magit-treediff-scroll-down)
  (keymap-set magit-treediff-pane-mode-map "g"
              #'magit-treediff-pane-refresh)
  (keymap-set magit-treediff-pane-mode-map "s"
              #'magit-treediff-stage)
  (keymap-set magit-treediff-pane-mode-map "u"
              #'magit-treediff-unstage)
  (keymap-set magit-treediff-pane-mode-map "k"
              #'magit-treediff-discard)
  (keymap-set magit-treediff-pane-mode-map "q"
              #'magit-treediff-pane-quit)
  (keymap-set magit-treediff-pane-mode-map "<remap> <magit-visit-thing>"
              #'magit-treediff-visit-file)

  (set-keymap-parent magit-treediff-tree-mode-map special-mode-map)
  (keymap-set magit-treediff-tree-mode-map "n" #'next-line)
  (keymap-set magit-treediff-tree-mode-map "p" #'previous-line)
  (keymap-set magit-treediff-tree-mode-map "g"
              #'magit-treediff-tree-refresh)
  (keymap-set magit-treediff-tree-mode-map "s"
              #'magit-treediff-stage)
  (keymap-set magit-treediff-tree-mode-map "u"
              #'magit-treediff-unstage)
  (keymap-set magit-treediff-tree-mode-map "k"
              #'magit-treediff-discard)
  (keymap-set magit-treediff-tree-mode-map "q"
              #'magit-treediff-tree-quit)
  (keymap-set magit-treediff-tree-mode-map "RET"
              #'magit-treediff-tree-visit-file))

(magit-treediff--initialize-keymaps)

(defun magit-treediff--initialize-evil-keymaps ()
  "Initialize Evil bindings for treediff modes."
  (when (featurep 'evil)
    (evil-define-key* 'normal magit-treediff-mode-map
      (kbd "q") #'magit-treediff-quit)
    (evil-define-key* 'normal magit-treediff-pane-mode-map
      (kbd "q") #'magit-treediff-pane-quit)
    (evil-define-key* 'normal magit-treediff-tree-mode-map
      (kbd "q") #'magit-treediff-tree-quit)))

(magit-treediff--initialize-evil-keymaps)
(with-eval-after-load 'evil
  (magit-treediff--initialize-evil-keymaps))

(defun magit-treediff--initialize-override-keymaps ()
  "Initialize high-priority treediff override keymaps."
  (keymap-set magit-treediff-mode-override-map "q"
              #'magit-treediff-quit)
  (keymap-set magit-treediff-pane-mode-override-map "q"
              #'magit-treediff-pane-quit)
  (keymap-set magit-treediff-tree-mode-override-map "j"
              #'magit-treediff-tree-next-line)
  (keymap-set magit-treediff-tree-mode-override-map "k"
              #'magit-treediff-tree-previous-line)
  (keymap-set magit-treediff-tree-mode-override-map "n"
              #'magit-treediff-tree-next-line-fallback)
  (keymap-set magit-treediff-tree-mode-override-map "p"
              #'magit-treediff-tree-previous-line-fallback)
  (keymap-set magit-treediff-tree-mode-override-map "<down>"
              #'magit-treediff-tree-next-line)
  (keymap-set magit-treediff-tree-mode-override-map "<up>"
              #'magit-treediff-tree-previous-line)
  (keymap-set magit-treediff-tree-mode-override-map "x"
              #'magit-treediff-discard)
  (keymap-set magit-treediff-tree-mode-override-map "q"
              #'magit-treediff-tree-quit))

(magit-treediff--initialize-override-keymaps)

(defun magit-treediff--install-override-map (map)
  "Install high-priority MAP for the current treediff buffer."
  (setq-local magit-treediff--override-mode t)
  (setq-local emulation-mode-map-alists
              (cons `((magit-treediff--override-mode . ,map))
                    emulation-mode-map-alists)))

(defun magit-treediff--refresh-live-buffers ()
  "Refresh treediff local state in already-live buffers."
  (magit-treediff--cleanup-invalid-auxiliary-buffers)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond
       (magit-treediff-tree-buffer-p
        (magit-treediff--install-override-map
         magit-treediff-tree-mode-override-map))
       ((derived-mode-p 'magit-treediff-pane-mode)
        (magit-treediff--install-override-map
         magit-treediff-pane-mode-override-map))
       ((derived-mode-p 'magit-treediff-mode)
        (magit-treediff--install-override-map
         magit-treediff-mode-override-map)))))
  (dolist (window (window-list nil 'no-minibuf))
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'magit-treediff-pane-mode)
                 (buffer-live-p magit-treediff-parent-buffer))
        (with-current-buffer magit-treediff-parent-buffer
          (magit-treediff--display-tree
           (magit-treediff--ensure-tree-buffer)))))))

(magit-treediff--refresh-live-buffers)

(defun magit-treediff-tree-refresh ()
  "Refresh the treediff tree buffer."
  (interactive)
  (if-let ((buffer (and (buffer-live-p magit-treediff-parent-buffer)
                        magit-treediff-parent-buffer)))
      (with-current-buffer buffer
        (magit-treediff--rerender-controller))
    (user-error "Parent buffer no longer exists")))

(defun magit-treediff-tree-visit-file ()
  "Show the diff for the file at point in the parent treediff buffer."
  (interactive)
  (let ((file (magit-treediff--tree-file-at-point)))
    (unless file
      (user-error "No file at point"))
    (unless (buffer-live-p magit-treediff-parent-buffer)
      (user-error "Parent buffer no longer exists"))
    (with-current-buffer magit-treediff-parent-buffer
      (magit-treediff--select-file file))))

(defun magit-treediff--current-file-model ()
  (let ((file (magit-treediff--current-file)))
    (magit-treediff--with-controller
     (lambda ()
       (and file
            (magit-treediff--file-model file))))))

(defun magit-treediff--current-hunk ()
  (or (get-text-property (point) 'magit-treediff-hunk)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'magit-treediff-hunk))))

(defun magit-treediff--current-scope ()
  (if (and (derived-mode-p 'magit-treediff-pane-mode)
           (magit-treediff--current-hunk))
      'hunk
    'file))

(defun magit-treediff--hunk-row-lines (row)
  (pcase (plist-get row :type)
    ('context (list (concat " " (or (plist-get row :left-text) ""))))
    ('added   (list (concat "+" (or (plist-get row :right-text) ""))))
    ('removed (list (concat "-" (or (plist-get row :left-text) ""))))
    ('changed (delq nil
                    (list (and (plist-get row :left-text)
                               (concat "-" (plist-get row :left-text)))
                          (and (plist-get row :right-text)
                               (concat "+" (plist-get row :right-text))))))
    (_ nil)))

(defun magit-treediff--patch-file-header (file)
  (let* ((old-file (or (plist-get file :source) (plist-get file :file)))
         (new-file (plist-get file :file))
         (deleted (equal (plist-get file :status) "deleted"))
         (created (equal (plist-get file :status) "new file")))
    (string-join
     (delq nil
           (list (format "diff --git %s %s" old-file new-file)
                 (format "--- %s" (if created "/dev/null" old-file))
                 (format "+++ %s" (if deleted "/dev/null" new-file))))
     "\n")))

(defun magit-treediff--patch-for-hunk (file hunk)
  (concat
   (magit-treediff--patch-file-header file)
   "\n"
   (plist-get hunk :header)
   "\n"
   (mapconcat (lambda (row)
                (string-join (magit-treediff--hunk-row-lines row) "\n"))
              (plist-get hunk :rows)
              "\n")
   "\n"))

(defun magit-treediff--apply-patch (files command args patch)
  (let ((context 3))
    (unless magit-inhibit-refresh
      (magit-run-before-change-functions files command))
    (with-temp-buffer
      (insert patch)
      (let ((magit-inhibit-refresh t))
        (magit-run-git-with-input
         "apply" args "-p0" (format "-C%s" context) "--ignore-space-change" "-")))
    (unless magit-inhibit-refresh
      (magit-run-after-apply-functions files command)
      (magit-refresh))))

(defun magit-treediff--visit-side (row side)
  (let ((preferred (if (and side (plist-get row (intern (format ":%s-line" side))))
                       side
                     nil)))
    (or preferred
        (and (plist-get row :left-line) 'left)
        (and (plist-get row :right-line) 'right)
        side
        'right)))

(defun magit-treediff--visit-sides (file)
  (magit-treediff--with-controller
     (lambda ()
       (let* ((old-file (or (plist-get file :source) (plist-get file :file)))
              (new-file (plist-get file :file)))
       (pcase (magit-treediff--effective-source-kind)
         ('staged
          (list (list (magit-rev-abbrev "HEAD") old-file)
                (list "{index}" new-file)))
         ('unstaged
          (list (list (if (magit-anything-staged-p nil old-file)
                          "{index}"
                        (magit-rev-abbrev "HEAD"))
                      old-file)
                (list "{worktree}" new-file)))
         ('range
          (pcase-let* ((`(,old-rev . ,new-rev)
                        (magit-split-range magit-treediff-range t)))
            (list (list old-rev old-file)
                  (list new-rev new-file))))
         (_
          (list (list "{worktree}" old-file)
                (list "{worktree}" new-file))))))))

(defun magit-treediff--visit-target ()
  (let* ((row (or (get-text-property (point) 'magit-treediff-row)
                  (and (> (point) (point-min))
                       (get-text-property (1- (point)) 'magit-treediff-row))
                  (user-error "No diff row at point")))
         (file (or (magit-treediff--current-file-model)
                   (user-error "No file at point")))
         (side (magit-treediff--visit-side row
                                               (and (boundp 'magit-treediff-side)
                                                    magit-treediff-side)))
         (sides (magit-treediff--visit-sides file))
         (pair (pcase side
                 ('left (car sides))
                 (_ (cadr sides))))
         (line (pcase side
                 ('left (or (plist-get row :left-line) (plist-get row :right-line) 1))
                 (_ (or (plist-get row :right-line) (plist-get row :left-line) 1)))))
    (list (nth 0 pair) (nth 1 pair) line)))

(defun magit-treediff-visit-file ()
  "Visit the file/blob appropriate for the current diff row and pane."
  (interactive)
  (pcase-let ((`(,rev ,file ,line) (magit-treediff--visit-target)))
    (let ((buf (magit-find-file-noselect rev file)))
      (pop-to-buffer-same-window buf)
      (with-current-buffer buf
        (goto-char (point-min))
        (forward-line (1- line)))
      buf)))

(defun magit-treediff-pane-refresh ()
  "Refresh the parent treediff controller buffer."
  (interactive)
  (if (buffer-live-p magit-treediff-parent-buffer)
      (with-current-buffer magit-treediff-parent-buffer
        (magit-treediff--rerender-controller))
    (user-error "Parent buffer no longer exists")))

(defun magit-treediff-controller-refresh ()
  "Refresh the current treediff controller."
  (interactive)
  (magit-treediff--rerender-controller))

(defun magit-treediff-pane-quit ()
  "Quit the compatibility pane buffer and its treediff controller."
  (interactive)
  (if (buffer-live-p magit-treediff-parent-buffer)
      (with-current-buffer magit-treediff-parent-buffer
        (magit-treediff-quit))
    (quit-window)))

(defun magit-treediff-tree-quit ()
  "Quit the treediff tree and its controller."
  (interactive)
  (if (buffer-live-p magit-treediff-parent-buffer)
      (with-current-buffer magit-treediff-parent-buffer
        (magit-treediff-quit))
    (quit-window)))

(defun magit-treediff-toggle-tree ()
  "Toggle the treediff tree window."
  (interactive)
  (if-let ((window (and (buffer-live-p magit-treediff--tree-buffer)
                        (get-buffer-window magit-treediff--tree-buffer t))))
      (delete-window window)
    (magit-treediff--display-tree
     (magit-treediff--ensure-tree-buffer))))

(defun magit-treediff-quit ()
  "Close the tree diff viewer windows and buffers."
  (interactive)
  (dolist (buffer (list magit-treediff--tree-buffer
                        magit-treediff--left-buffer
                        magit-treediff--right-buffer
                        magit-treediff--diff-buffer
                        magit-treediff--fontify-buffer))
    (when (buffer-live-p buffer)
      (set-buffer-modified-p nil)))
  (ignore-errors
    (magit-treediff--restore-window-configuration))
  (dolist (buffer (list magit-treediff--tree-buffer
                        magit-treediff--left-buffer
                        magit-treediff--right-buffer
                        magit-treediff--diff-buffer
                        magit-treediff--fontify-buffer))
    (when (buffer-live-p buffer)
      (ignore-errors
        (kill-buffer buffer))))
  (ignore-errors
    (bury-buffer)))

(defun magit-treediff--viewer-buffers ()
  "Return all live treediff controller buffers for the current repository."
  (let ((topdir (ignore-errors (magit-toplevel))))
    (seq-filter
     (lambda (buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'magit-treediff-mode)
              (equal topdir (ignore-errors (magit-toplevel))))))
     (buffer-list))))

(defun magit-treediff-refresh-buffer ()
  "Refresh the current `magit-treediff-mode' buffer."
  (unless (or magit-treediff-typearg magit-treediff-type
              magit-treediff-range)
    (pcase-let ((`(,range ,typearg ,type) (magit-treediff--dwim-target)))
      (setq magit-treediff-range range
            magit-treediff-typearg typearg
            magit-treediff-type type)))
  ;; The model is always built: the tree uses it regardless of view style.
  (setq magit-treediff--model (magit-treediff--model))
  (setq magit-treediff-selected-file (magit-treediff--select-default-file))
  (magit-set-header-line-format (magit-treediff--format-header))
  (magit-treediff--display-tree
   (magit-treediff--ensure-tree-buffer))
  (magit-treediff--viewer-call :render)
  (magit-treediff--sync-tree)
  (set-buffer-modified-p nil))

(defun magit-treediff-buffer-name ()
  (format "Magit Treediff: %s/"
          (abbreviate-file-name (magit-toplevel))))

(defun magit-treediff-setup-buffer (range typearg args files
                                              &optional type locked origin)
  (ignore locked)
  (let ((buffer (get-buffer-create (magit-treediff-buffer-name))))
    (with-current-buffer buffer
      (magit-treediff-mode)
      (setq default-directory (file-name-as-directory (magit-toplevel)))
      (magit-treediff--configure-controller range typearg args files type origin))
    buffer))

(defun magit-treediff--dwim-target ()
  (pcase (magit-treediff--magit-diff-dwim)
    ('unmerged
     (unless (magit-merge-in-progress-p)
       (user-error "No merge is in progress"))
     (list (magit--merge-range) nil 'committed))
    ('unstaged
     (list nil nil 'unstaged))
    ('staged
     (list nil "--cached" 'staged))
    (`(commit . ,value)
     (list (format "%s^..%s" value value) nil 'committed))
    ((and range (pred stringp))
     (list range nil 'committed))
    (_
     (cond
      ((magit-anything-unstaged-p) (list nil nil 'unstaged))
      ((magit-anything-staged-p)   (list nil "--cached" 'staged))
      (t (list (or (magit-get-current-branch) "HEAD") nil 'committed))))))

(defun magit-treediff--focus-viewer ()
  "Select the appropriate diff window of the current viewer, if visible."
  (magit-treediff--viewer-call :focus))

;;;###autoload
(defun magit-treediff (&optional args files)
  "Show the current diff context in the treediff viewer."
  (interactive (magit-diff-arguments))
  (magit-treediff--cleanup-invalid-auxiliary-buffers)
  (let ((origin (magit-treediff--source-origin)))
    (pcase-let* ((`(,range ,typearg ,type) (magit-treediff--dwim-target))
               (buffer (or (magit-treediff--existing-viewer)
                           (magit-treediff-setup-buffer
                            range typearg args files type nil origin))))
      (with-current-buffer buffer
        (magit-treediff--configure-controller range typearg args files type origin)
        (magit-treediff--rerender-controller)
        (let ((controller (current-buffer)))
          (magit-treediff--focus-viewer)
          (with-current-buffer controller
            (magit-treediff--display-tree
             (magit-treediff--ensure-tree-buffer)))))
      buffer)))

(defun magit-treediff-debug-point ()
  "Report all face-related properties and overlay priorities at point."
  (interactive)
  (let* ((pos (point))
         (ovs (overlays-at pos t))  ; sorted = highest priority first
         (ov-info (mapcar (lambda (ov)
                            (list :face     (overlay-get ov 'face)
                                  :fl-face  (overlay-get ov 'font-lock-face)
                                  :priority (overlay-get ov 'priority)))
                          ovs))
         (eff-face (get-char-property pos 'face))
         (eff-fl-face (get-char-property pos 'font-lock-face)))
    (message (concat "pos=%d\n"
                     "text-prop  face=%-30S fl-face=%S\n"
                     "effective  face=%-30S fl-face=%S\n"
                     "resolved   fg=%-20S bg=%S\n"
                     "overlays (hi→lo priority):\n%s")
             pos
             (get-text-property pos 'face)
             (get-text-property pos 'font-lock-face)
             eff-face eff-fl-face
             (face-foreground eff-face nil t)
             (face-background eff-face nil t)
             (mapconcat (lambda (o)
                          (format "  face=%-30S fl-face=%-30S priority=%S"
                                  (plist-get o :face)
                                  (plist-get o :fl-face)
                                  (plist-get o :priority)))
                        ov-info "\n"))))

(provide 'magit-treediff)

;;; magit-treediff.el ends here
