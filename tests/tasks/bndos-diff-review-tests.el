;;; bndos-diff-review-tests.el --- Margin signs, scrolling past drawn rows, review colours -*- lexical-binding: t; -*-

(require 'test-helper)
(require 'bndos-review-tests)

(defun bndos-test-margin-marks (string)
  "The left-margin texts STRING displays, in order."
  (let ((i 0) marks)
    (while (< i (length string))
      (let ((display (get-text-property i 'display string)))
        (when (equal (car-safe display) '(margin left-margin))
          (push (cadr display) marks)))
      (setq i (next-single-property-change i 'display string (length string))))
    (nreverse marks)))

(defun bndos-test-marks-at (pos)
  "The margin texts drawn before POS by this buffer's zero-width overlays, by priority."
  (mapcar (lambda (o) (car (bndos-test-margin-marks (overlay-get o 'before-string))))
          (sort (seq-filter (lambda (o) (and (= (overlay-start o) pos) (overlay-get o 'before-string)
                                             (bndos-test-margin-marks (overlay-get o 'before-string))))
                            diff-review--overlays)
                (lambda (a b) (< (or (overlay-get a 'priority) 0) (or (overlay-get b 'priority) 0))))))

(defun bndos-test-sign (mark)
  "MARK's text and face."
  (cons (substring-no-properties mark) (get-text-property 0 'face mark)))

(defun bndos-test-comment-at (line body)
  "Comment BODY on LINE of this file."
  (goto-char (diff-review--line-pos line))
  (cl-letf (((symbol-function 'read-string-from-buffer) (bndos-test-say body)))
    (diff-review-comment)))


;;; Signs in the margin

(ert-deftest bndos-diff-signs-faces-inherit-the-indicators ()
  (should (eq (face-attribute 'diff-review-sign-added :inherit) 'diff-indicator-added))
  (should (eq (face-attribute 'diff-review-sign-removed :inherit) 'diff-indicator-removed)))

(ert-deftest bndos-diff-signs-mark-added-lines-and-removed-rows ()
  (let ((diff-review-margin-signs t)
        (diff-review-thread-marker nil))
    (bndos-test-with-diff-view (key wt review file)
      ;; one / TWO / four over one / two / three: lines 2 and 3 added.
      (let ((carrier (seq-find (lambda (o) (overlay-get o 'diff-review-mark)) diff-review--overlays)))
        (should (equal (mapcar #'bndos-test-sign (bndos-test-margin-marks (overlay-get carrier 'before-string)))
                       '(("+" . diff-review-sign-added)))))
      (should (equal (mapcar #'bndos-test-sign (bndos-test-marks-at (diff-review--line-pos 3)))
                     '(("+" . diff-review-sign-added))))
      (should-not (bndos-test-marks-at (diff-review--line-pos 1)))
      ;; The removed rows above line 2.
      (let ((lift (seq-find (lambda (o) (and (overlay-get o 'diff-review-rows-of)
                                             (= (overlay-end o) (diff-review--line-pos 2))))
                            diff-review--overlays)))
        (should (equal (mapcar #'bndos-test-sign (bndos-test-margin-marks (overlay-get lift 'before-string)))
                       '(("-" . diff-review-sign-removed) ("-" . diff-review-sign-removed))))))))

(ert-deftest bndos-diff-signs-mark-a-deleted-file ()
  (let ((diff-review-margin-signs t)
        (diff-review-thread-marker nil))
    (bndos-test-with-review (key wt review)
        (lambda (wt) (bndos-test-git wt "rm" "-q" "src/a.txt") (bndos-test-commit wt "drop"))
      (with-current-buffer (bndos-test-open-file review "src/a.txt")
        (should diff-review--deleted)
        (dolist (line '(1 2 3))
          (should (equal (mapcar #'bndos-test-sign (bndos-test-marks-at (diff-review--line-pos line)))
                         '(("-" . diff-review-sign-removed)))))))))

(ert-deftest bndos-diff-signs-off-keeps-the-fringe ()
  (let ((diff-review-margin-signs nil))
    (should (equal (diff-review--change-mark nil) (diff-review--fringe-mark 'diff-indicator-added)))
    (should (equal (diff-review--change-mark t) (diff-review--fringe-mark 'diff-indicator-removed)))))


;;; Sign, then thread marker

(ert-deftest bndos-diff-thread-markers-line-up-after-the-signs ()
  (let ((diff-review-margin-signs t)
        (diff-review-thread-marker "▐"))
    (bndos-test-with-diff-view (key wt review file)
      (bndos-test-comment-at 1 "on one")
      (bndos-test-comment-at 3 "on four")
      (let ((marks (lambda (line) (mapcar #'substring-no-properties
                                          (bndos-test-marks-at (diff-review--line-pos line))))))
        ;; Line 1 has no sign: its marker keeps the sign's column.
        (should (equal (funcall marks 1) '(" ▐")))
        ;; Line 3, added: the sign, then the marker, which outranks it.
        (should (equal (funcall marks 3) '("+" "▐"))))
      (let ((bars (seq-filter (lambda (o) (equal (mapcar #'substring-no-properties
                                                         (bndos-test-margin-marks (or (overlay-get o 'before-string) "")))
                                                 '("▐")))
                              diff-review--overlays)))
        (should bars)
        (should (seq-every-p (lambda (o) (eql (overlay-get o 'priority) 1)) bars)))
      ;; A thread's rows have no sign: padded too.
      (let ((thread (seq-find (lambda (o) (equal (gethash "body" (overlay-get o 'diff-review-comment)) "on one"))
                              (diff-review--thread-overlays))))
        (diff-review--redraw-thread thread t)
        (let* ((lift (seq-find (lambda (o) (and (overlay-get o 'diff-review-rows-of)
                                                (= (overlay-end o) (overlay-end thread))))
                               diff-review--overlays))
               (marks (mapcar #'substring-no-properties
                              (bndos-test-margin-marks (overlay-get lift 'before-string)))))
          (should (member " ▐" marks))
          (should-not (member "▐" marks)))))))

(ert-deftest bndos-diff-thread-bar-pads-only-beside-signs ()
  (let ((diff-review-thread-marker "▐"))
    (let ((diff-review-margin-signs t))
      (should (equal (bndos-test-margin-marks (diff-review--thread-bar nil nil)) '(" ▐")))
      (should (equal (bndos-test-margin-marks (diff-review--thread-bar nil t)) '("▐"))))
    (let ((diff-review-margin-signs nil))
      (should (equal (bndos-test-margin-marks (diff-review--thread-bar nil nil)) '("▐"))))
    (let ((external (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'diff-review--external-p) (lambda (c) (eq c external))))
        (should (eq (get-text-property 0 'face (car (bndos-test-margin-marks (diff-review--thread-bar external t))))
                    'diff-review-external-bar))
        (should (eq (get-text-property 0 'face (car (bndos-test-margin-marks (diff-review--thread-bar nil t))))
                    'diff-review-comment-bar))))))


;;; The margin's width

(ert-deftest bndos-diff-margin-width-counts-signs-and-markers ()
  (pcase-dolist (`(,signs ,marker ,width) '((t "▐" 2) (t nil 1) (nil "▐" 1)))
    (let ((diff-review-margin-signs signs)
          (diff-review-thread-marker marker))
      (bndos-test-with-diff-view (key wt review file)
        (should (= left-margin-width width))
        (diff-review-toggle-plain)
        (should (= left-margin-width 0))
        (diff-review-toggle-plain)
        (should (= left-margin-width width))
        (with-current-buffer (diff-review-toggle-view)
          (should (= left-margin-width width)))))))


;;; The diff view's signs

(ert-deftest bndos-diff-view-signs-use-the-sign-faces ()
  (bndos-test-with-diff-view (key wt review file)
    (with-current-buffer (diff-review-toggle-view)
      (let ((prefix (lambda (text) (bndos-test-view-goto text)
                      (bndos-test-sign (get-text-property (point) 'line-prefix)))))
        (should (equal (funcall prefix "TWO") '("+" . diff-review-sign-added)))
        (should (equal (funcall prefix "two") '("-" . diff-review-sign-removed)))
        (should (equal (get-text-property (progn (bndos-test-view-goto "one") (point)) 'line-prefix) " "))))))


;;; Scrolling past rows drawn at one position

(ert-deftest bndos-diff-scroll-advice-is-installed ()
  (should (advice-member-p #'diff-review--scroll-up 'scroll-up))
  (should (advice-member-p #'diff-review--scroll-down 'scroll-down)))

(ert-deftest bndos-diff-scroll-that-stays-moves-one-line ()
  (bndos-test-with-diff-view (key wt review file)
    (let ((stuck (lambda (&rest _) nil)))
      ;; Up: on to line 2, point with it.
      (set-window-start nil (point-min))
      (set-window-point nil (point-min))
      (diff-review--scroll-up stuck)
      (should (= (window-start) (diff-review--line-pos 2)))
      (should (= (window-point) (diff-review--line-pos 2)))
      ;; Down: back to line 1.
      (diff-review--scroll-down stuck)
      (should (= (window-start) (diff-review--line-pos 1)))
      ;; A negative arg turns it.
      (set-window-start nil (diff-review--line-pos 3))
      (diff-review--scroll-up stuck -1)
      (should (= (window-start) (diff-review--line-pos 2)))
      ;; At the top there is no line before.
      (set-window-start nil (point-min))
      (diff-review--scroll-down stuck)
      (should (= (window-start) (point-min))))))

(ert-deftest bndos-diff-scroll-that-moves-is-left-alone ()
  (bndos-test-with-diff-view (key wt review file)
    (set-window-start nil (point-min))
    (let (got)
      (diff-review--scroll-up (lambda (&rest args) (setq got args)
                                (set-window-start nil (diff-review--line-pos 3)))
                              2)
      (should (equal got '(2)))
      (should (= (window-start) (diff-review--line-pos 3))))))

(ert-deftest bndos-diff-scroll-outside-a-review-is-untouched ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (switch-to-buffer (current-buffer))
    (set-window-start nil (point-min))
    (let ((calls 0))
      (diff-review--scroll-up (lambda (&rest _) (cl-incf calls)))
      (diff-review--scroll-down (lambda (&rest _) (cl-incf calls)))
      (should (= calls 2))
      (should (= (window-start) (point-min))))))


;;; Colours

(ert-deftest bndos-diff-colours-are-github-darks ()
  (let ((config (with-temp-buffer
                  (insert-file-contents (expand-file-name "../../modules/bndos/ui/config.el" bndos-test-dir))
                  (buffer-string))))
    (dolist (spec '("'(diff-review-added :background \"#12261e\" :extend t)"
                    "'(diff-review-removed :background \"#301b1e\" :extend t)"
                    "'(diff-review-sign-added :foreground \"#3fb950\" :background \"#1c4328\" :weight bold)"
                    "'(diff-review-sign-removed :foreground \"#f85149\" :background \"#532426\" :weight bold)"))
      (should (string-search spec config)))))

;;; Long threads

(defconst bndos-diff-long-body
  (concat (make-string 30 ?a) " " (mapconcat #'identity (make-list 40 "word") " ")
          "\n\nsecond " (mapconcat #'identity (make-list 40 "para") " ")))

(ert-deftest bndos-diff-every-paragraph-is-filled ()
  (skip-unless (require 'markdown-mode nil t))
  (let ((lines (split-string (diff-review--markdown-render bndos-diff-long-body 40) "\n")))
    (should (> (length lines) 6))
    (should (seq-every-p (lambda (l) (<= (string-width l) 40)) lines))))

(ert-deftest bndos-diff-overview-lists-a-long-comment-on-one-row ()
  (let ((c (make-hash-table :test #'equal)))
    (puthash "id" "c1" c) (puthash "path" "a.txt" c) (puthash "line" 2 c)
    (puthash "author" "me" c) (puthash "body" bndos-diff-long-body c)
    (with-temp-buffer
      (cl-letf (((symbol-function 'diff-review--label) #'ignore)
                ((symbol-function 'frame-width) (lambda (&rest _) 100)))
        (let ((line (diff-review--comment-line c)))
          (should (= (length (split-string (string-trim-right line "\n") "\n")) 1))
          (should (<= (string-width line) 70)))))))

(ert-deftest bndos-diff-open-threads-in-a-file-are-cards ()
  (skip-unless (require 'markdown-mode nil t))
  (let ((c (make-hash-table :test #'equal))
        (r (make-hash-table :test #'equal)))
    (puthash "id" "c1" c) (puthash "author" "me" c) (puthash "body" bndos-diff-long-body c)
    (puthash "author" "agent:x" r) (puthash "body" "done" r)
    (puthash "replies" (vector r) c)
    (with-temp-buffer
      (let* ((fill-column 60)
             (text (diff-review--thread-string c 2 2 nil t)))
        (should (string-match-p "│ second" text))          ; the second paragraph, in the card
        (should (string-match-p "◆ agent:x" text))         ; the reply's own header
        (should-not (string-match-p "^\\S-*. me +[0-9]" (car (split-string text "\n"))))))
    ;; Read only: no keys to reply, resolve or delete.
    (puthash "external" "GitHub" c)
    (with-temp-buffer
      (setq-local diff-review-file-mode t)
      (cl-letf (((symbol-function 'diff-review--external-p) (lambda (_) "GitHub")))
        (let ((diff-review-file-header-keys t))
          (should-not (string-match-p "reply" (diff-review--thread-string c 2 2 nil t))))))))

(provide 'bndos-diff-review-tests)
;;; bndos-diff-review-tests.el ends here
