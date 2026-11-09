;;; bndos/editor/config.el -*- lexical-binding: t; -*-

(setq delete-by-moving-to-trash t
      bookmark-default-file (file-name-concat doom-user-dir "bookmarks"))
(setq-default comment-line-break-function nil)
(put 'set-goal-column 'disabled nil)


;;; Evil

;; Insert state behaves like plain Emacs, except ESC.
(after! evil
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] #'evil-normal-state))

(map! :n "g s k" #'bndos/evilem-previous-line
      :n "g s j" #'bndos/evilem-next-line
      :n "."     #'evil-repeat
      :n "*p"    #'bndos/insert-primary-selection
      :v "+y"    #'clipboard-kill-ring-save
      :i "C-?"   #'undo-fu-only-redo
      :i "C-M-/" #'undo-fu-only-redo-all
      "H-d" #'bndos/scroll-down-lines
      "H-u" #'bndos/scroll-up-lines)


;;; Completion

(setq consult-fontify-max-size 1024)

(defadvice! bndos-consult-xref-sort-a (candidates)
  "Sort Consult Xref CANDIDATES by file, then line number."
  :filter-return #'consult-xref--candidates
  (cl-flet ((key (candidate)
              (let ((loc (xref-item-location
                          (get-text-property 0 'consult-xref candidate))))
                (cons (xref-location-group loc)
                      (or (xref-location-line loc) 0)))))
    (cl-stable-sort (copy-sequence candidates)
                    (lambda (a b)
                      (let ((a (key a)) (b (key b)))
                        (or (string< (car a) (car b))
                            (and (string= (car a) (car b))
                                 (< (cdr a) (cdr b)))))))))

(after! corfu
  (setq corfu-auto-delay 0
        corfu-auto-prefix 0
        corfu-popupinfo-delay '(0.1 . 0.1))
  ;; Doom binds these in `corfu-mode-map', shadowing line navigation and
  ;; set-mark everywhere corfu is on.  `corfu-map' still handles them while the
  ;; popup is open.
  (map! :map corfu-mode-map
        :i "C-n"   nil
        :i "C-p"   nil
        :i "C-SPC" nil
        :i "C-@"   nil))


;;; Editing

(map! :after smartparens
      :map smartparens-mode-map
      "M-<backspace>" #'sp-backward-unwrap-sexp)

(map! "H-."  #'mc/mark-next-like-this
      "H-,"  #'mc/mark-previous-like-this
      "H->"  #'mc/skip-to-next-like-this
      "H-<"  #'mc/skip-to-previous-like-this
      "C-\"" #'mc/mark-all-like-this
      (:after multiple-cursors-core
       :map mc/keymap
       "H-x C-." #'mc/unmark-next-like-this
       "H-x C-," #'mc/unmark-previous-like-this
       "H-x C-:" #'mc/mark-pop
       "M-["     #'mc/insert-numbers
       "M-]"     #'mc/insert-letters
       "C-x C-a" #'mc/vertical-align-with-space))


;;; Terminal clipboard

(add-hook! '(doom-after-init-hook server-after-make-frame-hook)
           #'bndos-clipboard-enable-h)
