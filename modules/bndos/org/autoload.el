;;; bndos/org/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun bndos-org-inline-svg-a (fn file width)
  "Render SVG inline images natively, no taller than `bndos-org-image-max-height'."
  (let ((image (funcall fn file width)))
    (if (and image (member (downcase (or (file-name-extension file) "")) '("svg" "svgz")))
        (let ((window (or (get-buffer-window (current-buffer) t) (selected-window)))
              (props (copy-sequence (cdr image))))
          (setq props (plist-put props :type 'svg))
          (setq props (plist-put props :max-height
                                 (floor (* bndos-org-image-max-height
                                           (window-body-height window t)))))
          (cons (car image) props))
      image)))

;;;###autoload
(defun bndos-org-mermaid-fix-svg-spacing-a (_body params)
  "Preserve spaces in Mermaid's SVG labels, which librsvg otherwise collapses."
  (when-let* ((output (alist-get :file params))
              (file (expand-file-name output))
              ((equal (downcase (or (file-name-extension file) "")) "svg"))
              ((file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (unless (search-forward "xml:space=\"preserve\" class=\"text-inner-tspan\"" nil t)
        (goto-char (point-min))
        (while (search-forward "class=\"text-inner-tspan\"" nil t)
          (replace-match "xml:space=\"preserve\" class=\"text-inner-tspan\"" t t))
        (write-region (point-min) (point-max) file nil 'silent)))
    (clear-image-cache file)
    (when-let* ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (unless (buffer-modified-p)
          (revert-buffer t t t))))))
