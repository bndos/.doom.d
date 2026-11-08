;;; bndos/markdown/autoload/xwidget.el -*- lexical-binding: t; -*-

;;; Mermaid in markdown-xwidget previews

(defconst bndos-markdown-xwidget--mermaid-script
  "<script type=\"text/javascript\">
  document.addEventListener(\"DOMContentLoaded\", async () => {
    const selector = [
      \"pre.mermaid\",
      \"pre > code.language-mermaid\",
      \"pre > code.lang-mermaid\",
      \"pre > code.mermaid\",
      \"div.sourceCode.mermaid pre > code\"
    ].join(\", \");

    document.querySelectorAll(selector).forEach((node) => {
      const pre = node.tagName === \"PRE\" ? node : node.closest(\"pre\");
      if (!pre || pre.dataset.processed === \"true\") return;
      const code = pre.querySelector(\"code\");
      const source = code ? code.textContent : pre.textContent;
      pre.classList.add(\"mermaid\");
      pre.textContent = source;
    });

    if (window.mermaid) {
      try {
        await mermaid.run({ querySelector: \"pre.mermaid:not([data-processed=\\\"true\\\"])\" });
      } catch (error) {
        console.error(\"Mermaid render failed\", error);
      }
    }
  });
</script>"
  "Normalizes Mermaid blocks from common renderers, then runs Mermaid once.")

;;;###autoload
(defun bndos-markdown-xwidget-mermaid-a (fn mermaid-theme)
  "Render Mermaid blocks however pandoc/GFM marked them up."
  (concat (replace-regexp-in-string "startOnLoad: true," "startOnLoad: false,"
                                    (funcall fn mermaid-theme) t t)
          bndos-markdown-xwidget--mermaid-script))


;;; Search in xwidget-webkit buffers

(defvar-local bndos-xwidget--search-string "")

(defun bndos-xwidget--search (reverse)
  (setq bndos-xwidget--search-string
        (read-string (if reverse "Search backward: " "Search: ")
                     bndos-xwidget--search-string))
  (xwidget-webkit-search bndos-xwidget--search-string
                         (xwidget-webkit-current-session) t reverse t))

;;;###autoload
(defun bndos/xwidget-search-forward ()
  (interactive)
  (bndos-xwidget--search nil))

;;;###autoload
(defun bndos/xwidget-search-backward ()
  (interactive)
  (bndos-xwidget--search t))

;;;###autoload
(defun bndos/xwidget-search-next ()
  (interactive)
  (xwidget-webkit-next-result (xwidget-webkit-current-session)))

;;;###autoload
(defun bndos/xwidget-search-previous ()
  (interactive)
  (xwidget-webkit-previous-result (xwidget-webkit-current-session)))
