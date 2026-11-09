#!/usr/bin/env bash
# Byte-compile the tasks/hutch/treediff libraries against Doom's packages.
# Output goes to a temporary directory; nothing is written next to sources.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
doom=$(cd "$here/../.." && pwd)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
files=(
  "$doom"/modules/bndos/git/lisp/magit-treediff.el
  "$doom"/modules/bndos/git/lisp/pr-review-treediff.el
  "$doom"/modules/bndos/git/lisp/hutch/*.el
  "$doom"/modules/bndos/tasks/lisp/*.el
)
"${EMACS:-$HOME/.local/bin/emacs}" -Q --batch -l "$here/load-path.el" --eval "
(let ((failed nil))
  (setq byte-compile-dest-file-function
        (lambda (f) (expand-file-name (concat (file-name-base f) \".elc\") \"$out\")))
  (dolist (f command-line-args-left)
    (unless (byte-compile-file f) (push f failed)))
  (setq command-line-args-left nil)
  (when failed (message \"FAILED: %s\" failed) (kill-emacs 1)))" "${files[@]}"
