;; -*- no-byte-compile: t; -*-
;;; bndos/git/packages.el

;; pr-review-worktree and pr-review-tree live in ./lisp,
;; diff-review in ./lisp/diff-review (see its VENDORED.org).

(package! pr-review)
(package! consult-gh)
(package! consult-gh-embark)
(package! consult-gh-forge)
(package! consult-gh-with-pr-review)
(package! code-review
  :recipe (:host github :repo "phelrine/code-review" :branch "fix/closql-update"
           ;; GraphQL queries live in a subdirectory.
           :files ("*.el" "graphql" "code-review-pkg.el"))
  :pin "97dae6fca12d49833dcbe865460021151520c10b")
(package! majutsu :recipe (:host github :repo "0WD0/majutsu"))
