;;; bndos-outbox-tests.el --- Drafted replies to a pull request, sent from the outbox -*- lexical-binding: t; -*-

;; Against a fake gh (`bndos-test-gh-pr') that keeps a pull request in files
;; and records what is posted: what is sent is what was marked, once.

(require 'test-helper)
(require 'bndos-review-tests)
(require 'bndos-review-outbox)
(require 'bndos-changes-tests)          ; bndos-test-shown

(defconst bndos-test-gh-pr-script "#!/usr/bin/env python3
import json, os, sys, time
d = os.path.dirname(os.path.abspath(__file__))
def load(name, default):
    try:
        with open(os.path.join(d, name), encoding='utf-8') as f: return json.load(f)
    except (OSError, ValueError): return default
def save(name, value):
    with open(os.path.join(d, name), 'w', encoding='utf-8') as f: json.dump(value, f)
args = sys.argv[1:]
if args[0] == 'pr':
    print(json.dumps(load('pr.json', {}))); sys.exit(0)
if args[:2] == ['api', 'user']:
    print(json.dumps({'login': 'tester'})); sys.exit(0)
threads, conversation = load('threads.json', []), load('conversation.json', [])
def field(name):
    return next((a.split('=', 1)[1] for a in args if a.startswith(name + '=')), None)
def page(nodes, after):
    # Two comments a page, as GitHub's hundred: a long thread has more pages.
    start = int(after or 0)
    return {'nodes': nodes[start:start + 2],
            'pageInfo': {'hasNextPage': start + 2 < len(nodes), 'endCursor': str(start + 2)}}
if args[:2] == ['api', 'graphql']:
    query = field('query')
    if 'node(id:' in query:
        thread = next(t for t in threads if t['id'] == field('id'))
        print(json.dumps({'data': {'node': {'comments': page(thread['comments']['nodes'], field('after'))}}}))
        sys.exit(0)
    reviews = load('reviews.json', [])
    if 'reviewThreads' not in query:
        pr = {'comments': {'nodes': conversation}, 'reviews': {'nodes': reviews},
              'pending': {'totalCount': 1 if load('pending.json', False) else 0}}
    else:
        pr = {'reviewThreads': {'pageInfo': {'hasNextPage': False, 'endCursor': None},
                                'nodes': [dict(t, comments=page(t['comments']['nodes'], None)) for t in threads]},
              'comments': {'nodes': conversation}, 'reviews': {'nodes': reviews}}
    print(json.dumps({'data': {'repository': {'pullRequest': pr}}})); sys.exit(0)
def answer(status, reason, value, code):
    # As gh api --include prints it: status line, headers, a blank line, the body.
    sys.stdout.write('HTTP/2.0 %d %s\\nContent-Type: application/json\\r\\nX-Github-Media-Type: github.v3\\r\\n\\r\\n%s'
                     % (status, reason, json.dumps(value)))
    if code: sys.stderr.write('gh: %s (HTTP %d)\\n' % (reason, status))
    sys.exit(code)
if args[:4] == ['api', '--include', '-X', 'POST']:
    path = args[4]
    with open(args[args.index('--input') + 1], 'rb') as f: body = json.loads(f.read().decode('utf-8'))['body']
    modes = load('modes.json', [])
    mode = modes.pop(0) if modes else 'ok'
    save('modes.json', modes)
    if mode == 'slow': time.sleep(1.5)
    if mode == 'refused':
        answer(422, 'Unprocessable Entity', {'message': 'Validation Failed'}, 1)
    if mode == 'pending':
        answer(422, 'Unprocessable Entity',
               {'message': 'Validation Failed',
                'errors': [{'resource': 'PullRequestReview', 'code': 'custom', 'field': 'user_id',
                            'message': 'user_id can only have one pending review per pull request'}]}, 1)
    if mode == 'noanswer':
        sys.stderr.write('error connecting to api.github.com\\n'); sys.exit(1)
    if mode == 'killed':
        os.kill(os.getpid(), 9)
    posts = load('posts.json', [])
    n = len(posts) + 1
    posts.append({'path': path, 'body': body})
    save('posts.json', posts)
    node = {'databaseId': 1000 + n, 'url': 'https://github.com/me/repo/pull/7#c%d' % (1000 + n), 'body': body,
            'createdAt': '2026-11-19T12:00:%02dZ' % n, 'author': {'login': 'tester'}}
    if '/replies' in path:
        to = int(path.split('/comments/')[1].split('/')[0])
        for t in threads:
            if t['comments']['nodes'][0]['databaseId'] == to: t['comments']['nodes'].append(node)
        save('threads.json', threads)
    else:
        conversation.append(node); save('conversation.json', conversation)
    if mode == 'lost':
        answer(502, 'Bad Gateway', {'message': 'Server Error'}, 1)
    if mode == 'garbled':
        sys.stdout.write('HTTP/2.0 201 Created\\r\\n\\r\\n{\"id\": 10'); sys.exit(0)
    answer(201, 'Created', {'id': 1000 + n, 'html_url': node['url'],
                            'body': body if mode != 'untagged' else body.split('\\n\\n<!--')[0]}, 0)
sys.exit(2)
"
  "A gh keeping a pull request in files beside it: pr.json, threads.json,
conversation.json; posts go to posts.json, each answered as modes.json's
next mode says: ok, slow, refused and pending (422, nothing posted),
noanswer (nothing posted, no answer), killed (gh dies, nothing posted),
lost (posted, a 502), garbled (posted, a reply cut short), untagged
\(posted, the reply's body without its marker).")

(defun bndos-test-gh-pr (dir head)
  "Write the fake gh into DIR: pull request 7, one thread on src/a.txt:2 and
one comment on its conversation.  Return the gh."
  (let ((gh (expand-file-name "gh" dir)))
    (with-temp-file gh (insert bndos-test-gh-pr-script))
    (set-file-modes gh #o755)
    (bndos-test-gh-pr-write dir "pr.json"
                            (list :number 7 :url "https://github.com/me/repo/pull/7" :state "OPEN"
                                  :baseRefName "main" :baseRefOid head :headRefOid head))
    (bndos-test-gh-pr-write dir "threads.json"
                            (vector (list :id "T1" :isResolved :false :isOutdated :false :path "src/a.txt"
                                          :diffSide "RIGHT" :line 2 :originalLine 2 :originalStartLine :null
                                          :comments (list :nodes (vector (bndos-test-gh-node 501 "Why uppercase?" 1 head))))))
    (bndos-test-gh-pr-write dir "conversation.json"
                            (vector (list :id "IC1" :url "https://github.com/me/repo/pull/7#ic1" :body "Looks close."
                                          :createdAt "2026-11-12T09:00:00Z" :author (list :login "octo"))))
    gh))

(defun bndos-test-gh-node (id body minute commit)
  (list :databaseId id :url (format "https://github.com/me/repo/pull/7#c%d" id) :body body
        :createdAt (format "2026-11-12T10:%02d:00Z" minute) :author (list :login "octo")
        :originalCommit (list :oid commit)))

(defun bndos-test-gh-pr-write (dir name value)
  (with-temp-file (expand-file-name name dir) (insert (json-serialize value))))

(defun bndos-test-gh-pr-read (dir name)
  (let ((file (expand-file-name name dir)))
    (and (file-exists-p file)
         (with-temp-buffer (insert-file-contents file)
                           (json-parse-buffer :object-type 'plist :array-type 'list)))))

(defmacro bndos-test-with-pr (spec &rest body)
  "SPEC is (REVIEW STUBS WT): a task review whose pull request, #7, the fake
gh in STUBS serves, fetched."
  (declare (indent 1))
  `(bndos-test-with-two-files (key ,(nth 2 spec) ,(nth 0 spec))
     (ignore key)
     (let* ((,(nth 1 spec) (make-temp-file "bndos-gh-" t))
            (bndos-review-github-program (bndos-test-gh-pr ,(nth 1 spec) (bndos-test-git ,(nth 2 spec) "rev-parse" "HEAD")))
            (bndos-review-outbox--in-flight nil)
            (diff-review-overview-threads 'full))
       (unwind-protect
           (progn
             (bndos-test-fetch ,(nth 0 spec))
             ,@body)
         (delete-directory ,(nth 1 spec) t)))))

(defun bndos-test-fetch (review)
  "Fetch REVIEW's pull request and wait for it."
  (with-current-buffer review (setq bndos-review-github--state nil))
  (bndos-review-github-refresh review t)
  (should (bndos-proc-wait (lambda () (let ((state (buffer-local-value 'bndos-review-github--state review)))
                                        (and (plist-get state :number) (not (plist-get state :fetching)))))
                           30))
  (with-current-buffer review (bndos-review-outbox--redraw)))

(defun bndos-test-drafts (review &optional state)
  "REVIEW's drafts, in STATE (default all)."
  (seq-filter (lambda (d) (or (null state) (equal (plist-get d :state) state)))
              (bndos-review-outbox-load (bndos-review-outbox-store review))))

(defun bndos-test-draft-reply (review body)
  "Reply BODY to thread T1 from the overview, as r does."
  (with-current-buffer review
    (goto-char (point-min))
    (re-search-forward "Why uppercase\\?")
    (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) body)))
      (diff-review-reply))))

(defun bndos-test-mark-all ()
  "Mark all drafts of this outbox, drawn again first as a watch would."
  (bndos-review-outbox--render)
  (bndos-review-outbox-mark-all))

(defun bndos-test-outbox (review)
  (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
    (with-current-buffer review (bndos-review-outbox)))
  (let ((outbox (get-buffer "*outbox: me/repo#7*")))
    (should (bndos-proc-wait (lambda () (and (with-current-buffer outbox bndos-review-outbox--login)
                                             (not (plist-get (buffer-local-value 'bndos-review-github--state review)
                                                             :fetching))))
                             10))
    (with-current-buffer outbox (bndos-review-outbox--render))
    outbox))

(defun bndos-test-outbox-at (outbox regexp)
  "In OUTBOX, point on the draft whose text matches REGEXP."
  (with-current-buffer outbox
    (bndos-review-outbox--render)
    (goto-char (point-min))
    (should (re-search-forward regexp nil t))))

(defun bndos-test-send (outbox &optional confirm)
  "Send OUTBOX's marked drafts, confirming with CONFIRM (default yes), and wait."
  (with-current-buffer outbox
    (cl-letf (((symbol-function 'y-or-n-p) (or confirm (lambda (&rest _) t))))
      (bndos-review-outbox-send))
    (should (bndos-proc-wait (lambda () (not bndos-review-outbox--sending)) 15))
    ;; And the fetch a send ends with.
    (should (bndos-proc-wait (lambda () (not (plist-get (buffer-local-value 'bndos-review-github--state
                                                                            bndos-review-outbox--review)
                                                        :fetching)))
                             15))))

(defun bndos-test-posts (stubs) (bndos-test-gh-pr-read stubs "posts.json"))


;;; A reply: a draft, then sent once marked and confirmed

(ert-deftest bndos-outbox-a-reply-is-a-draft-until-sent ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "Fixed in the next commit.")
    (let ((draft (car (bndos-test-drafts review "drafts"))))
      (should (equal (plist-get (plist-get draft :target) :kind) "reply"))
      (should (equal (plist-get (plist-get draft :target) :comment) 501))
      (should (equal (plist-get draft :seen) "2026-11-12T10:01:00Z")))
    (should-not (bndos-test-posts stubs))
    ;; Shown in its thread, and counted.
    (with-current-buffer review
      (bndos-review-outbox--redraw)
      (let ((text (buffer-string)))
        (should (string-match-p "✎ 1 draft" text))
        (should (string-match-p "draft, not sent" text))
        (should (string-match-p "Fixed in the next commit\\." text))))
    (let ((outbox (bndos-test-outbox review)))
      (with-current-buffer outbox
        (should (string-match-p "posting as @tester" (buffer-string)))
        (should (string-match-p "Reply on src/a.txt:2  @octo: Why uppercase\\?" (buffer-string)))
        ;; Nothing marked, nothing sent.
        (should-error (bndos-review-outbox-send) :type 'user-error))
      (bndos-test-outbox-at outbox "Reply on")
      (with-current-buffer outbox (bndos-review-outbox-mark))
      ;; Declined: nothing sent, the mark kept.
      (bndos-test-send outbox (lambda (&rest _) nil))
      (should-not (bndos-test-posts stubs))
      (bndos-test-send outbox)
      (let ((posts (bndos-test-posts stubs)))
        (should (= (length posts) 1))
        (should (equal (plist-get (car posts) :path) "repos/me/repo/pulls/7/comments/501/replies"))
        (should (string-match-p "\\`Fixed in the next commit\\.\n\n<!-- bndos-draft:d[0-9]+-[0-9a-f]+ -->\\'"
                                (plist-get (car posts) :body))))
      (should (= (length (bndos-test-drafts review "sent")) 1))
      (should (string-match-p "1 sent" (with-current-buffer outbox bndos-review-outbox--report))))
    ;; Fetched again: GitHub's reply, without the marker, and no draft.
    (bndos-test-fetch review)
    (with-current-buffer review
      (let ((text (buffer-string)))
        (should (string-match-p "@tester" text))
        (should-not (string-match-p "bndos-draft" text))
        (should-not (string-match-p "draft, not sent" text))))))

(ert-deftest bndos-outbox-a-comment-on-the-conversation ()
  (bndos-test-with-pr (review stubs wt)
    ;; r on a comment of the conversation quotes it in a new comment.
    (with-current-buffer review
      (goto-char (point-min))
      (re-search-forward "Looks close\\.")
      (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_ initial) (concat initial "Thanks!"))))
        (diff-review-reply)))
    (should (equal (plist-get (car (bndos-test-drafts review "drafts")) :body) "> Looks close.\n\nThanks!"))
    (with-current-buffer review
      (bndos-review-outbox--redraw)
      (should (string-match-p "^ +Draft +conversation" (buffer-string))))
    (let ((outbox (bndos-test-outbox review)))
      (with-current-buffer outbox (bndos-test-mark-all))
      (bndos-test-send outbox))
    (should (equal (plist-get (car (bndos-test-posts stubs)) :path) "repos/me/repo/issues/7/comments"))))


(ert-deftest bndos-outbox-answers-a-whole-review-point-by-point ()
  (bndos-test-with-pr (review stubs wt)
    (let ((summary (list :id "PRR1" :url "https://github.com/me/repo/pull/7#prr1" :state "CHANGES_REQUESTED"
                         :body "Overall good.\n\n1. Rename foo.\n2. Add tests."
                         :submittedAt "2026-11-12T11:00:00Z" :author (list :login "octo"))))
      (bndos-test-gh-pr-write stubs "reviews.json" (vector summary))
      (bndos-test-fetch review)
      ;; r on the review's summary: all of it quoted, to answer under each point.
      (let (quoted)
        (with-current-buffer review
          (goto-char (point-min))
          (re-search-forward "Overall good\\.")
          ;; Only the quote: nothing to send.
          (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_ quote) (setq quoted quote) quote)))
            (should-error (diff-review-reply) :type 'user-error))
          (should (equal quoted "> Overall good.\n>\n> 1. Rename foo.\n> 2. Add tests.\n\n"))
          (cl-letf (((symbol-function 'read-string-from-buffer)
                     (lambda (_ _quote) "> 1. Rename foo.\n\nRenamed to bar.\n\n> 2. Add tests.\n\nAdded.")))
            (diff-review-reply))))
      (let ((draft (car (bndos-test-drafts review "drafts"))))
        (should (equal (plist-get draft :target) '(:kind "comment" :answers "@octo")))
        (should (equal (plist-get draft :seen) "2026-11-12T11:00:00Z")))
      (let ((outbox (bndos-test-outbox review)))
        (with-current-buffer outbox
          (should (string-match-p "Comment on the pull request, answering @octo" (buffer-string))))
        ;; Another review since: said before sending.
        (bndos-test-gh-pr-write stubs "reviews.json"
                                (vector summary (list :id "PRR2" :url "https://github.com/me/repo/pull/7#prr2"
                                                      :state "COMMENTED" :body "One more thing."
                                                      :submittedAt "2026-11-12T12:00:00Z" :author (list :login "octo"))))
        (bndos-test-fetch review)
        (with-current-buffer outbox
          (bndos-review-outbox--render)
          (should (string-match-p "1 new comment or review on the pull request since it was drafted" (buffer-string)))))
      ;; Agents see the reviews' summaries too, in the conversation's order.
      (let ((process-environment (cons (concat "BNDOS_TASK_GH=" bndos-review-github-program) process-environment))
            (default-directory wt))
        (with-temp-buffer
          (should (zerop (call-process bndos-task-cli nil t nil "drafts" "threads")))
          (let ((conversation (alist-get 'conversation (json-read-from-string (buffer-string)))))
            (should (equal (mapcar (lambda (m) (alist-get 'body m)) conversation)
                           '("Looks close." "Overall good.\n\n1. Rename foo.\n2. Add tests." "One more thing.")))
            (should (equal (alist-get 'review (aref conversation 1)) "changes_requested"))))))))

;;; What is sent is what was marked

(defun bndos-test-store-file (review state draft)
  (bndos-review-outbox--path (bndos-review-outbox-store review) state (plist-get draft :id)))

(defun bndos-test-rewrite (review draft body)
  "Change DRAFT's body behind the outbox's back, as an agent would."
  (let ((file (bndos-test-store-file review "drafts" draft)))
    (bndos-review-outbox--write file (plist-put (bndos-review-outbox--read file) :body body))))

(ert-deftest bndos-outbox-sends-only-drafts-as-they-were-marked ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "first")
    (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "second")))
      (bndos-review-outbox-comment review))
    (let* ((outbox (bndos-test-outbox review))
           (first (seq-find (lambda (d) (equal (plist-get d :body) "first")) (bndos-test-drafts review)))
           (second (seq-find (lambda (d) (equal (plist-get d :body) "second")) (bndos-test-drafts review))))
      (with-current-buffer outbox (bndos-test-mark-all))
      ;; Changed after it was marked: the outbox says so and sends nothing.
      (bndos-test-rewrite review first "first, changed")
      (with-current-buffer outbox
        (should-error (bndos-review-outbox-send) :type 'user-error)
        (should (string-match-p "changed since you marked it" (buffer-string))))
      (should-not (bndos-test-posts stubs))
      ;; Changed while the confirmation was up: that one isn't sent.
      (with-current-buffer outbox (bndos-test-mark-all))
      (bndos-test-send outbox (lambda (&rest _) (bndos-test-rewrite review second "second, changed") t))
      (let ((posts (bndos-test-posts stubs)))
        (should (= (length posts) 1))
        (should (string-prefix-p "first, changed\n" (plist-get (car posts) :body))))
      (should (string-match-p "1 not sent: changed since it was marked"
                              (with-current-buffer outbox bndos-review-outbox--report)))
      (should (equal (mapcar (lambda (d) (plist-get d :body)) (bndos-test-drafts review "drafts"))
                     '("second, changed")))
      ;; Deleted while the confirmation was up: not sent either.
      (with-current-buffer outbox (bndos-test-mark-all))
      (bndos-test-send outbox (lambda (&rest _) (delete-file (bndos-test-store-file review "drafts" second)) t))
      (should (= (length (bndos-test-posts stubs)) 1)))))

(ert-deftest bndos-outbox-a-draft-is-sent-once ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "once")
    (bndos-test-gh-pr-write stubs "modes.json" ["slow" "ok"])
    (let* ((draft (car (bndos-test-drafts review)))
           (marks (list (cons (plist-get draft :id) (bndos-review-outbox-hash draft))))
           (store (bndos-review-outbox-store review))
           results)
      ;; Two sends of the same draft at once, as from two Emacsen.
      (bndos-review-outbox-send-marks review store marks (lambda (r) (push r results)))
      (should (member (plist-get draft :id) bndos-review-outbox--in-flight))
      ;; Being sent: not changed, not deleted, not sent again.
      (should (stringp (bndos-review-outbox-change store (plist-get draft :id) (cdar marks) "late")))
      (should (stringp (bndos-review-outbox-remove store (plist-get draft :id) (cdar marks))))
      (bndos-review-outbox-send-marks review store marks (lambda (r) (push r results)))
      (should (bndos-proc-wait (lambda () (= (length results) 2)) 15))
      (should (= (length (bndos-test-posts stubs)) 1))
      (should (equal (sort (mapcar (lambda (r) (symbol-name (cadr (car r)))) results) #'string<)
                     '("sent" "skipped"))))))

(ert-deftest bndos-outbox-an-edit-holds-the-draft-from-a-send ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "held")
    (let* ((draft (car (bndos-test-drafts review)))
           (store (bndos-review-outbox-store review))
           (held (bndos-review-outbox--hold store (plist-get draft :id) (bndos-review-outbox-hash draft)))
           results)
      (should (file-name-absolute-p held))
      ;; Not among the drafts while held, so not sent.
      (bndos-review-outbox-send-marks review store (list (cons (plist-get draft :id) (bndos-review-outbox-hash draft)))
                                      (lambda (r) (setq results r)))
      (should (equal (cdr (car results)) '(skipped "sent, deleted or being changed since it was marked")))
      (should-not (bndos-test-posts stubs))
      ;; Held by a process that died: back among the drafts.
      (rename-file held (expand-file-name (format "drafts/.%s.held-999999" (plist-get draft :id)) store))
      ;; Not at once: its holder may be one whose pid isn't seen from here.
      (should-not (bndos-test-drafts review "drafts"))
      (let ((bndos-review-outbox--hold-grace 0))
        (should (= (length (bndos-test-drafts review "drafts")) 1))))))


;;; Failures

(ert-deftest bndos-outbox-a-refused-send-stays-a-draft ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "refused")
    (bndos-test-gh-pr-write stubs "modes.json" ["refused"])
    (let ((outbox (bndos-test-outbox review)))
      (with-current-buffer outbox (bndos-test-mark-all))
      (bndos-test-send outbox)
      (let ((draft (car (bndos-test-drafts review "drafts"))))
        (should (string-match-p "HTTP 422" (plist-get draft :error)))
        (should-not (plist-get draft :claim)))
      (should (string-match-p "1 failed, kept as drafts" (with-current-buffer outbox bndos-review-outbox--report)))
      (with-current-buffer outbox
        (bndos-review-outbox--render)
        (should (string-match-p "the last send failed: .*HTTP 422" (buffer-string))))
      ;; Sent again once the user says so.
      (with-current-buffer outbox (bndos-test-mark-all))
      (bndos-test-send outbox)
      (should (= (length (bndos-test-posts stubs)) 1))
      (should (bndos-test-drafts review "sent")))))

;; By the HTTP status GitHub answers with, never by gh's wording.
(ert-deftest bndos-outbox-goes-by-the-status-github-answers ()
  (bndos-test-with-pr (review stubs wt)
    (let ((outbox (bndos-test-outbox review))
          (send (lambda (body mode)
                  (bndos-test-draft-reply review body)
                  (bndos-test-gh-pr-write stubs "modes.json" (vector mode))
                  (let* ((draft (seq-find (lambda (d) (equal (plist-get d :body) body))
                                          (bndos-test-drafts review "drafts")))
                         results)
                    (bndos-review-outbox-send-marks review (bndos-review-outbox-store review)
                                                    (list (cons (plist-get draft :id) (bndos-review-outbox-hash draft)))
                                                    (lambda (r) (setq results r)))
                    (should (bndos-proc-wait (lambda () results) 15))
                    (cdr (car results)))))
          (warnings nil))
      (ignore outbox)
      ;; A pending review of mine on GitHub: refused, and said why.
      (let ((outcome (funcall send "while pending" "pending")))
        (should (eq (car outcome) 'failed))
        (should (string-match-p "pending review" (cadr outcome))))
      (should (string-match-p "pending review on this pull request"
                              (plist-get (seq-find (lambda (d) (equal (plist-get d :body) "while pending"))
                                                   (bndos-test-drafts review "drafts"))
                                         :error)))
      (bndos-review-outbox-remove (bndos-review-outbox-store review)
                                  (plist-get (car (bndos-test-drafts review "drafts")) :id)
                                  (bndos-review-outbox-hash (car (bndos-test-drafts review "drafts"))))
      ;; gh killed before an answer, a reply cut short: unconfirmed, both.
      (should (eq (car (funcall send "killed" "killed")) 'unknown))
      (should (eq (car (funcall send "garbled" "garbled")) 'unknown))
      (should (= (length (bndos-test-drafts review "sending")) 2))
      ;; Settled by what GitHub has: the cut-short one arrived, the other not.
      (bndos-test-fetch review)
      (should (equal (mapcar (lambda (d) (plist-get d :body)) (bndos-test-drafts review "sent")) '("garbled")))
      (should (equal (mapcar (lambda (d) (plist-get d :body)) (bndos-test-drafts review "sending")) '("killed")))
      ;; Sent, its marker dropped by GitHub: sent, and said so.
      (cl-letf (((symbol-function 'display-warning) (lambda (_type message &rest _) (push message warnings))))
        (should (eq (car (funcall send "untagged" "untagged")) 'sent)))
      (should (string-match-p "hidden tag" (car warnings)))
      (should (= (length (bndos-test-posts stubs)) 2)))))

(ert-deftest bndos-outbox-an-unknown-outcome-is-never-sent-again-by-itself ()
  (bndos-test-with-pr (review stubs wt)
    ;; Posted, its answer lost: unconfirmed, then settled by the marker.
    (bndos-test-draft-reply review "lost answer")
    (bndos-test-gh-pr-write stubs "modes.json" ["lost"])
    (let ((outbox (bndos-test-outbox review))
          (draft (car (bndos-test-drafts review)))
          results)
      ;; Sent without the fetch the outbox ends a send with, to see it unsettled.
      (bndos-review-outbox-send-marks review (bndos-review-outbox-store review)
                                      (list (cons (plist-get draft :id) (bndos-review-outbox-hash draft)))
                                      (lambda (r) (setq results r)))
      (should (bndos-proc-wait (lambda () results) 15))
      (should (equal (car (cdr (car results))) 'unknown))
      (should (bndos-review-outbox-unconfirmed-p (car (bndos-test-drafts review "sending"))))
      (with-current-buffer outbox
        (bndos-review-outbox--render)
        (should (string-match-p "unconfirmed (HTTP 502)" (buffer-string)))
        ;; Not a draft: mark all leaves it, R is for it.
        (bndos-test-mark-all)
        (should-not bndos-review-outbox--marks))
      (bndos-test-fetch review)
      (should (= (length (bndos-test-drafts review "sent")) 1))
      (should (plist-get (plist-get (car (bndos-test-drafts review "sent")) :sent) :found))
      (should (= (length (bndos-test-posts stubs)) 1))
      ;; Never posted, no answer: unconfirmed until the user puts it back.
      (bndos-test-draft-reply review "no answer")
      (bndos-test-gh-pr-write stubs "modes.json" ["noanswer"])
      (with-current-buffer outbox (bndos-test-mark-all))
      (bndos-test-send outbox)
      (should (string-match-p "1 unconfirmed: check GitHub" (with-current-buffer outbox bndos-review-outbox--report)))
      (bndos-test-fetch review)
      (should (= (length (bndos-test-drafts review "sending")) 1))
      (bndos-test-outbox-at outbox "no answer")
      (with-current-buffer outbox
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (bndos-review-outbox-return)))
      (should (equal (mapcar (lambda (d) (plist-get d :body)) (bndos-test-drafts review "drafts")) '("no answer")))
      (should (= (length (bndos-test-posts stubs)) 1)))))


;;; What can go wrong around a send

(ert-deftest bndos-outbox-sends-any-text-as-written ()
  (bndos-test-with-pr (review stubs wt)
    (let ((body "Ça marche — merci 😀\n\n```\nλ x → x\n```"))
      ;; No question of a coding system: in batch one would be an error.
      (bndos-test-draft-reply review body)
      (should (equal (plist-get (car (bndos-test-drafts review "drafts")) :body) body))
      (let ((outbox (bndos-test-outbox review)))
        (with-current-buffer outbox (bndos-test-mark-all))
        (bndos-test-send outbox)
        (should (string-match-p "1 sent" (with-current-buffer outbox bndos-review-outbox--report))))
      (should (string-prefix-p (concat body "\n\n<!-- bndos-draft:")
                               (plist-get (car (bndos-test-posts stubs)) :body)))
      (should (equal (plist-get (car (bndos-test-drafts review "sent")) :body) body)))
    ;; An agent's, from a shell without a UTF-8 locale.
    (let ((process-environment (append (list (concat "BNDOS_TASK_GH=" bndos-review-github-program) "LC_ALL=C" "LANG=C")
                                       process-environment))
          (default-directory wt))
      (should (zerop (call-process bndos-task-cli nil nil nil "drafts" "add" "--conversation"
                                   "--body" "Déjà vu ✓" "--author" "worker"))))
    (should (member "Déjà vu ✓" (mapcar (lambda (d) (plist-get d :body)) (bndos-test-drafts review "drafts"))))))

(ert-deftest bndos-outbox-a-send-that-cant-start-leaves-nothing-stuck ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "no gh")
    (let ((outbox (bndos-test-outbox review)))
      (with-current-buffer outbox (bndos-test-mark-all))
      (let ((bndos-review-github-program (expand-file-name "missing-gh" stubs)))
        (with-current-buffer outbox
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (bndos-review-outbox-send))
          (should (bndos-proc-wait (lambda () (not bndos-review-outbox--sending)) 15))))
      (should-not bndos-review-outbox--in-flight)
      (let ((draft (car (bndos-test-drafts review "drafts"))))
        (should (equal (plist-get draft :body) "no gh"))
        (should (string-match-p "not sent" (plist-get draft :error))))
      (should (string-match-p "1 failed" (with-current-buffer outbox bndos-review-outbox--report)))
      ;; And the outbox sends again.
      (bndos-test-fetch review)
      (with-current-buffer outbox (bndos-test-mark-all))
      (bndos-test-send outbox)
      (should (= (length (bndos-test-posts stubs)) 1)))))

(ert-deftest bndos-outbox-a-send-of-another-emacs-is-left-to-it ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "theirs")
    (let* ((store (bndos-review-outbox-store review))
           (draft (car (bndos-test-drafts review)))
           (id (plist-get draft :id))
           (other (start-process "other-emacs" nil (expand-file-name invocation-name invocation-directory)
                                 "-Q" "--batch" "--eval" "(sleep-for 60)"))
           (outbox (bndos-test-outbox review)))
      (unwind-protect
          (progn
            ;; Claimed by the other Emacs, posting now.
            (rename-file (bndos-review-outbox--path store "drafts" id) (bndos-review-outbox--path store "sending" id))
            (bndos-review-outbox--write (bndos-review-outbox--path store "sending" id)
                                        (plist-put (bndos-review-outbox--clean draft) :claim
                                                   (list :pid (process-id other) :host (system-name) :at "now")))
            (let ((sending (car (bndos-test-drafts review "sending"))))
              (should-not (bndos-review-outbox-unconfirmed-p sending)))
            (bndos-test-outbox-at outbox "theirs")
            (with-current-buffer outbox
              (should (string-match-p "sending…" (buffer-string)))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (should-error (bndos-review-outbox-return) :type 'user-error)))
            ;; Its post arrives: a fetch leaves it to the Emacs sending it.
            (bndos-review-outbox-reconcile review (list (list id "https://x" "T1")))
            (should (bndos-test-drafts review "sending"))
            ;; Claimed long ago, that Emacs still running: R, asked harder.
            (bndos-review-outbox--write (bndos-review-outbox--path store "sending" id)
                                        (plist-put (bndos-review-outbox--clean draft) :claim
                                                   (list :pid (process-id other) :host (system-name)
                                                         :at "2026-01-01T00:00:00Z")))
            (bndos-test-outbox-at outbox "theirs")
            (with-current-buffer outbox
              (let (question)
                (cl-letf (((symbol-function 'yes-or-no-p) (lambda (q) (setq question q) nil)))
                  (bndos-review-outbox-return))
                (should (string-match-p "still runs" question))))
            (should (bndos-test-drafts review "sending"))
            ;; That Emacs gone: unconfirmed, and settled by what GitHub has.
            (delete-process other)
            (should (bndos-proc-wait (lambda () (not (process-attributes (process-id other)))) 10))
            (should (bndos-review-outbox-unconfirmed-p (car (bndos-test-drafts review "sending"))))
            (bndos-review-outbox-reconcile review (list (list id "https://x" "T1")))
            (should (bndos-test-drafts review "sent")))
        (when (process-live-p other) (delete-process other))))))

(ert-deftest bndos-outbox-marks-what-is-shown ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "as shown")
    (let ((outbox (bndos-test-outbox review))
          (draft (car (bndos-test-drafts review))))
      (bndos-test-outbox-at outbox "as shown")
      ;; Changed by an agent after it was drawn, before the redraw.
      (bndos-review-outbox-change (bndos-review-outbox-store review) (plist-get draft :id)
                                  (bndos-review-outbox-hash draft) "not as shown")
      (with-current-buffer outbox
        (goto-char (point-min))
        (re-search-forward "as shown")
        (let ((inhibit-redisplay t))
          (should-error (bndos-review-outbox-mark) :type 'user-error))
        (should-not bndos-review-outbox--marks)
        ;; Drawn again, as it is now: m marks that.
        (goto-char (point-min))
        (re-search-forward "not as shown")
        (bndos-review-outbox-mark)
        (should (equal (cdr (car bndos-review-outbox--marks))
                       (bndos-review-outbox-hash (car (bndos-test-drafts review))))))
      ;; M leaves one changed or new since drawn.
      (with-current-buffer outbox (bndos-review-outbox-unmark-all))
      (bndos-test-draft-reply review "new, not shown")
      (with-current-buffer outbox
        ;; r on a thread with my draft edits it: the new text is the same draft's.
        (bndos-review-outbox-mark-all)
        (should-not bndos-review-outbox--marks)
        (bndos-review-outbox-mark-all)
        (should (= (length bndos-review-outbox--marks) 1))))))

(ert-deftest bndos-outbox-a-draft-goes-to-its-own-pull-request ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "for #7")
    (let ((outbox (bndos-test-outbox review))
          (draft (car (bndos-test-drafts review)))
          results)
      ;; The review shows another pull request now.
      (with-current-buffer review
        (setq bndos-review-github--state (plist-put (copy-sequence bndos-review-github--state) :number 8)))
      (bndos-review-outbox-send-marks review (with-current-buffer outbox bndos-review-outbox--store)
                                      (list (cons (plist-get draft :id) (bndos-review-outbox-hash draft)))
                                      (lambda (r) (setq results r)))
      (should (equal (cdr (car results)) '(skipped "the review shows another pull request now")))
      (should-not (bndos-test-posts stubs))
      (should (equal (bndos-review-outbox--endpoint draft) "repos/me/repo/pulls/7/comments/501/replies")))))

(ert-deftest bndos-outbox-a-marker-counts-only-where-its-draft-was-going ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "a reply")
    (let ((id (plist-get (car (bndos-test-drafts review)) :id)))
      ;; Its marker, copied into the conversation: not the reply.
      (bndos-review-outbox-reconcile review (list (list id "https://x" 'conversation)))
      (should (bndos-test-drafts review "drafts"))
      (bndos-review-outbox-reconcile review (list (list id "https://x" "T9")))
      (should (bndos-test-drafts review "drafts"))
      (bndos-review-outbox-reconcile review (list (list id "https://x" "T1")))
      (should (bndos-test-drafts review "sent")))))

(ert-deftest bndos-outbox-a-pending-review-holds-replies ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-gh-pr-write stubs "pending.json" t)
    (bndos-test-fetch review)
    (bndos-test-draft-reply review "while pending")
    (let ((outbox (bndos-test-outbox review)))
      (bndos-test-outbox-at outbox "while pending")
      (with-current-buffer outbox
        (should (string-match-p "can't send: you have a pending review" (buffer-string)))
        (should-error (bndos-review-outbox-mark) :type 'user-error))
      ;; Submitted on GitHub: sent.
      (bndos-test-gh-pr-write stubs "pending.json" :false)
      (bndos-test-fetch review)
      (with-current-buffer outbox (bndos-test-mark-all))
      (bndos-test-send outbox)
      (should (= (length (bndos-test-posts stubs)) 1)))))

(ert-deftest bndos-outbox-reads-long-threads-to-their-end ()
  (bndos-test-with-pr (review stubs wt)
    (let ((head (bndos-test-git wt "rev-parse" "HEAD")))
      ;; Five messages: three pages of the fake's two.
      (bndos-test-gh-pr-write stubs "threads.json"
                              (vector (list :id "T1" :isResolved :false :isOutdated :false :path "src/a.txt"
                                            :diffSide "RIGHT" :line 2 :originalLine 2 :originalStartLine :null
                                            :comments (list :nodes (vector (bndos-test-gh-node 501 "Why uppercase?" 1 head)
                                                                           (bndos-test-gh-node 502 "two" 2 head)
                                                                           (bndos-test-gh-node 503 "three" 3 head)
                                                                           (bndos-test-gh-node 504 "four" 4 head)
                                                                           (bndos-test-gh-node 505 "the fifth" 5 head)))))))
    (bndos-test-fetch review)
    (with-current-buffer review (should (string-match-p "the fifth" (buffer-string))))
    (bndos-test-draft-reply review "sixth")
    (should (equal (plist-get (car (bndos-test-drafts review)) :seen) "2026-11-12T10:05:00Z"))
    ;; Sent, on the third page: found there.
    (let ((outbox (bndos-test-outbox review)))
      (with-current-buffer outbox (bndos-test-mark-all))
      (bndos-test-send outbox))
    (bndos-test-fetch review)
    (should (bndos-test-drafts review "sent"))
    (should-not (bndos-test-drafts review "sending"))
    ;; The CLI reads them all too.
    (let ((process-environment (cons (concat "BNDOS_TASK_GH=" bndos-review-github-program) process-environment))
          (default-directory wt))
      (with-temp-buffer
        (should (zerop (call-process bndos-task-cli nil t nil "drafts" "threads")))
        (should (string-match-p "the fifth" (buffer-string)))
        (should (string-match-p "sixth" (buffer-string)))))))

;;; Changes a reply links

(defun bndos-test-gh-cli (dir &rest args)
  "Run bndos-task ARGS in DIR against the fake gh: (EXIT . JSON-or-text)."
  (let ((process-environment (cons (concat "BNDOS_TASK_GH=" bndos-review-github-program) process-environment))
        (default-directory dir))
    (with-temp-buffer
      (let ((exit (apply #'call-process bndos-task-cli nil '(t nil) nil args)))
        (cons exit (or (ignore-errors (json-parse-string (buffer-string) :object-type 'plist :array-type 'list
                                                         :null-object nil :false-object nil))
                       (buffer-string)))))))

(defun bndos-test-push (stubs wt)
  "Say GitHub has WT's HEAD as the pull request's head."
  (let ((pr (bndos-test-gh-pr-read stubs "pr.json")))
    (bndos-test-gh-pr-write stubs "pr.json" (plist-put pr :headRefOid (bndos-test-git wt "rev-parse" "HEAD")))))

(defun bndos-test-fix (stubs wt review)
  "The agent's fix of thread T1 (src/a.txt:2) and a test, in one commit, pushed."
  (bndos-test-commit wt "address review" "src/a.txt" "one\ntwo\nfour\n" "tests/a_test.txt" "checks two\n")
  (bndos-test-push stubs wt)
  (bndos-test-fetch review))

(ert-deftest bndos-outbox-a-reply-links-the-changes-that-answer-it ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-fix stubs wt review)
    ;; The hunks, from the commit T1 was written on to HEAD, from git.
    (let* ((hunks (bndos-test-gh-cli wt "drafts" "hunks" "--thread" "T1"))
           (refs (mapcar (lambda (h) (plist-get h :ref)) (plist-get (cdr hunks) :hunks)))
           (ref (seq-find (lambda (r) (string-match-p "@src/a.txt:" r)) refs)))
      (should (zerop (car hunks)))
      (should (plist-get (cdr hunks) :pushed))
      (should (= (length refs) 2))
      ;; An invented one is refused; nothing drafted.
      (should-not (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--thread" "T1" "--body" "Done."
                                              "--hunks" "deadbeefdeadbeef@src/a.txt:2" "--author" "worker"))))
      (should-not (bndos-test-drafts review))
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--thread" "T1" "--body" "Lowercased it."
                                          "--hunks" ref "--author" "worker"))))
      (let* ((draft (car (bndos-test-drafts review)))
             (listed (car (plist-get (cdr (bndos-test-gh-cli wt "drafts" "list")) :drafts))))
        (should (= (length (plist-get (plist-get draft :changes) :links)) 1))
        ;; Emacs and the CLI hash a draft and its links alike.
        (should (equal (plist-get listed :hash) (bndos-review-outbox-hash draft)))))
    ;; Under its thread, one line; in the outbox, a line and nothing blocking.
    (bndos-review-outbox-changed (bndos-review-outbox-store review))   ; as the watch does
    (with-current-buffer review
      (bndos-review-outbox--redraw)
      (should (string-match-p "draft, not sent · 1 change in 1 file" (buffer-string))))
    (let ((outbox (bndos-test-outbox review)))
      (bndos-test-outbox-at outbox "Lowercased it")
      (with-current-buffer outbox
        (should (string-match-p "Changes: 1 change in 1 file · v to review them" (buffer-string)))
        (should-not (string-match-p "can't send" (buffer-string)))
        ;; v: a review of the changes alone, of the worktree as checked out.
        (bndos-review-outbox-view-changes))
      (let ((view (find-buffer-visiting (expand-file-name "changes-review.json" (bndos-review-outbox-store review)))))
        (with-current-buffer view
          (should (derived-mode-p 'diff-review-mode))
          (should (equal (bndos-test-changes-files) '("src/a.txt")))
          (should (equal (bndos-test-changes-removed "src/a.txt") '("TWO")))
          (let ((text (buffer-string)))
            (should (string-match-p "Reply on src/a.txt:2, by agent:worker, not sent" text))
            ;; Its thread and the draft, no other.
            (should (string-match-p "Lowercased it\\." text))
            (should (string-match-p "Why uppercase\\?" text))
            (should (= (length diff-review-external-comments) 1))))
        ;; The pull request's review is its own still.
        (with-current-buffer review
          (should-not bndos-review-changes-scope))
        ;; u: what no draft explains, the test here, in the same review.
        (with-current-buffer outbox (bndos-review-outbox-unexplained))
        (should (eq (find-buffer-visiting (expand-file-name "changes-review.json" (bndos-review-outbox-store review))) view))
        (with-current-buffer view
          (should (string-match-p "Changes no draft explains" (buffer-string)))
          (should (equal (bndos-test-changes-files) '("tests/a_test.txt")))
          (should-not diff-review-external-comments))
        (kill-buffer view)))))

(defun bndos-test-changes-files ()
  "The paths this review lists."
  (mapcar (lambda (f) (plist-get f :path)) diff-review--files))

(defun bndos-test-changes-removed (path)
  "The lines this review marks removed in PATH."
  (cl-loop for h in (diff-review--parse-hunks (seq-find (lambda (f) (equal (plist-get f :path) path)) diff-review--files))
           append (plist-get h :removed)))

(ert-deftest bndos-outbox-a-reply-with-changes-is-sent-and-keeps-them ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-fix stubs wt review)
    (let ((refs (mapcar (lambda (h) (plist-get h :ref))
                        (plist-get (cdr (bndos-test-gh-cli wt "drafts" "hunks" "--thread" "T1")) :hunks))))
      ;; Linked by ref and by rule: arrays and all, as the CLI writes them.
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--thread" "T1" "--body" "Lowercased; tested."
                                             "--hunks" (car refs) "--hunks-in" "tests/" "--author" "worker")))))
    (let ((outbox (bndos-test-outbox review))
          (before (plist-get (car (bndos-test-drafts review)) :changes)))
      (with-current-buffer outbox (bndos-test-mark-all))
      (bndos-test-send outbox)
      (should (string-match-p "1 sent" (with-current-buffer outbox bndos-review-outbox--report)))
      (should (= (length (bndos-test-posts stubs)) 1))
      ;; Sent, its changes as they were: Emacs rewrote the file on the way.
      (let ((sent (car (bndos-test-drafts review "sent"))))
        (should (equal (plist-get sent :changes) before))
        (should (equal (plist-get (car (plist-get (plist-get sent :changes) :rules)) :in) '("tests/"))))
      ;; And the CLI still reads it.
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "list")))))))

(ert-deftest bndos-outbox-changes-claimed-must-be-on-github ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-fix stubs wt review)
    (let ((ref (plist-get (car (plist-get (cdr (bndos-test-gh-cli wt "drafts" "hunks" "--thread" "T1")) :hunks)) :ref)))
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--thread" "T1" "--body" "Done."
                                          "--hunks" ref "--author" "worker")))))
    (let ((outbox (bndos-test-outbox review)))
      ;; A commit here, not pushed: the reply can't go.
      (bndos-test-commit wt "more" "b.txt" "b, more\n")
      (bndos-test-outbox-at outbox "Done")
      (with-current-buffer outbox
        (should (string-match-p "can't send: the branch here isn't the pull request's head on GitHub" (buffer-string)))
        (should-error (bndos-review-outbox-mark) :type 'user-error))
      ;; Pushed: it can; an edit not committed is warned of.
      (bndos-test-push stubs wt)
      (bndos-test-fetch review)
      (bndos-test-write wt "b.txt" "edited\n")
      (let ((bndos-review-changes-cache-seconds 0))
        (bndos-test-outbox-at outbox "Done")
        (with-current-buffer outbox
          (should-not (string-match-p "can't send" (buffer-string)))
          (should (string-match-p "uncommitted changes in the worktree" (buffer-string))))
        ;; The fix undone: said before sending.
        (bndos-test-git wt "checkout" "-q" "--" "b.txt")
        (bndos-test-commit wt "undo" "src/a.txt" "one\nTWO\nfour\n")
        (bndos-test-push stubs wt)
        (bndos-test-fetch review)
        (bndos-test-outbox-at outbox "Done")
        (with-current-buffer outbox
          (should (string-match-p "1 linked change gone: the code is back as the reviewer saw it" (buffer-string))))))))

(ert-deftest bndos-outbox-relinking-changes-a-marked-draft ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-fix stubs wt review)
    (let ((ref (seq-find (lambda (r) (string-match-p "@src/a.txt:" r))
                         (mapcar (lambda (h) (plist-get h :ref))
                                 (plist-get (cdr (bndos-test-gh-cli wt "drafts" "hunks" "--thread" "T1")) :hunks)))))
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--thread" "T1" "--body" "Done."
                                          "--hunks" ref "--author" "worker")))))
    (let ((outbox (bndos-test-outbox review))
          (id (plist-get (car (bndos-test-drafts review)) :id)))
      (with-current-buffer outbox (bndos-test-mark-all))
      (should (with-current-buffer outbox bndos-review-outbox--marks))
      ;; The agent links other changes, the text the same: no longer as marked.
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "edit" id "--hunks-in" "tests/"))))
      (should (equal (plist-get (car (bndos-test-drafts review)) :body) "Done."))
      (with-current-buffer outbox
        (should (string-match-p "changed or went since you marked them"
                                (cadr (should-error (bndos-review-outbox-send) :type 'user-error)))))
      (should-not (bndos-test-posts stubs)))))

;;; Editing, deleting, and what to know before sending

(ert-deftest bndos-outbox-edits-and-deletions-are-of-the-draft-as-seen ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "v1")
    (let ((outbox (bndos-test-outbox review))
          (draft (car (bndos-test-drafts review))))
      ;; r on a thread with my draft edits it.
      (bndos-test-draft-reply review "v2")
      (should (equal (mapcar (lambda (d) (plist-get d :body)) (bndos-test-drafts review)) '("v2")))
      ;; Changed by someone else while it was being edited: not saved, the text kept.
      (bndos-test-outbox-at outbox "v2")
      (with-current-buffer outbox
        (cl-letf (((symbol-function 'read-string-from-buffer)
                   (lambda (&rest _) (bndos-test-rewrite review draft "theirs") "mine")))
          (should-error (bndos-review-outbox-edit) :type 'user-error)))
      (should (equal (current-kill 0) "mine"))
      (should (equal (plist-get (car (bndos-test-drafts review)) :body) "theirs"))
      ;; Deleted only as shown.
      (bndos-test-outbox-at outbox "theirs")
      (with-current-buffer outbox
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (bndos-test-rewrite review draft "again") t)))
          (should-error (bndos-review-outbox-delete) :type 'user-error)))
      (should (bndos-test-drafts review))
      (bndos-test-outbox-at outbox "again")
      (with-current-buffer outbox
        (cl-letf (((symbol-function 'y-or-n-p) #'always))
          (bndos-review-outbox-delete)))
      (should-not (bndos-test-drafts review)))))

(ert-deftest bndos-outbox-says-what-changed-since-a-draft-was-written ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "my answer")
    (let ((outbox (bndos-test-outbox review))
          (head (bndos-test-git wt "rev-parse" "HEAD")))
      ;; A new message in the thread, which is resolved meanwhile.
      (bndos-test-gh-pr-write stubs "threads.json"
                              (vector (list :id "T1" :isResolved t :isOutdated :false :path "src/a.txt"
                                            :diffSide "RIGHT" :line 2 :originalLine 2 :originalStartLine :null
                                            :comments (list :nodes (vector (bndos-test-gh-node 501 "Why uppercase?" 1 head)
                                                                           (bndos-test-gh-node 502 "Also this." 30 head))))))
      (bndos-test-fetch review)
      (with-current-buffer outbox
        (bndos-review-outbox--render)
        (should (string-match-p "1 new message in the thread since it was drafted" (buffer-string)))
        (should (string-match-p "the thread is resolved on GitHub" (buffer-string))))
      ;; The thread gone: it can't be marked.
      (bndos-test-gh-pr-write stubs "threads.json" [])
      (bndos-test-fetch review)
      (bndos-test-outbox-at outbox "my answer")
      (with-current-buffer outbox
        (should (string-match-p "can't send: its thread isn't on the pull request" (buffer-string)))
        (should-error (bndos-review-outbox-mark) :type 'user-error)))))

(ert-deftest bndos-outbox-fetches-again-before-sending-on-old-threads ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "later")
    (let ((outbox (bndos-test-outbox review)))
      (with-current-buffer outbox (bndos-test-mark-all))
      (with-current-buffer review
        (setq bndos-review-github--state
              (plist-put bndos-review-github--state :at (time-subtract nil 3600))))
      (let ((asked nil))
        (bndos-test-send outbox (lambda (&rest _) (setq asked t)))
        (should-not asked))
      (should (bndos-proc-wait (lambda () (< (bndos-review-outbox--age review) 60)) 10))
      (should-not (bndos-test-posts stubs)))))

(ert-deftest bndos-outbox-shows-agents-drafts-for-checking ()
  (bndos-test-with-pr (review stubs wt)
    (let ((process-environment (cons (concat "BNDOS_TASK_GH=" bndos-review-github-program) process-environment)))
      (let ((default-directory wt))
        (should (zerop (call-process bndos-task-cli nil nil nil "drafts" "add" "--thread" "T1"
                                     "--body" "Drafted for you." "--author" "codex")))))
    ;; What the store's watch does when it is written (batch Emacs gets no file
    ;; events: the GUI tests have the watch itself).
    (bndos-review-outbox-changed (bndos-review-outbox-store review))
    (should (bndos-proc-wait (lambda () (with-current-buffer review (string-match-p "✎ agent:codex" (buffer-string))))
                             5))
    (let ((outbox (bndos-test-outbox review)))
      (with-current-buffer outbox
        (should (string-match-p "drafted by agent:codex: read it before sending" (buffer-string)))))))


;;; The command agents draft with

(defun bndos-test-drafts-cli (wt gh &rest args)
  "Run `bndos-task drafts ARGS' in WT with GH: (EXIT . JSON)."
  (let ((process-environment (cons (concat "BNDOS_TASK_GH=" gh) process-environment))
        (default-directory wt))
    (with-temp-buffer
      (let ((exit (apply #'call-process bndos-task-cli nil '(t nil) nil "drafts" args)))
        (cons exit (ignore-errors (json-parse-string (buffer-string) :object-type 'plist :array-type 'list
                                                     :null-object nil :false-object nil)))))))

(ert-deftest bndos-cli-drafts-are-agents-and-never-sent ()
  (bndos-test-with-pr (review stubs wt)
    (let ((gh bndos-review-github-program))
      ;; Threads, with the ids to reply to.
      (let ((threads (cdr (bndos-test-drafts-cli wt gh "threads"))))
        (should (equal (mapcar (lambda (th) (plist-get th :id)) (plist-get threads :threads)) '("T1")))
        (should (equal (plist-get (car (plist-get threads :conversation)) :body) "Looks close.")))
      ;; A reply: an agent's draft, where Emacs keeps them.
      (let* ((added (cdr (bndos-test-drafts-cli wt gh "add" "--thread" "T1" "--body" "On it." "--author" "claude")))
             (id (plist-get (plist-get added :draft) :id)))
        (should (equal (plist-get (plist-get added :draft) :author) "agent:claude"))
        (let ((draft (car (bndos-test-drafts review "drafts"))))
          (should (equal (plist-get draft :id) id))
          (should (equal (plist-get (plist-get draft :target) :comment) 501)))
        ;; Edited and deleted only while it is an agent's draft.
        (should (zerop (car (bndos-test-drafts-cli wt gh "edit" id "--body" "On it, done."))))
        (should (equal (plist-get (car (bndos-test-drafts review)) :body) "On it, done.")))
      (should-not (zerop (car (bndos-test-drafts-cli wt gh "add" "--thread" "NOPE" "--body" "x"))))
      (should-not (zerop (car (bndos-test-drafts-cli wt gh "add" "--conversation" "--body" "  "))))
      (bndos-test-draft-reply review "mine")
      (let ((mine (seq-find (lambda (d) (equal (plist-get d :body) "mine")) (bndos-test-drafts review))))
        (should-not (zerop (car (bndos-test-drafts-cli wt gh "edit" (plist-get mine :id) "--body" "x"))))
        (should-not (zerop (car (bndos-test-drafts-cli wt gh "delete" (plist-get mine :id)))))
        (should (equal (plist-get (seq-find (lambda (d) (equal (plist-get d :id) (plist-get mine :id)))
                                            (bndos-test-drafts review))
                                  :body)
                       "mine")))
      (let ((theirs (seq-find (lambda (d) (string-prefix-p "agent:" (plist-get d :author))) (bndos-test-drafts review))))
        (should (zerop (car (bndos-test-drafts-cli wt gh "delete" (plist-get theirs :id))))))
      (should (= (length (plist-get (cdr (bndos-test-drafts-cli wt gh "list")) :drafts)) 1))
      ;; Nothing posted.
      (should-not (bndos-test-posts stubs)))))

;;; Against GitHub, when asked: BNDOS_TEST_GITHUB_REPO=OWNER/NAME

(defun bndos-test-live-gh (dir &rest args)
  "gh ARGS in DIR: its output, or an error."
  (with-temp-buffer
    (let ((default-directory dir))
      (unless (zerop (apply #'call-process "gh" nil t nil args))
        (error "gh %s: %s" (car args) (buffer-string)))
      (string-trim (buffer-string)))))

(ert-deftest bndos-outbox-live-replies-reach-github-once ()
  "Opens a draft pull request on a test branch of BNDOS_TEST_GITHUB_REPO,
replies through the outbox, checks what GitHub has, and closes it."
  (skip-unless (getenv "BNDOS_TEST_GITHUB_REPO"))
  (let* ((repo (getenv "BNDOS_TEST_GITHUB_REPO"))
         (dir (file-name-as-directory (file-truename (make-temp-file "bndos-live-" t))))
         (clone (expand-file-name "clone/" dir))
         (branch (format "bndos-outbox-test-%s" (format-time-string "%Y%m%d%H%M%S")))
         (bndos-review-github-program "gh")
         (diff-review-overview-threads 'full)
         number review)
    (unwind-protect
        (progn
          (bndos-test-live-gh dir "repo" "clone" repo clone "--" "--depth" "1" "--quiet")
          (bndos-test-git clone "checkout" "-q" "-b" branch)
          (bndos-test-commit clone "outbox test" "bndos-outbox-test.txt" "one\ntwo\nthree\n")
          (bndos-test-git clone "push" "-q" "-u" "origin" branch)
          (bndos-test-live-gh clone "pr" "create" "--draft" "--head" branch "--base" "main" "--title" "[test] bndos outbox, closed by the test"
                              "--body" "Opened and closed by bndos-outbox-live-replies-reach-github-once.")
          (setq number (string-to-number (bndos-test-live-gh clone "pr" "view" "--json" "number" "-q" ".number")))
          ;; A review thread on line 2, as a reviewer writes it.
          (let ((input (expand-file-name "thread.json" dir)))
            (with-temp-file input
              (insert (json-serialize (list :body "Why two?" :commit_id (bndos-test-git clone "rev-parse" "HEAD")
                                            :path "bndos-outbox-test.txt" :line 2 :side "RIGHT"))))
            (bndos-test-live-gh clone "api" "-X" "POST" (format "repos/%s/pulls/%d/comments" repo number)
                                "--input" input))
          ;; Reviewed here: the thread fetched, a reply and a comment drafted.
          (let ((default-directory clone))
            (diff-review-find-file (expand-file-name "review.json" dir) clone "origin/main"))
          (setq review (current-buffer))
          (bndos-test-fetch review)
          (bndos-test-draft-reply-to review "Why two\\?" "Two is the second line — ça va ✓.")
          (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "Thanks for looking.")))
            (bndos-review-outbox-comment review))
          (should-not (string-match-p "Two is the second"
                                      (bndos-test-live-gh clone "api" (format "repos/%s/pulls/%d/comments" repo number))))
          (let ((outbox (bndos-test-outbox-of review)))
            (with-current-buffer outbox (bndos-test-mark-all))
            (bndos-test-send outbox)
            (should (string-match-p "2 sent" (with-current-buffer outbox bndos-review-outbox--report))))
          ;; On GitHub, at once (not in a pending review), each once, with the marker.
          (let ((replies (json-parse-string (bndos-test-live-gh clone "api" (format "repos/%s/pulls/%d/comments" repo number))
                                            :object-type 'plist :array-type 'list))
                (comments (json-parse-string (bndos-test-live-gh clone "api" (format "repos/%s/issues/%d/comments" repo number))
                                             :object-type 'plist :array-type 'list)))
            (should (= (seq-count (lambda (c) (string-prefix-p "Two is the second line — ça va ✓.\n\n<!-- bndos-draft:" (plist-get c :body)))
                                  replies)
                       1))
            (should (plist-get (seq-find (lambda (c) (string-prefix-p "Two is" (plist-get c :body))) replies) :in_reply_to_id))
            (should (= (seq-count (lambda (c) (string-prefix-p "Thanks for looking.\n\n<!-- bndos-draft:" (plist-get c :body)))
                                  comments)
                       1)))
          ;; Fetched back: in their thread and timeline, no marker, no drafts left.
          (bndos-test-fetch review)
          (should (= (length (bndos-test-drafts review "sent")) 2))
          (should-not (bndos-test-drafts review "drafts"))
          (with-current-buffer review
            (should (string-match-p "Two is the second line — ça va ✓\\." (buffer-string)))
            (should-not (string-match-p "bndos-draft" (buffer-string))))
          ;; A review's summary, answered point by point in the conversation.
          (let ((input (expand-file-name "summary.json" dir)))
            (with-temp-file input
              (insert (json-serialize (list :event "COMMENT" :body "Overall fine.\n\n1. One thing."))))
            (bndos-test-live-gh clone "api" "-X" "POST" (format "repos/%s/pulls/%d/reviews" repo number)
                                "--input" input))
          (bndos-test-fetch review)
          (with-current-buffer review
            (goto-char (point-min))
            (re-search-forward "Overall fine\\.")
            (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_ quote) (concat quote "Answered."))))
              (diff-review-reply)))
          (let ((draft (seq-find (lambda (d) (plist-get (plist-get d :target) :answers)) (bndos-test-drafts review "drafts")))
                results)
            (should (equal (plist-get draft :body) "> Overall fine.\n>\n> 1. One thing.\n\nAnswered."))
            (bndos-review-outbox-send-marks review (bndos-review-outbox-store review)
                                            (list (cons (plist-get draft :id) (bndos-review-outbox-hash draft)))
                                            (lambda (r) (setq results r)))
            (should (bndos-proc-wait (lambda () results) 30))
            (should (eq (cadr (car results)) 'sent))
            (should (string-match-p "Answered\\."
                                    (bndos-test-live-gh clone "api" (format "repos/%s/issues/%d/comments" repo number)))))
          ;; A fix, pushed; its reply links the change, taken by the CLI from
          ;; the commit the thread was written on, as GitHub says.
          (bndos-test-commit clone "fix line two" "bndos-outbox-test.txt" "one\n2\nthree\n" "notes.txt" "unrelated\n")
          (bndos-test-git clone "push" "-q" "origin" branch)
          (bndos-test-live-fetch-head review clone)
          (let* ((thread (seq-find (lambda (c) (equal (gethash "body" c) "Why two?"))
                                   (buffer-local-value 'bndos-review-github--comments review)))
                 (default-directory clone)
                 (hunks (with-temp-buffer
                          (should (zerop (call-process bndos-task-cli nil '(t nil) nil "drafts" "hunks"
                                                       "--thread" (gethash "thread-id" thread))))
                          (json-parse-string (buffer-string) :object-type 'plist :array-type 'list :false-object nil)))
                 (ref (plist-get (seq-find (lambda (h) (equal (plist-get h :path) "bndos-outbox-test.txt"))
                                           (plist-get hunks :hunks))
                                 :ref)))
            (should (plist-get hunks :pushed))
            (should (equal (plist-get hunks :base) (gethash "commit" thread)))
            (should (zerop (call-process bndos-task-cli nil nil nil "drafts" "add" "--thread" (gethash "thread-id" thread)
                                         "--body" "Made it a digit." "--hunks" ref "--author" "worker")))
            (bndos-review-outbox-changed (bndos-review-outbox-store review))
            (let ((draft (seq-find (lambda (d) (equal (plist-get d :body) "Made it a digit.")) (bndos-test-drafts review "drafts"))))
              (should-not (car (bndos-review-outbox-problems review draft)))
              (let* ((view (bndos-review-changes-view clone (plist-get draft :changes)))
                     (text (bndos-test-shown clone (mapcar #'car (plist-get view :hunks)))))
                (should (equal (mapcar #'cdr (plist-get view :hunks)) '(linked)))
                (should (string-match-p "^\\+2$" text))
                (should-not (string-match-p "unrelated" text)))
              ;; A commit here, not pushed: held.
              (bndos-test-commit clone "local only" "notes.txt" "unrelated, more\n")
              (let ((bndos-review-changes-cache-seconds 0))
                (should (string-match-p "isn't the pull request's head"
                                        (car (bndos-review-outbox-problems review draft)))))
              (bndos-test-git clone "push" "-q" "origin" branch)
              (bndos-test-live-fetch-head review clone)
              (let ((bndos-review-changes-cache-seconds 0) results)
                (should-not (car (bndos-review-outbox-problems review draft)))
                (bndos-review-outbox-send-marks review (bndos-review-outbox-store review)
                                                (list (cons (plist-get draft :id) (bndos-review-outbox-hash draft)))
                                                (lambda (r) (setq results r)))
                (should (bndos-proc-wait (lambda () results) 30))
                (should (eq (cadr (car results)) 'sent)))))
          ;; A pending review of mine: replies held here; GitHub refuses one
          ;; sent anyway, and nothing is posted.
          (let* ((input (expand-file-name "pending.json" dir))
                 (pending (progn
                            (with-temp-file input
                              (insert (json-serialize (list :commit_id (bndos-test-git clone "rev-parse" "HEAD")
                                                            :comments (vector (list :path "bndos-outbox-test.txt" :line 3
                                                                                    :side "RIGHT" :body "pending"))))))
                            (gethash "id" (json-parse-string
                                           (bndos-test-live-gh clone "api" "-X" "POST"
                                                               (format "repos/%s/pulls/%d/reviews" repo number)
                                                               "--input" input))))))
            (unwind-protect
                (progn
                  (bndos-test-draft-reply-to review "Why two\\?" "While pending.")
                  (bndos-test-fetch review)
                  (let ((draft (seq-find (lambda (d) (equal (plist-get d :body) "While pending."))
                                         (bndos-test-drafts review "drafts")))
                        results)
                    (should (string-match-p "pending review" (car (bndos-review-outbox-problems review draft))))
                    (bndos-review-outbox-send-marks review (bndos-review-outbox-store review)
                                                    (list (cons (plist-get draft :id) (bndos-review-outbox-hash draft)))
                                                    (lambda (r) (setq results r)))
                    (should (bndos-proc-wait (lambda () results) 30))
                    (should (eq (cadr (car results)) 'failed))
                    (should (string-match-p "pending review" (caddr (car results))))
                    (should (seq-find (lambda (d) (equal (plist-get d :body) "While pending."))
                                      (bndos-test-drafts review "drafts")))
                    (should-not (string-match-p "While pending"
                                                (bndos-test-live-gh clone "api" (format "repos/%s/pulls/%d/comments" repo number))))))
              (bndos-test-live-gh clone "api" "-X" "DELETE" (format "repos/%s/pulls/%d/reviews/%d" repo number pending)))))
      (when (buffer-live-p review) (kill-buffer review))
      (when number
        (ignore-errors (bndos-test-live-gh clone "pr" "close" (number-to-string number))))
      (ignore-errors (bndos-test-live-gh dir "api" "-X" "DELETE" (format "repos/%s/git/refs/heads/%s" repo branch)))
      (delete-directory dir t))))

(defun bndos-test-live-fetch-head (review clone)
  "Fetch REVIEW until GitHub has CLONE's HEAD as the pull request's head: it
lags a push by a moment, as g after a push does."
  (let ((head (bndos-test-git clone "rev-parse" "HEAD")) (tries 0))
    (while (progn (bndos-test-fetch review)
                  (and (not (equal (plist-get (buffer-local-value 'bndos-review-github--state review) :head-oid) head))
                       (< (cl-incf tries) 10)))
      (sleep-for 2))
    (should (equal (plist-get (buffer-local-value 'bndos-review-github--state review) :head-oid) head))))

(defun bndos-test-draft-reply-to (review regexp body)
  "Reply BODY to the thread whose text matches REGEXP, from the overview."
  (with-current-buffer review
    (goto-char (point-min))
    (re-search-forward regexp)
    (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) body)))
      (diff-review-reply))))

(defun bndos-test-outbox-of (review)
  "REVIEW's outbox, whatever its pull request."
  (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
    (let ((outbox (with-current-buffer review (bndos-review-outbox))))
      (should (bndos-proc-wait (lambda () (and (with-current-buffer outbox bndos-review-outbox--login)
                                               (not (plist-get (buffer-local-value 'bndos-review-github--state review)
                                                               :fetching))))
                               20))
      (with-current-buffer outbox (bndos-review-outbox--render))
      outbox)))

(provide 'bndos-outbox-tests)
;;; bndos-outbox-tests.el ends here
