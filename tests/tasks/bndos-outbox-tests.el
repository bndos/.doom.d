;;; bndos-outbox-tests.el --- Drafts for a pull request, sent those picked -*- lexical-binding: t; -*-

;; Against a fake gh (`bndos-test-gh-pr') that keeps a pull request in files
;; and records what is posted: what is sent is what was picked, once.

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
        pr = {'author': {'login': load('author.json', 'octo')},
              'comments': {'nodes': conversation}, 'reviews': {'nodes': reviews},
              'pending': {'totalCount': 1 if load('pending.json', False) else 0}}
    else:
        pr = {'reviewThreads': {'pageInfo': {'hasNextPage': False, 'endCursor': None},
                                'nodes': [dict(t, comments=page(t['comments']['nodes'], None)) for t in threads]},
              'comments': {'nodes': conversation}, 'reviews': {'nodes': reviews}}
    print(json.dumps({'data': {'viewer': {'login': 'tester'}, 'repository': {'pullRequest': pr}}})); sys.exit(0)
def answer(status, reason, value, code):
    # As gh api --include prints it: status line, headers, a blank line, the body.
    sys.stdout.write('HTTP/2.0 %d %s\\nContent-Type: application/json\\r\\nX-Github-Media-Type: github.v3\\r\\n\\r\\n%s'
                     % (status, reason, json.dumps(value)))
    if code: sys.stderr.write('gh: %s (HTTP %d)\\n' % (reason, status))
    sys.exit(code)
if args[:4] == ['api', '--include', '-X', 'POST']:
    path = args[4]
    with open(args[args.index('--input') + 1], 'rb') as f: payload = json.loads(f.read().decode('utf-8'))
    body = payload.get('body') or ''
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
    posts.append({'path': path, 'body': body, 'payload': payload})
    save('posts.json', posts)
    node = {'databaseId': 1000 + n, 'url': 'https://github.com/me/repo/pull/7#c%d' % (1000 + n), 'body': body,
            'createdAt': '2026-11-19T12:00:%02dZ' % n, 'author': {'login': 'tester'}}
    if '/replies' in path:
        to = int(path.split('/comments/')[1].split('/')[0])
        for t in threads:
            if t['comments']['nodes'][0]['databaseId'] == to: t['comments']['nodes'].append(node)
        save('threads.json', threads)
    elif path.endswith('/reviews'):
        # A review: a thread of each comment on lines, its summary in the timeline.
        for i, c in enumerate(payload.get('comments') or []):
            threads.append({'id': 'T%d-%d' % (n, i), 'isResolved': False, 'isOutdated': False, 'path': c['path'],
                            'diffSide': c.get('side', 'RIGHT'), 'line': c['line'], 'originalLine': c['line'],
                            'originalStartLine': c.get('start_line'),
                            'comments': {'nodes': [dict(node, databaseId=2000 + 10 * n + i, body=c['body'],
                                                        originalCommit={'oid': payload['commit_id']})]}})
        save('threads.json', threads)
        reviews = load('reviews.json', [])
        reviews.append({'id': 'PRR%d' % n, 'url': node['url'], 'body': body, 'submittedAt': node['createdAt'],
                        'state': {'COMMENT': 'COMMENTED', 'REQUEST_CHANGES': 'CHANGES_REQUESTED',
                                  'APPROVE': 'APPROVED'}[payload['event']],
                        'author': {'login': 'tester'}})
        save('reviews.json', reviews)
    elif '/pulls/' in path:
        # A comment on a file: a thread on no lines.
        threads.append({'id': 'T%d' % n, 'isResolved': False, 'isOutdated': False, 'path': payload['path'],
                        'diffSide': 'RIGHT', 'line': None, 'originalLine': None, 'originalStartLine': None,
                        'comments': {'nodes': [dict(node, originalCommit={'oid': payload['commit_id']})]}})
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
conversation.json, reviews.json; posts go to posts.json (a review makes
threads of its comments and a summary of its body), each answered as modes.json's
next mode says: ok, slow, refused and pending (422, nothing posted),
noanswer (nothing posted, no answer), killed (gh dies, nothing posted),
lost (posted, a 502), garbled (posted, a reply cut short), untagged
\(posted, the reply's body without its marker).")

(defun bndos-test-gh-pr (dir head &optional base)
  "Write the fake gh into DIR: pull request 7 from BASE (default HEAD) to
HEAD, one thread on src/a.txt:2 and one comment on its conversation, by
@octo; GitHub posts as @tester.  Return the gh."
  (let ((gh (expand-file-name "gh" dir)))
    (with-temp-file gh (insert bndos-test-gh-pr-script))
    (set-file-modes gh #o755)
    (bndos-test-gh-pr-write dir "pr.json"
                            (list :number 7 :url "https://github.com/me/repo/pull/7" :state "OPEN"
                                  :baseRefName "main" :baseRefOid (or base head) :headRefOid head))
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
  "SPEC is (REVIEW STUBS WT [FROM-MAIN]): a task review whose pull request, #7,
the fake gh in STUBS serves, fetched.  With FROM-MAIN, the pull request's
base is main, so its diff is the task's commit: src/a.txt and b.txt."
  (declare (indent 1))
  `(bndos-test-with-two-files (key ,(nth 2 spec) ,(nth 0 spec))
     (ignore key)
     (let* ((,(nth 1 spec) (make-temp-file "bndos-gh-" t))
            (bndos-review-github-program
             (bndos-test-gh-pr ,(nth 1 spec) (bndos-test-git ,(nth 2 spec) "rev-parse" "HEAD")
                               (and ,(nth 3 spec) (bndos-test-git ,(nth 2 spec) "rev-parse" "HEAD~1"))))
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

(defun bndos-test-redraw (review)
  "Draw REVIEW again, its store changed, as the store's watch does."
  (bndos-review-outbox-changed (bndos-review-outbox-store review))
  (bndos-review-outbox--redraw))

(defun bndos-test-at-thread (review regexp)
  "In REVIEW, drawn again, point on the text matching REGEXP."
  (bndos-test-redraw review)
  (with-current-buffer review
    (goto-char (point-min))
    (should (re-search-forward regexp nil t))
    (goto-char (match-beginning 0))))

(defun bndos-test-send (review &optional confirm choose)
  "Send REVIEW's drafts as \\[bndos-review-send] does, and wait: CHOOSE, as
`bndos-review-outbox-choose-function', picks them (default all); CONFIRM
answers the confirmation (default yes), a character for the kind of review."
  (let ((bndos-review-outbox-choose-function (or choose (lambda (_review drafts) drafts)))
        (confirm (or confirm #'always)))
    (with-current-buffer review
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest args) (apply confirm args)))
                ((symbol-function 'read-multiple-choice)
                 (lambda (&rest args)
                   (let ((answer (apply confirm args)))
                     (cond ((characterp answer) (list answer)) (answer (list ?c)) (t (list ?q)))))))
        (bndos-review-send)))
    (should (bndos-proc-wait (lambda () (not (buffer-local-value 'bndos-review-outbox--sending review))) 15))
    ;; And the fetch a send ends with.
    (should (bndos-proc-wait (lambda () (not (plist-get (buffer-local-value 'bndos-review-github--state review)
                                                        :fetching)))
                             15))))

(defun bndos-test-report (review) (buffer-local-value 'bndos-review-outbox-report review))

(defun bndos-test-asked (review draft)
  "What sending asks of DRAFT."
  (bndos-review-outbox--line review draft (bndos-review-outbox-problems review draft)))

(defun bndos-test-draft (review body)
  "REVIEW's draft whose body is BODY."
  (seq-find (lambda (d) (equal (plist-get d :body) body)) (bndos-test-drafts review)))

(defun bndos-test-posts (stubs) (bndos-test-gh-pr-read stubs "posts.json"))


;;; A reply: a draft, then sent once picked and confirmed

(ert-deftest bndos-outbox-a-reply-is-a-draft-until-sent ()
  (bndos-test-with-pr (review stubs wt)
    (with-current-buffer review
      (should (string-match-p "No drafts" (cadr (should-error (bndos-review-send) :type 'user-error)))))
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
    ;; Asked of each: where it goes, whose, what it says.  None picked, none sent.
    (let (asked)
      (bndos-test-send review nil (lambda (review drafts) (setq asked (mapcar (lambda (d) (bndos-test-asked review d)) drafts))
                                    nil))
      (should (= (length asked) 1))
      (should (string-match-p "\\`Reply on src/a.txt:2  @octo: Why uppercase\\? — [^:]+: “Fixed in the next commit\\.”"
                              (car asked))))
    (should-not (bndos-test-posts stubs))
    ;; Declined at the confirmation: nothing sent.
    (bndos-test-send review #'ignore)
    (should-not (bndos-test-posts stubs))
    (let (question)
      (bndos-test-send review (lambda (q &rest _) (setq question q) t))
      ;; A reply alone makes no review: only a yes.
      (should (string-match-p "Send 1 draft to me/repo#7 as @tester\\?" question)))
    (let ((posts (bndos-test-posts stubs)))
      (should (= (length posts) 1))
      (should (equal (plist-get (car posts) :path) "repos/me/repo/pulls/7/comments/501/replies"))
      (should (string-match-p "\\`Fixed in the next commit\\.\n\n<!-- bndos-draft:d[0-9]+-[0-9a-f]+ -->\\'"
                              (plist-get (car posts) :body))))
    (should (= (length (bndos-test-drafts review "sent")) 1))
    (should (string-match-p "1 sent" (bndos-test-report review)))
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
    (bndos-test-send review)
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
        (should (equal (plist-get draft :seen) "2026-11-12T11:00:00Z"))
        (should (string-match-p "\\`Comment on the pull request, answering @octo" (bndos-test-asked review draft)))
        ;; Another review since: said before sending.
        (bndos-test-gh-pr-write stubs "reviews.json"
                                (vector summary (list :id "PRR2" :url "https://github.com/me/repo/pull/7#prr2"
                                                      :state "COMMENTED" :body "One more thing."
                                                      :submittedAt "2026-11-12T12:00:00Z" :author (list :login "octo"))))
        (bndos-test-fetch review)
        (should (string-match-p "1 new comment or review on the pull request since it was drafted"
                                (bndos-test-asked review draft))))
      ;; Agents see the reviews' summaries too, in the conversation's order.
      (let ((process-environment (cons (concat "BNDOS_TASK_GH=" bndos-review-github-program) process-environment))
            (default-directory wt))
        (with-temp-buffer
          (should (zerop (call-process bndos-task-cli nil t nil "drafts" "threads")))
          (let ((conversation (alist-get 'conversation (json-read-from-string (buffer-string)))))
            (should (equal (mapcar (lambda (m) (alist-get 'body m)) conversation)
                           '("Looks close." "Overall good.\n\n1. Rename foo.\n2. Add tests." "One more thing.")))
            (should (equal (alist-get 'review (aref conversation 1)) "changes_requested"))))))))

;;; What is sent is what was picked

(defun bndos-test-store-file (review state draft)
  (bndos-review-outbox--path (bndos-review-outbox-store review) state (plist-get draft :id)))

(defun bndos-test-rewrite (review draft body)
  "Change DRAFT's body behind Emacs's back, as an agent would."
  (let ((file (bndos-test-store-file review "drafts" draft)))
    (bndos-review-outbox--write file (plist-put (bndos-review-outbox--read file) :body body))))

(ert-deftest bndos-outbox-sends-only-drafts-as-they-were-picked ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "first")
    (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "second")))
      (bndos-review-outbox-comment review))
    (let ((first (bndos-test-draft review "first"))
          (second (bndos-test-draft review "second")))
      ;; Changed while the confirmation was up: that one isn't sent.
      (bndos-test-send review (lambda (&rest _) (bndos-test-rewrite review second "second, changed") t))
      (let ((posts (bndos-test-posts stubs)))
        (should (= (length posts) 1))
        (should (string-prefix-p "first\n" (plist-get (car posts) :body))))
      (should (string-match-p "1 sent, 1 not sent: changed since you chose it" (bndos-test-report review)))
      (should (equal (mapcar (lambda (d) (plist-get d :body)) (bndos-test-drafts review "drafts"))
                     '("second, changed")))
      ;; Deleted while the confirmation was up: not sent either.
      (bndos-test-send review (lambda (&rest _) (delete-file (bndos-test-store-file review "drafts" second)) t))
      (should (= (length (bndos-test-posts stubs)) 1))
      (ignore first))))

(ert-deftest bndos-outbox-sends-only-those-picked ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "send me")
    (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "keep me")))
      (bndos-review-outbox-comment review))
    ;; Asked draft by draft: y the first, n the other.
    (let (asked)
      (cl-letf (((symbol-function 'map-y-or-n-p)
                 (lambda (prompter actor list &rest _)
                   (dolist (d list)
                     (push (funcall prompter d) asked)
                     (when (equal (plist-get d :body) "send me") (funcall actor d))))))
        (bndos-test-send review nil #'bndos-review-outbox--choose))
      (should (= (length asked) 2))
      (should (seq-every-p (lambda (q) (string-prefix-p "Send " q)) asked)))
    (should (equal (mapcar (lambda (p) (car (split-string (plist-get p :body) "\n"))) (bndos-test-posts stubs))
                   '("send me")))
    (should (equal (mapcar (lambda (d) (plist-get d :body)) (bndos-test-drafts review "drafts")) '("keep me")))))

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
      (should (equal (cdr (car results)) '(skipped "sent, deleted or being changed since you chose it")))
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
    (bndos-test-send review)
    (let ((draft (car (bndos-test-drafts review "drafts"))))
      (should (string-match-p "HTTP 422" (plist-get draft :error)))
      (should-not (plist-get draft :claim))
      (should (string-match-p "the last send failed: .*HTTP 422" (bndos-test-asked review draft))))
    (should (string-match-p "1 failed, kept as drafts" (bndos-test-report review)))
    (bndos-test-redraw review)
    (with-current-buffer review (should (string-match-p "draft, not sent: it failed" (buffer-string))))
    ;; Sent again once the user says so.
    (bndos-test-send review)
    (should (= (length (bndos-test-posts stubs)) 1))
    (should (bndos-test-drafts review "sent"))))

;; By the HTTP status GitHub answers with, never by gh's wording.
(ert-deftest bndos-outbox-goes-by-the-status-github-answers ()
  (bndos-test-with-pr (review stubs wt)
    (let ((send (lambda (body mode)
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
    (let ((draft (car (bndos-test-drafts review)))
          results)
      ;; Sent without the fetch a send ends with, to see it unsettled.
      (bndos-review-outbox-send-marks review (bndos-review-outbox-store review)
                                      (list (cons (plist-get draft :id) (bndos-review-outbox-hash draft)))
                                      (lambda (r) (setq results r)))
      (should (bndos-proc-wait (lambda () results) 15))
      (should (equal (car (cdr (car results))) 'unknown))
      (should (bndos-review-outbox-unconfirmed-p (car (bndos-test-drafts review "sending"))))
      (bndos-test-redraw review)
      (with-current-buffer review (should (string-match-p "unconfirmed: may be on GitHub" (buffer-string))))
      ;; Not a draft: not asked about.
      (with-current-buffer review
        (should (string-match-p "No drafts" (cadr (should-error (bndos-review-send) :type 'user-error)))))
      (bndos-test-fetch review)
      (should (= (length (bndos-test-drafts review "sent")) 1))
      (should (plist-get (plist-get (car (bndos-test-drafts review "sent")) :sent) :found))
      (should (= (length (bndos-test-posts stubs)) 1))
      ;; Never posted, no answer: unconfirmed until the user puts it back.
      (bndos-test-draft-reply review "no answer")
      (bndos-test-gh-pr-write stubs "modes.json" ["noanswer"])
      (bndos-test-send review)
      (should (string-match-p "1 unconfirmed: check GitHub" (bndos-test-report review)))
      (bndos-test-fetch review)
      (should (= (length (bndos-test-drafts review "sending")) 1))
      ;; At its thread: put back.
      (bndos-test-at-thread review "Why uppercase\\?")
      (with-current-buffer review
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (bndos-review-draft-put-back)))
      (should (equal (mapcar (lambda (d) (plist-get d :body)) (bndos-test-drafts review "drafts")) '("no answer")))
      (should (= (length (bndos-test-posts stubs)) 1)))))


;;; What can go wrong around a send

(ert-deftest bndos-outbox-sends-any-text-as-written ()
  (bndos-test-with-pr (review stubs wt)
    (let ((body "Ça marche — merci 😀\n\n```\nλ x → x\n```"))
      ;; No question of a coding system: in batch one would be an error.
      (bndos-test-draft-reply review body)
      (should (equal (plist-get (car (bndos-test-drafts review "drafts")) :body) body))
      (bndos-test-send review)
      (should (string-match-p "1 sent" (bndos-test-report review)))
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
    (let ((bndos-review-github-program (expand-file-name "missing-gh" stubs)))
      (bndos-test-send review))
    (should-not bndos-review-outbox--in-flight)
    (let ((draft (car (bndos-test-drafts review "drafts"))))
      (should (equal (plist-get draft :body) "no gh"))
      (should (string-match-p "not sent" (plist-get draft :error))))
    (should (string-match-p "1 failed" (bndos-test-report review)))
    ;; And it sends again.
    (bndos-test-fetch review)
    (bndos-test-send review)
    (should (= (length (bndos-test-posts stubs)) 1))))

(ert-deftest bndos-outbox-a-send-of-another-emacs-is-left-to-it ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "theirs")
    (let* ((store (bndos-review-outbox-store review))
           (draft (car (bndos-test-drafts review)))
           (id (plist-get draft :id))
           (other (start-process "other-emacs" nil (expand-file-name invocation-name invocation-directory)
                                 "-Q" "--batch" "--eval" "(sleep-for 60)")))
      (unwind-protect
          (progn
            ;; Claimed by the other Emacs, posting now.
            (rename-file (bndos-review-outbox--path store "drafts" id) (bndos-review-outbox--path store "sending" id))
            (bndos-review-outbox--write (bndos-review-outbox--path store "sending" id)
                                        (plist-put (bndos-review-outbox--clean draft) :claim
                                                   (list :pid (process-id other) :host (system-name) :at "now")))
            (let ((sending (car (bndos-test-drafts review "sending"))))
              (should-not (bndos-review-outbox-unconfirmed-p sending)))
            (bndos-test-at-thread review "Why uppercase\\?")
            (with-current-buffer review
              (should (string-match-p "theirs" (buffer-string)))
              (should (string-match-p "sending" (buffer-string)))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (should-error (bndos-review-draft-put-back) :type 'user-error)))
            ;; Its post arrives: a fetch leaves it to the Emacs sending it.
            (bndos-review-outbox-reconcile review (list (list id "https://x" "T1")))
            (should (bndos-test-drafts review "sending"))
            ;; Claimed long ago, that Emacs still running: R, asked harder.
            (bndos-review-outbox--write (bndos-review-outbox--path store "sending" id)
                                        (plist-put (bndos-review-outbox--clean draft) :claim
                                                   (list :pid (process-id other) :host (system-name)
                                                         :at "2026-01-01T00:00:00Z")))
            (bndos-test-at-thread review "Why uppercase\\?")
            (with-current-buffer review
              (let (question)
                (cl-letf (((symbol-function 'yes-or-no-p) (lambda (q) (setq question q) nil)))
                  (bndos-review-draft-put-back))
                (should (string-match-p "still runs" question))))
            (should (bndos-test-drafts review "sending"))
            ;; That Emacs gone: unconfirmed, and settled by what GitHub has.
            (delete-process other)
            (should (bndos-proc-wait (lambda () (not (process-attributes (process-id other)))) 10))
            (should (bndos-review-outbox-unconfirmed-p (car (bndos-test-drafts review "sending"))))
            (bndos-review-outbox-reconcile review (list (list id "https://x" "T1")))
            (should (bndos-test-drafts review "sent")))
        (when (process-live-p other) (delete-process other))))))

(ert-deftest bndos-outbox-a-draft-goes-to-its-own-pull-request ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "for #7")
    (let ((draft (car (bndos-test-drafts review)))
          (store (bndos-review-outbox-store review))
          results)
      ;; The review shows another pull request now.
      (with-current-buffer review
        (setq bndos-review-github--state (plist-put (copy-sequence bndos-review-github--state) :number 8)))
      (bndos-review-outbox-send-marks review store
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
    (let ((shown nil))
      (cl-letf (((symbol-function 'display-message-or-buffer) (lambda (m &rest _) (setq shown m))))
        (with-current-buffer review
          (should (string-match-p "Nothing can be sent" (cadr (should-error (bndos-review-send) :type 'user-error))))))
      (should (string-match-p "1 draft can't be sent:\n  Reply on src/a.txt:2.* — you have a pending review" shown)))
    ;; Submitted on GitHub: sent.
    (bndos-test-gh-pr-write stubs "pending.json" :false)
    (bndos-test-fetch review)
    (bndos-test-send review)
    (should (= (length (bndos-test-posts stubs)) 1))))

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
    (bndos-test-send review)
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
    ;; Under its thread, one line, nothing blocking it.
    (bndos-test-redraw review)
    (with-current-buffer review
      (should (string-match-p "draft, not sent · 1 change in 1 file" (buffer-string))))
    (should-not (car (bndos-review-outbox-problems review (car (bndos-test-drafts review)))))
    (progn
      ;; At its thread: a review of the changes alone, of the worktree as checked out.
      (bndos-test-at-thread review "Why uppercase\\?")
      (with-current-buffer review (bndos-review-outbox-thread-changes))
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
        ;; What no draft explains, the test here, in the same review.
        (with-current-buffer review (bndos-review-outbox-unexplained))
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
    (let ((before (plist-get (car (bndos-test-drafts review)) :changes)))
      (bndos-test-send review)
      (should (string-match-p "1 sent" (bndos-test-report review)))
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
    (let ((draft (car (bndos-test-drafts review)))
          (bndos-review-changes-cache-seconds 0))
      ;; A commit here, not pushed: the reply can't go.
      (bndos-test-commit wt "more" "b.txt" "b, more\n")
      (should (string-match-p "the branch here isn't the pull request's head on GitHub"
                              (car (bndos-review-outbox-problems review draft))))
      (cl-letf (((symbol-function 'display-message-or-buffer) #'ignore))
        (should-error (bndos-test-send review) :type 'user-error))
      ;; Pushed: it can; an edit not committed is warned of.
      (bndos-test-push stubs wt)
      (bndos-test-fetch review)
      (bndos-test-write wt "b.txt" "edited\n")
      (let ((problems (bndos-review-outbox-problems review draft)))
        (should-not (car problems))
        (should (member "uncommitted changes in the worktree: the reviewer won't see them" (cdr problems))))
      ;; The fix undone: said before sending.
      (bndos-test-git wt "checkout" "-q" "--" "b.txt")
      (bndos-test-commit wt "undo" "src/a.txt" "one\nTWO\nfour\n")
      (bndos-test-push stubs wt)
      (bndos-test-fetch review)
      (should (string-match-p "1 linked change gone: the code is back as the reviewer saw it"
                              (bndos-test-asked review draft))))))

(ert-deftest bndos-outbox-relinking-changes-a-picked-draft ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-fix stubs wt review)
    (let ((ref (seq-find (lambda (r) (string-match-p "@src/a.txt:" r))
                         (mapcar (lambda (h) (plist-get h :ref))
                                 (plist-get (cdr (bndos-test-gh-cli wt "drafts" "hunks" "--thread" "T1")) :hunks)))))
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--thread" "T1" "--body" "Done."
                                          "--hunks" ref "--author" "worker")))))
    (let ((id (plist-get (car (bndos-test-drafts review)) :id)))
      ;; The agent links other changes while the confirmation is up, the text
      ;; the same: no longer what was picked.
      (bndos-test-send review (lambda (&rest _)
                                (should (zerop (car (bndos-test-gh-cli wt "drafts" "edit" id "--hunks-in" "tests/"))))
                                t))
      (should (equal (plist-get (car (bndos-test-drafts review)) :body) "Done."))
      (should (string-match-p "changed since you chose it" (bndos-test-report review)))
      (should-not (bndos-test-posts stubs)))))

;;; Editing, deleting, and what to know before sending

(ert-deftest bndos-outbox-edits-and-deletions-are-of-the-draft-as-seen ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "v1")
    (let ((draft (car (bndos-test-drafts review))))
      ;; r on a thread with my draft edits it.
      (bndos-test-draft-reply review "v2")
      (should (equal (mapcar (lambda (d) (plist-get d :body)) (bndos-test-drafts review)) '("v2")))
      ;; Changed by someone else while it was being edited: not saved, the text kept.
      (bndos-test-at-thread review "Why uppercase\\?")
      (with-current-buffer review
        (cl-letf (((symbol-function 'read-string-from-buffer)
                   (lambda (&rest _) (bndos-test-rewrite review draft "theirs") "mine")))
          (should-error (bndos-review-draft-edit) :type 'user-error)))
      (should (equal (current-kill 0) "mine"))
      (should (equal (plist-get (car (bndos-test-drafts review)) :body) "theirs"))
      ;; Deleted only as shown.
      (bndos-test-at-thread review "Why uppercase\\?")
      (with-current-buffer review
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (bndos-test-rewrite review draft "again") t)))
          (should-error (bndos-review-draft-delete) :type 'user-error)))
      (should (bndos-test-drafts review))
      (bndos-test-at-thread review "Why uppercase\\?")
      (with-current-buffer review
        (cl-letf (((symbol-function 'y-or-n-p) #'always))
          (bndos-review-draft-delete)))
      (should-not (bndos-test-drafts review)))))

(ert-deftest bndos-outbox-says-what-changed-since-a-draft-was-written ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "my answer")
    (let ((head (bndos-test-git wt "rev-parse" "HEAD"))
          (draft (car (bndos-test-drafts review))))
      ;; A new message in the thread, which is resolved meanwhile.
      (bndos-test-gh-pr-write stubs "threads.json"
                              (vector (list :id "T1" :isResolved t :isOutdated :false :path "src/a.txt"
                                            :diffSide "RIGHT" :line 2 :originalLine 2 :originalStartLine :null
                                            :comments (list :nodes (vector (bndos-test-gh-node 501 "Why uppercase?" 1 head)
                                                                           (bndos-test-gh-node 502 "Also this." 30 head))))))
      (bndos-test-fetch review)
      (let ((asked (bndos-test-asked review draft)))
        (should (string-match-p "1 new message in the thread since it was drafted" asked))
        (should (string-match-p "the thread is resolved on GitHub" asked)))
      ;; The thread gone: it can't be sent.
      (bndos-test-gh-pr-write stubs "threads.json" [])
      (bndos-test-fetch review)
      (should (string-match-p "its thread isn't on the pull request" (car (bndos-review-outbox-problems review draft))))
      (cl-letf (((symbol-function 'display-message-or-buffer) #'ignore))
        (should-error (bndos-test-send review) :type 'user-error))
      (should-not (bndos-test-posts stubs)))))

(ert-deftest bndos-outbox-fetches-again-before-sending-on-old-threads ()
  (bndos-test-with-pr (review stubs wt)
    (bndos-test-draft-reply review "later")
    (with-current-buffer review
      (setq bndos-review-github--state
            (plist-put bndos-review-github--state :at (time-subtract nil 3600))))
    (let* ((asked 0)
           (bndos-review-outbox-choose-function (lambda (_review _drafts) (cl-incf asked) nil)))
      (with-current-buffer review (bndos-review-send))
      ;; Not asked on the old threads: once fetched.
      (should (= asked 0))
      (should (bndos-proc-wait (lambda () (= asked 1)) 10)))
    (should (< (bndos-review-outbox--age review) 60))
    (should-not (bndos-test-posts stubs))))

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
    (should (string-match-p "agent:codex: “Drafted for you\\.”  ⚠ drafted by agent:codex: read it before sending"
                            (bndos-test-asked review (car (bndos-test-drafts review)))))
    ;; Edited at point: mine from then on, sent as mine.
    (bndos-test-at-thread review "Why uppercase\\?")
    (with-current-buffer review
      (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_prompt initial) (concat initial " Checked."))))
        (bndos-review-draft-edit)))
    (let ((draft (car (bndos-test-drafts review))))
      (should (equal (plist-get draft :body) "Drafted for you. Checked."))
      (should-not (string-prefix-p "agent:" (plist-get draft :author)))
      (should-not (string-match-p "drafted by" (bndos-test-asked review draft))))
    ;; And no agent's to change any more.
    (should-not (zerop (car (bndos-test-gh-cli wt "drafts" "edit" (plist-get (car (bndos-test-drafts review)) :id)
                                               "--body" "x"))))))

;;; Comments on lines and files, and the review they make

(ert-deftest bndos-cli-drafts-comments-on-the-pull-requests-lines ()
  (bndos-test-with-pr (review stubs wt t)
    (let ((head (bndos-test-git wt "rev-parse" "HEAD")))
      ;; The lines GitHub takes comments on: its diff's, from main.
      (let ((lines (cdr (bndos-test-gh-cli wt "drafts" "lines"))))
        (should (equal (plist-get lines :head) head))
        (should (equal (mapcar (lambda (f) (plist-get f :path)) (plist-get lines :files)) '("b.txt" "src/a.txt")))
        (should (equal (plist-get (seq-find (lambda (f) (equal (plist-get f :path) "src/a.txt")) (plist-get lines :files))
                                  :right)
                       '((1 3)))))
      ;; On lines out of its diff, or a file it doesn't change: refused, said why.
      (let ((refused (bndos-test-gh-cli wt "drafts" "add" "--path" "src/a.txt" "--line" "9" "--body" "x" "--author" "w")))
        (should-not (zerop (car refused)))
        (should (string-match-p "isn't in one hunk.*lines there: 1-3" (format "%s" (cdr refused)))))
      (should-not (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--path" "nope.txt" "--body" "x" "--author" "w"))))
      (should-not (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--conversation" "--line" "2" "--body" "x"))))
      (should-not (bndos-test-drafts review))
      ;; On lines 2-3, on b.txt, the summary; one summary only.
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--path" "src/a.txt" "--start-line" "2" "--line" "3"
                                          "--body" "Why uppercase here?" "--author" "worker"))))
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--path" "b.txt" "--body" "Is this file needed?"
                                          "--author" "worker"))))
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--review" "--body" "Close; two questions."
                                          "--author" "worker"))))
      (should-not (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--review" "--body" "again" "--author" "worker"))))
      (should (equal (plist-get (plist-get (bndos-test-draft review "Why uppercase here?") :target)
                                :start_line)
                     2))
      (should (equal (plist-get (bndos-test-draft review "Why uppercase here?") :target)
                     (list :kind "line" :path "src/a.txt" :line 3 :side "RIGHT" :commit head :start_line 2)))
      (should (equal (plist-get (bndos-test-draft review "Is this file needed?") :target)
                     (list :kind "file" :path "b.txt" :commit head)))
      ;; Quoting a message: by its id from threads.
      (should (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--thread" "T1" "--quote" "501"
                                          "--body" "Lowercased." "--author" "worker"))))
      (should (bndos-test-draft review "> Why uppercase?\n\nLowercased."))
      (should-not (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--conversation" "--quote" "999" "--body" "x")))))
    ;; Drawn as comments of the review: on their lines, in the conversation.
    (bndos-test-redraw review)
    (with-current-buffer review
      (let ((text (buffer-string)))
        (should (string-match-p "Why uppercase here\\?" text))
        (should (string-match-p "Is this file needed\\?" text))
        (should (string-match-p "Draft review" text))
        (should (string-match-p "Close; two questions\\." text))))
    (let ((file (bndos-test-open-file review "src/a.txt")))
      (with-current-buffer file
        (should (seq-find (lambda (o) (equal (gethash "body" (overlay-get o 'diff-review-comment)) "Why uppercase here?"))
                          (diff-review--thread-overlays)))))
    ;; Sent: the comments on lines and the summary as one review, asking for
    ;; changes; the file's comment and the reply alone.
    (let (question)
      (bndos-test-send review (lambda (q &rest _) (setq question q) ?r))
      (should (string-match-p "Send 2 drafts and a review (its summary, 1 comment on lines) to me/repo#7 as @tester"
                              question)))
    (should (string-match-p "4 sent" (bndos-test-report review)))
    (let* ((posts (bndos-test-posts stubs))
           (reviewed (seq-find (lambda (p) (string-suffix-p "/reviews" (plist-get p :path))) posts))
           (payload (plist-get reviewed :payload))
           (file (seq-find (lambda (p) (equal (plist-get p :path) "repos/me/repo/pulls/7/comments")) posts)))
      (should (= (length posts) 3))
      (should (equal (plist-get payload :event) "REQUEST_CHANGES"))
      (should (equal (plist-get payload :commit_id) (bndos-test-git wt "rev-parse" "HEAD")))
      (should (string-match-p "\\`Close; two questions\\.\n\n<!-- bndos-draft:" (plist-get payload :body)))
      (let ((c (car (plist-get payload :comments))))
        (should (= (length (plist-get payload :comments)) 1))
        (should (equal (list (plist-get c :path) (plist-get c :start_line) (plist-get c :line) (plist-get c :side))
                       '("src/a.txt" 2 3 "RIGHT")))
        (should (string-match-p "\\`Why uppercase here\\?\n\n<!-- bndos-draft:" (plist-get c :body))))
      (should (equal (plist-get (plist-get file :payload) :subject_type) "file"))
      (should (equal (plist-get (plist-get file :payload) :path) "b.txt")))
    ;; Fetched back: all on GitHub, by their markers, none left as drafts.
    (bndos-test-fetch review)
    (should (= (length (bndos-test-drafts review "sent")) 4))
    (should-not (bndos-test-drafts review "drafts"))
    (should-not (bndos-test-drafts review "sending"))))

(ert-deftest bndos-outbox-a-review-is-settled-as-one ()
  (bndos-test-with-pr (review stubs wt t)
    (dolist (args '(("--path" "src/a.txt" "--line" "2" "--body" "one")
                    ("--path" "src/a.txt" "--line" "3" "--body" "two")))
      (should (zerop (car (apply #'bndos-test-gh-cli wt "drafts" "add" "--author" "worker" args)))))
    ;; Asking for changes needs what to change.
    (should-error (bndos-test-send review (lambda (&rest _) ?r)) :type 'user-error)
    (should-not (bndos-test-posts stubs))
    ;; Refused: both drafts again, said why.
    (bndos-test-gh-pr-write stubs "modes.json" ["refused"])
    (bndos-test-send review)
    (should (string-match-p "2 failed" (bndos-test-report review)))
    (should (seq-every-p (lambda (d) (string-match-p "HTTP 422" (plist-get d :error))) (bndos-test-drafts review "drafts")))
    ;; Posted, its answer lost: both unconfirmed, then found by their markers.
    (bndos-test-gh-pr-write stubs "modes.json" ["lost"])
    (bndos-test-send review)
    (should (string-match-p "2 unconfirmed" (bndos-test-report review)))
    (should (= (length (bndos-test-drafts review "sent")) 2))
    (should (= (length (bndos-test-posts stubs)) 1))
    (should (equal (plist-get (plist-get (car (bndos-test-posts stubs)) :payload) :event) "COMMENT"))))

(ert-deftest bndos-outbox-a-review-of-your-own-pull-request-only-comments ()
  (bndos-test-with-pr (review stubs wt t)
    (bndos-test-gh-pr-write stubs "author.json" "tester")
    (bndos-test-fetch review)
    (should (zerop (car (bndos-test-gh-cli wt "drafts" "add" "--review" "--body" "Notes to self." "--author" "w"))))
    (let (question)
      (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (error "Asked for a verdict"))))
        (bndos-test-send review (lambda (q &rest _) (setq question q) t)))
      (should (string-match-p "your own pull request: a comment review" question)))
    (should (equal (plist-get (plist-get (car (bndos-test-posts stubs)) :payload) :event) "COMMENT"))))

(ert-deftest bndos-outbox-drafts-a-comment-on-the-lines-at-point ()
  (bndos-test-with-pr (review stubs wt t)
    (let ((file (bndos-test-open-file review "src/a.txt")))
      (with-current-buffer file
        (goto-char (point-min))
        (forward-line 1)
        (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "Mine, on two.")))
          (bndos-review-draft-comment))
        ;; A file comment, with a prefix argument.
        (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (&rest _) "On the whole file.")))
          (bndos-review-draft-comment t)))
      (should (equal (plist-get (bndos-test-draft review "Mine, on two.") :target)
                     (list :kind "line" :path "src/a.txt" :line 2 :side "RIGHT" :commit (bndos-test-git wt "rev-parse" "HEAD"))))
      (should (equal (plist-get (plist-get (bndos-test-draft review "On the whole file.") :target) :kind) "file"))
      (should-not (string-prefix-p "agent:" (plist-get (bndos-test-draft review "Mine, on two.") :author)))
      ;; Not as GitHub has it, edited here: refused.
      (with-current-buffer file
        (goto-char (point-max))
        (insert "five\n")
        (should (string-match-p "isn.t as the pull request.s head has it"
                                (cadr (should-error (bndos-review-draft-comment) :type 'user-error))))
        (set-buffer-modified-p nil))
      ;; The summary: drafted, then edited, one.
      (with-current-buffer review
        (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_prompt initial) (concat initial "Looks good."))))
          (bndos-review-draft-summary)
          (bndos-review-draft-summary)))
      (should (bndos-test-draft review "Looks good.Looks good."))
      ;; Quoting at a thread: a reply starting with the quote.
      (bndos-test-at-thread review "Why uppercase\\?")
      (with-current-buffer review
        (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_prompt initial) (concat initial "Done."))))
          (bndos-review-draft-quote)))
      (should (equal (plist-get (plist-get (bndos-test-draft review "> Why uppercase?\n\nDone.") :target) :kind) "reply"))
      ;; Sent as one review with the line's comment, approving.
      (bndos-test-send review (lambda (&rest _) ?a))
      (let ((payload (plist-get (seq-find (lambda (p) (string-suffix-p "/reviews" (plist-get p :path)))
                                          (bndos-test-posts stubs))
                                :payload)))
        (should (equal (plist-get payload :event) "APPROVE"))
        (should (equal (mapcar (lambda (c) (plist-get c :line)) (plist-get payload :comments)) '(2)))))))

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
replies and reviews through the drafts, checks what GitHub has, and closes it."
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
          (bndos-test-send review)
          (should (string-match-p "2 sent" (bndos-test-report review)))
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
          ;; An agent's comment on line 3 and the summary: one review, at
          ;; once; its comment a thread of its own on line 3.
          (let ((default-directory clone))
            (should (zerop (call-process bndos-task-cli nil nil nil "drafts" "add" "--path" "bndos-outbox-test.txt"
                                         "--line" "3" "--body" "Three, inline." "--author" "worker")))
            (should (zerop (call-process bndos-task-cli nil nil nil "drafts" "add" "--review"
                                         "--body" "Reviewed here." "--author" "worker"))))
          (bndos-review-outbox-changed (bndos-review-outbox-store review))
          (bndos-test-send review)
          (should (string-match-p "2 sent" (bndos-test-report review)))
          (let ((inline (seq-find (lambda (c) (string-prefix-p "Three, inline.\n\n<!-- bndos-draft:" (plist-get c :body)))
                                  (json-parse-string (bndos-test-live-gh clone "api" (format "repos/%s/pulls/%d/comments" repo number))
                                                     :object-type 'plist :array-type 'list :null-object nil)))
                (reviews (json-parse-string (bndos-test-live-gh clone "api" (format "repos/%s/pulls/%d/reviews" repo number))
                                            :object-type 'plist :array-type 'list :null-object nil)))
            (should inline)
            (should (equal (plist-get inline :line) 3))
            (should-not (plist-get inline :in_reply_to_id))
            (should (seq-find (lambda (r) (and (string-prefix-p "Reviewed here.\n\n<!-- bndos-draft:" (or (plist-get r :body) ""))
                                               (equal (plist-get r :state) "COMMENTED")))
                              reviews)))
          (bndos-test-fetch review)
          (should (= (length (bndos-test-drafts review "sent")) 4))
          (should-not (bndos-test-drafts review "drafts"))
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

(provide 'bndos-outbox-tests)
;;; bndos-outbox-tests.el ends here
