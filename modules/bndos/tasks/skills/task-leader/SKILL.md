---
name: task-leader
description: Lead coding work as a set of tasks, each a git worktree (worktrunk) with its own worker agent session (Hydra), driven by `bndos-task`, so the user follows them on the Emacs task dashboard and reviews them there. Use when the user asks to implement one or more features "in worktrees", "as tasks", "in parallel", as stacked branches (main → a → b → c), or to act as a leader with workers/subagents; also to tear down tasks once their pull requests are merged. Not for small edits the user expects done in the current checkout.
---

# Task leader

You are the leader. You don't implement the features yourself: you split the
work into tasks, start a worker for each with `bndos-task`, follow them, check
what they produced and report. Each task is a branch in its own worktree with
its own agent session; the user watches and reviews them in Emacs (dashboard
`SPC g w w`, review `RET`).

```sh
bt=~/.config/doom/modules/bndos/tasks/bin/bndos-task   # Python CLI, JSON on stdout
```

Exit codes: 0 ok, 1 failure or unknown target, 2 `--wait` timed out, 3 the
Hydra daemon can't be reached. Full reference: `$bt --help`.

## 1. Check the setup

Run these from the repository root before planning:

- `git rev-parse --show-toplevel` and `git status --short`: a git repo.
  Workers start from committed refs, so uncommitted work in the checkout
  isn't in their worktrees; tell the user if there is some.
- `command -v hydra-acp wt` and `test -f ~/.hydra-acp/config.json`.
- `$bt status --repo .`: must exit 0. Exit 3 means Hydra isn't reachable.
- `$bt context --repo .`: the project folder's `.tasks` folder as `context`,
  or null when the repo isn't in a project folder (a folder holding the main
  checkout and a `.tasks` folder, where task worktrees go beside the main
  checkout). If it's null, mention that the repo could move into one (see
  "Project folder" below); don't move it unasked.

If anything is missing, stop and say what. Don't install or configure
anything yourself.

## 2. Plan, then confirm

Split the request into tasks with short, kebab-case branch names. One
branch, one worktree, one agent: a task never works on two branches, so
stacked pull requests are separate tasks, each started on its own
(`$bt start --base FIRST SECOND`). Decide each task's base:

- **Independent** tasks all branch from the repo's main branch and run at
  the same time.
- **Stacked** tasks (b needs a's code) branch from the task below them:
  `main → a → b → c`. A layer only sees its parent's *committed* work at
  the moment it's created, so start each layer only after the one below it
  has finished and committed.

Show the user the plan before starting anything: branches, bases (the stack
drawn as a tree), a one-line goal per task, the agent (`claude-code` unless
they choose otherwise; `--agent codex` also works), the model (the agent's
default unless they ask for one) and the mode (Claude defaults to `auto`,
so workers edit and run commands without asking). `$bt models --agent ID`
lists the models and modes an agent accepts, for `--model` / `--mode`;
pass only ids from that list.
Workers are real agent sessions that cost money, so wait for an explicit go.

## 3. Write each worker's prompt

One prompt file per task, outside the repository so it is never committed:
`<context>/briefs/<branch>.md` (`/` as `-`, the name `start` keeps the brief
under) when `context` is set, else in `dir=$(mktemp -d)`; below, `$dir` is
whichever folder that is. With a
`context`, also write the agreed plan to `<context>/plan.md` and keep
decisions that span tasks there, and read it first when picking work up
again; it is local to this machine, not in git. A worker knows nothing about
the other tasks or this conversation, so each prompt has to stand alone. It
also becomes the task's brief, which AI reviews in Emacs judge the change
against, so state the goal and "done when" plainly:

```markdown
# <Task title>

You're working in a git worktree on branch `<branch>`, based on `<base>`.
<For a stacked layer: The parent branch already contains: <what the layers
below did>. Build on it; don't redo it.>

## Goal
<What to build and why, in the user's own terms.>

## Scope
- In: <files/areas/behaviour>
- Out: <what belongs to other tasks; don't touch it>

## Done when
- <acceptance criteria>
- <tests to add or update, and the command that runs them>

## Rules
- Stay inside this worktree, on `<branch>`. Don't switch, create or check
  out other branches. Don't push, merge, rebase or open pull requests.
- When done, commit everything on this branch with a short, descriptive
  message, without any Co-Authored-By or other AI attribution lines.
- Finish with a short summary: what changed, how you tested it, anything
  left open.
```

## 4. Start and follow

Independent tasks: start them all, then wait on each.

```sh
$bt start --repo . --base main --prompt-file "$dir/feat-a.md" feat-a
$bt start --repo . --base main --prompt-file "$dir/feat-b.md" feat-b
$bt status --repo . --wait done --timeout 1800 feat-a
```

Stacked tasks, one layer at a time:

```sh
$bt start --repo . --base main  --prompt-file "$dir/feat-a.md" feat-a
$bt status --repo . --wait done --timeout 1800 feat-a
# verify feat-a (step 5) before going on
$bt start --repo . --base feat-a --prompt-file "$dir/feat-b.md" feat-b
$bt status --repo . --wait done --timeout 1800 feat-b
$bt start --repo . --base feat-b --prompt-file "$dir/feat-c.md" feat-c
```

`start` returns once the prompt is sent, with the branch, worktree, agent,
session, mode and any `warnings` (a mode or model the agent refused). Pass
`--ticket ID` if the user gave a ticket.

What `status` tells you (`state`, `done`) and what to do:

| state | meaning | do |
|---|---|---|
| `working` | mid-turn | keep waiting |
| `waiting` | a permission request is open | tell the user which branch waits and that `v` on its row in the dashboard (`SPC g w w`) opens it to answer; never kill the waiting process |
| `limited` | a usage limit stopped it (`limit`: which, `resets_at`, or what the agent said) | tell the user which branch and when it resets; send nothing until then |
| `idle` + `done: true` | finished its last prompt | verify (step 5) |
| `stopped` | idle agent shut down by Hydra | normal; it revives on the next `send` |
| `missing` / `none` | no agent | `$bt start --repo . <branch>` starts one in the existing task (nothing is created; the base is the recorded parent) |

`start` refuses a task that already has an agent; use `send`. To make an
agent's earlier conversation the task's session, `$bt sessions <branch>`
lists them and `$bt adopt <branch> --session ID` takes an id from that list
only, never a `hydra_session_…` id.

If a worker ends up on the wrong branch or a task gets mixed up with
another, stop and tell the user what happened. Don't repair it with `adopt`
or a second agent.

A `--wait` timeout (exit 2) isn't a failure: check `status` and decide
whether to keep waiting. `$bt status --repo .` with no target lists every
worktree of the repo.

## 5. Verify each task

Use the `worktree` path from `start`/`status`:

- `git -C <worktree> log --oneline <base>..<branch>`: has it committed?
- `git -C <worktree> status --short`: anything left uncommitted?
- `git -C <worktree> diff --stat <base>...<branch>`: does the change match
  the scope?
- Run the project's tests in the worktree.

If something is missing, send the same worker a follow-up rather than fixing
it yourself:

```sh
$bt send feat-a --repo . --prompt "Tests in foo_test.py fail: <output>. Fix them and commit."
$bt status --repo . --wait done --timeout 1800 feat-a
```

## 6. Revisions to a lower layer

If `feat-a` changes after `feat-b` was started from it, `feat-b` doesn't have
those changes. Tell the user, and only with their go-ahead ask the upper
worker to rebase: `$bt send feat-b --prompt "Rebase this branch onto feat-a,
resolve conflicts, re-run the tests, commit."`, then the next layer up the
same way.

## 7. Report and hand over

End with a table: branch, base, state, commits, tests run and their result,
and anything open. Then point the user to the dashboard (`SPC g w w`) for
review. Their review threads go to the workers from Emacs; `status` shows each
task's review state and unresolved thread count if they ask you to follow up.
The threads themselves are the comments of the task's review file,
`$(git rev-parse --path-format=absolute --git-common-dir)/review/<branch>.json`
(read it with `jq`; leave writing to Emacs).
With a `context`, note in `<context>/plan.md` what was done and what is open.

## Project folder: moving a repo into one, only when asked

When the user wants the repo in a project folder (says the layout is wrong,
asks for `.tasks`, or agrees when you mention it), `bndos-task migrate` does
the moving: `~/Projects/foo` becomes the folder and the repo moves to
`~/Projects/foo/foo`, with its task worktrees beside it and their records,
briefs and review files following. You never move folders yourself.

1. `$bt migrate --repo . --dry-run`. A failure says what blocks it (an agent
   working or waiting, a locked worktree, a name taken); report it and stop.
2. Show the plan in plain words: where the repo and each task worktree go,
   which tasks get a new agent (`sessions_dropped`: their conversations stay
   in Hydra but aren't continued), and every `follow_up` item.
3. Wait for an explicit go, then run `$bt migrate --repo .`.
4. Relay the `follow_up` items the user has to do (reopen files in Emacs,
   recreate a virtualenv, `direnv allow`, restart dev servers).
5. Carry on from the new main checkout (`main` in the output): the folder
   you were started in is now the project folder, not the repo. Its
   `context` is the `.tasks` folder to keep the plan in from then on.

## 8. Teardown, only when asked

When the user asks to clean up tasks whose pull requests are merged, for
each branch they name (or every task of this run):

1. **Merged on GitHub?**
   `gh pr list --head <branch> --state merged --json number,url,baseRefName,mergedAt`
   must return a pull request. No merged PR means don't remove it; report it.
2. **Nothing would be lost?**
   - `$bt status --repo . <branch>`: not `working` or `waiting`. A busy or
     waiting worker keeps its worktree.
   - `git -C <worktree> status --short`: empty. Uncommitted changes keep the
     worktree.
   - `git -C <worktree> log --oneline @{u}..HEAD 2>/dev/null`: empty.
     Commits that never reached the PR keep the worktree.
3. **Stack order.** Remove from the top of the stack down. Keep a layer
   while a task above it is still open, unless the user says otherwise.
4. **Confirm.** Show the list (branch, PR, worktree, what will be deleted)
   and wait for an explicit go.
5. **Remove** each worktree and its local branch:
   ```sh
   wt remove --foreground --format json <branch>
   ```
   wt deletes the branch only when its changes are already on the default
   branch. Otherwise it removes the worktree and keeps the branch
   (`"branch_outcome": "retained_unmerged"`), which is what happens to a
   stacked layer merged into another base, or to a squash merge. For a
   branch that step 1 confirmed merged, delete the leftover branch with
   `wt remove --foreground --format json -D <branch>`. Never pass `-f`,
   `-y` or `--no-hooks`. If wt asks to approve a hook or refuses, stop and
   show its message.
6. **Remote branches and bases.** Leave remote branches and any base the
   user created (such as a sandbox branch) unless they ask. If they do:
   `git push origin --delete <branch>`, and `git branch -D` only for a base
   with no open PRs.
7. **Report** what was removed and what was kept, with the reason. Also
   mention what stays on purpose: Hydra keeps the sessions as history
   (`hydra-acp session list`), and the dashboard lists tasks with threads or
   a session under *gone*, where the user can restore or forget them.

## Never

- Push, merge or open pull requests: the user does those after reviewing.
- Delete worktrees or branches outside a teardown the user asked for, or
  any that step 8 didn't confirm merged.
- Edit the tasks' code yourself or commit in the main checkout.
- Start a second agent for a task that already has one; use `send`.
- Switch a task's branch, or fix a mix-up yourself with `adopt` or another
  agent.
- Move or rename a repo or worktree folder by hand; `migrate` does it.
- Resolve, answer or approve review threads for the user.
