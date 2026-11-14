#!/usr/bin/env bash
# Byte-compile and run the tasks and review test suites in `emacs -Q --batch'
# against Doom's built packages.  No network, no model calls: git repos are
# temporary, worktrunk (wt) is real, and agents are stub-acp-agent.py behind a
# private Hydra daemon (its own HYDRA_ACP_HOME and port; skipped without
# hydra-acp, or $HYDRA).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
emacs=${EMACS:-$HOME/.local/bin/emacs}
"$here/compile.sh"
export XDG_STATE_HOME=$(mktemp -d)   # never touch the real task store
export NO_UPDATE_NOTIFIER=1
cleanup() {
  if [[ -n ${BNDOS_TEST_HYDRA:-} ]]; then "$BNDOS_TEST_HYDRA" daemon stop >/dev/null 2>&1 || true; fi
  rm -rf "$XDG_STATE_HOME" "${HYDRA_ACP_HOME:-}"
}
trap cleanup EXIT
if hydra=$(command -v "${HYDRA:-hydra-acp}"); then
  export BNDOS_TEST_HYDRA=$hydra HYDRA_ACP_HOME=$(mktemp -d)   # never touch the real daemon
  export BNDOS_TEST_STUB_LOG=$HYDRA_ACP_HOME/stub.log
  export BNDOS_TEST_STUB_CANCEL_ONCE=$HYDRA_ACP_HOME/cancel-once
  port=$(python3 -I -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])')
  cat >"$HYDRA_ACP_HOME/config.json" <<JSON
{"daemon": {"port": $port},
 "agents": {"bndos-stub": {"command": "python3", "args": ["-I", "$here/stub-acp-agent.py"],
                           "env": {"STUB_LOG": "$BNDOS_TEST_STUB_LOG",
                                   "STUB_CANCEL_ONCE": "$BNDOS_TEST_STUB_CANCEL_ONCE",
                                   "STUB_PAST_SESSIONS": "past-1|earlier work,past-2|older work"}},
            "bndos-stub-options": {"command": "python3", "args": ["-I", "$here/stub-acp-agent.py"],
                                   "env": {"STUB_LOG": "$BNDOS_TEST_STUB_LOG", "STUB_MODELS_AS_CONFIG": "1"}}}}
JSON
else
  echo "hydra-acp not found: skipping the Hydra integration tests" >&2
fi
"$emacs" -Q --batch -l "$here/load-path.el" -L "$here" \
  -l bndos-core-tests -l bndos-review-tests -l bndos-diff-review-tests -l bndos-cli-tests \
  -l bndos-dashboard-tests -l bndos-agent-integration-tests \
  --eval '(ert-run-tests-batch-and-exit (or (getenv "SELECTOR") t))'
