#!/usr/bin/env bash
# Run the EIN/markdown regression tests inside a throwaway Doom daemon.
# Each test gets a 10s timeout so one hang can't stall the suite.
set -euo pipefail

DOOMDIR="${DOOMDIR:-$HOME/.config/doom}"
EMACS_BIN="${EMACS_BIN:-$HOME/.local/bin/emacs}"
EMACSCLIENT_BIN="${EMACSCLIENT_BIN:-$HOME/.local/bin/emacsclient}"
SOCKET="doom-ein-regression-$$"
TEST_FILE="$DOOMDIR/tests/ein-regression-tests.el"
RESULTS="$(mktemp)"

export DOOMDIR
export EIN_REGRESSION_TEST=1  # keeps Copilot from starting in the daemon

cleanup() {
  timeout 10 "$EMACSCLIENT_BIN" -s "$SOCKET" --eval '(kill-emacs)' >/dev/null 2>&1 \
    || pkill -9 -f -- "--daemon=$SOCKET" || true
  rm -f "$RESULTS"
}
trap cleanup EXIT

"$EMACS_BIN" --daemon="$SOCKET" >/dev/null 2>&1

timeout 300 "$EMACSCLIENT_BIN" -s "$SOCKET" --eval "
(progn
  (load-file \"$TEST_FILE\")
  (with-temp-file \"$RESULTS\"
    (dolist (test (ert-select-tests '(tag ein-regression) t))
      (let* ((status (with-timeout (10 'timeout)
                       (ert-run-test test)
                       (ert-test-result-type-p (ert-test-most-recent-result test) :passed)))
             (label (pcase status ('timeout \"TIMEOUT\") ('nil \"FAIL\") (_ \"PASS\"))))
        (insert (format \"%-4s %s\n\" label (ert-test-name test))))))
  t)" >/dev/null

cat "$RESULTS"
! grep -qv '^PASS' "$RESULTS"
