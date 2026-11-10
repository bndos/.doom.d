#!/usr/bin/env python3
"""Deterministic ACP agent for tests: no network, no model.  Run behind Hydra.

* Sessions offer modes "default" and "auto" (like Claude) and models "small"
  and "large"; set_mode / set_model accept those and refuse anything else.
  Loading a session (Hydra reviving it) always succeeds.  With
  $STUB_MODELS_AS_CONFIG set, models are a grouped "model" config option
  instead (as Claude reports them), set with session/set_config_option.
* session/prompt answers a [bndos-review request ...] prompt with a valid
  ```bndos-review``` block covering every "### c-..." item: "addressed" in
  IMPLEMENT mode, "answered" otherwise.  A prompt containing PERMIT first asks
  the client for permission and reports the outcome.  A Hutch review prompt
  (asking for a ```hutch-review``` block) gets one comment on the first
  changed file and an lgtm for the rest.  While the file $STUB_CANCEL_ONCE
  exists, a review prompt is answered as an interrupted turn (stop reason
  "cancelled", no block) and the file is removed; a later prompt asking for
  the hutch-review block (a resume) gets the review of the session's last
  review prompt.  Other prompts get "ok mode=M model=M".
* session/list reports the conversations in $STUB_PAST_SESSIONS
  ("id|title,id|title"), as made in the requested directory.
* Every message received is logged to $STUB_LOG (JSON lines) when set.
"""
import json
import os
import re
import sys
import uuid

LOG = os.environ.get("STUB_LOG")
MODES, MODELS = ("default", "auto"), ("small", "large")
SESSIONS = {}


def send(obj):
    sys.stdout.write(json.dumps(obj) + "\n")
    sys.stdout.flush()


def log(obj):
    if LOG:
        with open(LOG, "a") as f:
            f.write(json.dumps(obj) + "\n")


def session_state(sid):
    s = SESSIONS.setdefault(sid, {"mode": "default", "model": "small"})
    state = {"sessionId": sid,
             "modes": {"currentModeId": s["mode"],
                       "availableModes": [{"id": m, "name": m.title()} for m in MODES]}}
    if os.environ.get("STUB_MODELS_AS_CONFIG"):
        state["configOptions"] = [{"id": "model", "name": "Model", "category": "model", "type": "select",
                                   "currentValue": s["model"],
                                   "options": [{"group": "all", "name": "All",
                                                "options": [{"value": m, "name": m.title()} for m in MODELS]}]}]
    else:
        state["models"] = {"currentModelId": s["model"],
                           "availableModels": [{"modelId": m, "name": m.title()} for m in MODELS]}
    return state


def say(sid, text):
    # Stream in two chunks to exercise accumulation.
    half = len(text) // 2
    for chunk in (text[:half], text[half:]):
        send({"jsonrpc": "2.0", "method": "session/update", "params": {
            "sessionId": sid,
            "update": {"sessionUpdate": "agent_message_chunk", "content": {"type": "text", "text": chunk}}}})


def review_for(text):
    manifest = text.split("Changed files (insertions/deletions):\n", 1)[1].split("\n\n", 1)[0]
    files = [line.split()[0] for line in manifest.splitlines() if line.strip()]
    findings = [{"file": files[0], "title": "stub finding", "description": "stub looked at this",
                 "lines": "1"}] + [{"file": f, "lgtm": True} for f in files[1:]]
    return "Reviewed.\n\n```hutch-review\n" + json.dumps({"findings": findings}) + "\n```\n"


def reply_for(text, s):
    if "```hutch-review" in text:
        return review_for(text)
    m = re.search(r"\[bndos-review request (\S+) · task (\S+) · snapshot (\S+)\]", text)
    if not m:
        return f"ok mode={s['mode']} model={s['model']}"
    request, task, snapshot = m.groups()
    implement = "Mode: IMPLEMENT" in text
    comments = re.findall(r"^### (c-[0-9a-f]+) ", text, re.M)
    responses = [{"comment": c, "status": "addressed" if implement else "answered",
                  "body": f"stub reply to {c}", "files": ["stub.txt"] if implement else [],
                  "tests": []} for c in comments]
    block = {"protocol": "bndos-review/1", "task": task, "request": request,
             "snapshot": snapshot, "responses": responses}
    return "Handled.\n\n```bndos-review\n" + json.dumps(block) + "\n```\n"


def wait_for_response(rid):
    for line in sys.stdin:
        if line.strip():
            msg = json.loads(line)
            log(msg)
            if msg.get("id") == rid and "method" not in msg:
                return msg
    sys.exit(0)


for line in sys.stdin:
    line = line.strip()
    if not line:
        continue
    msg = json.loads(line)
    log(msg)
    method, mid, params = msg.get("method"), msg.get("id"), msg.get("params") or {}
    if mid is None or method is None:
        continue  # a notification, or a response to us
    ok = lambda result: send({"jsonrpc": "2.0", "id": mid, "result": result})
    refuse = lambda message: send({"jsonrpc": "2.0", "id": mid, "error": {"code": -32602, "message": message}})
    if method == "initialize":
        ok({"protocolVersion": 1,
            "agentCapabilities": {"loadSession": True, "promptCapabilities": {},
                                  "sessionCapabilities": {"resume": {}, "list": {}}},
            "authMethods": [], "agentInfo": {"name": "stub-acp", "version": "2"}})
    elif method == "session/new":
        ok(session_state(str(uuid.uuid4())))
    elif method in ("session/load", "session/resume"):
        ok(session_state(params.get("sessionId")))
    elif method == "session/list":
        past = [entry.split("|", 1) for entry in os.environ.get("STUB_PAST_SESSIONS", "").split(",") if entry]
        ok({"sessions": [{"sessionId": sid, "cwd": params.get("cwd"), "title": title,
                          "updatedAt": "2026-11-01T12:00:00Z"} for sid, title in past]})
    elif method == "session/set_mode":
        if params.get("modeId") not in MODES:
            refuse("Unknown mode")
            continue
        session_state(params["sessionId"])
        SESSIONS[params["sessionId"]]["mode"] = params["modeId"]
        ok({})
        send({"jsonrpc": "2.0", "method": "session/update", "params": {
            "sessionId": params["sessionId"],
            "update": {"sessionUpdate": "current_mode_update", "currentModeId": params["modeId"]}}})
    elif method == "session/set_model":
        if params.get("modelId") not in MODELS:
            refuse("Unknown model")
            continue
        session_state(params["sessionId"])
        SESSIONS[params["sessionId"]]["model"] = params["modelId"]
        ok({})
    elif method == "session/set_config_option":
        if params.get("configId") != "model" or params.get("value") not in MODELS:
            refuse("Unknown option value")
            continue
        session_state(params["sessionId"])
        SESSIONS[params["sessionId"]]["model"] = params["value"]
        ok({"configOptions": session_state(params["sessionId"]).get("configOptions", [])})
    elif method == "session/prompt":
        sid = params.get("sessionId")
        s = SESSIONS.setdefault(sid, {"mode": "default", "model": "small"})
        text = "".join(block.get("text", "") for block in params.get("prompt", []))
        if "PERMIT" in text:
            send({"jsonrpc": "2.0", "id": "perm", "method": "session/request_permission", "params": {
                "sessionId": sid,
                "toolCall": {"toolCallId": "t1", "title": "Write stub.txt", "kind": "edit", "status": "pending"},
                "options": [{"optionId": "allow", "name": "Allow", "kind": "allow_once"},
                            {"optionId": "reject", "name": "Reject", "kind": "reject_once"}]}})
            answer = wait_for_response("perm")
            outcome = (answer.get("result") or {}).get("outcome") or {}
            say(sid, f"permission {outcome.get('optionId') or 'refused'}")
        elif "```hutch-review" in text and os.path.exists(os.environ.get("STUB_CANCEL_ONCE") or "\0"):
            os.remove(os.environ["STUB_CANCEL_ONCE"])
            s["review"] = text
            say(sid, "Looking at the diff")
            ok({"stopReason": "cancelled"})
            continue
        elif "hutch-review" in text and "```hutch-review" not in text and s.get("review"):
            say(sid, review_for(s["review"]))
        else:
            if "```hutch-review" in text:
                s["review"] = text
            say(sid, reply_for(text, s))
        ok({"stopReason": "end_turn"})
    else:
        ok({})
