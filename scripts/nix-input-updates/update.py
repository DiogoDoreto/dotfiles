"""Prepare a lock-only update without credentials; publish only on explicit CI step."""

import argparse
import base64
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import uuid

from common import Forgejo, REMOTE, REPO, clean_env, host, output, positive, run, sha

STATE = Path(".private/tmp/input-update.json")


def changed_lock(machine):
    lock = f"hosts/{host(machine)}/flake.lock"
    changed = run("git", "diff", "--name-only", "HEAD").splitlines()
    if changed not in ([], [lock]) or run("git", "ls-files", "--others", "--exclude-standard"):
        raise ValueError("update changed files other than the selected host lock")
    if run("git", "diff", "--cached", "--name-only"):
        raise ValueError("unexpected staged changes")
    return bool(changed)


def input_report(before, after):
    """Compare sources by input path, not Nix's implementation-specific node IDs."""
    def sources(lock):
        result = {}

        def visit(node_id, path, ancestors):
            node = lock["nodes"][node_id]
            if path and "locked" in node:
                result["/".join(path)] = node["locked"]
            if node_id in ancestors:
                return
            for name, target in node.get("inputs", {}).items():
                # Lists are `follows` aliases, not independently locked inputs.
                # Report the source at its actual input path instead.
                if isinstance(target, str):
                    visit(target, (*path, name), ancestors | {node_id})

        visit(lock["root"], (), set())
        return result

    def describe(source):
        kind = source.get("type", "unknown")
        if kind in ("github", "gitlab"):
            location = f"{kind}:{source['owner']}/{source['repo']}"
        else:
            location = source.get("url", source.get("path", kind))
        revision = source.get("rev", source.get("narHash", ""))
        if revision:
            location += f"/{revision[:12]}"
        if "lastModified" in source:
            date = datetime.fromtimestamp(source["lastModified"], timezone.utc).date()
            location += f" ({date})"
        return location

    old, new = sources(before), sources(after)
    lines = []
    for name in sorted(old.keys() | new.keys()):
        if name not in old:
            lines.append(f"• Added input '{name}':\n    {describe(new[name])}")
        elif name not in new:
            lines.append(f"• Removed input '{name}':\n    {describe(old[name])}")
        elif old[name] != new[name]:
            lines.append(f"• Updated input '{name}':\n    {describe(old[name])}\n  → {describe(new[name])}")
    text = "\n".join(lines) or "Lock graph changed without changes to locked sources."
    # Indented code keeps lock-file strings from being interpreted as Markdown.
    return "### Flake input changes\n\n" + "\n".join("    " + line for line in text.splitlines()) + "\n\n"


def prepare(machine, base):
    host(machine)
    sha(base)
    if run("git", "rev-parse", "HEAD") != base or run("git", "status", "--porcelain"):
        raise ValueError("prepare requires a clean checkout at the captured main SHA")
    # No named inputs means ALL inputs. No check, system evaluation or build.
    run("nix", "flake", "update", "--flake", f"./hosts/{machine}")
    changed = changed_lock(machine)
    STATE.parent.mkdir(parents=True, exist_ok=True)
    STATE.write_text(json.dumps(dict(host=machine, base=base, changed=changed)))
    output("changed", str(changed).lower())
    if not changed:
        message = f"{machine}: already up to date; no branch or PR published."
        print(message)
        if os.environ.get("GITHUB_STEP_SUMMARY"):
            with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as summary:
                summary.write(message + "\n")


def publish(machine, base, run_id, api, token):
    host(machine)
    sha(base)
    positive(run_id)
    state = json.loads(STATE.read_text())
    if state != dict(host=machine, base=base, changed=True):
        raise ValueError("no changed update prepared for this host/base")
    if run("git", "rev-parse", "HEAD") != base or not changed_lock(machine):
        raise ValueError("prepared checkout no longer matches")
    lock = f"hosts/{machine}/flake.lock"
    report = input_report(json.loads(run("git", "show", f"{base}:{lock}")),
                          json.loads(Path(lock).read_text()))
    # A fresh random branch for every invocation, including retries. Never force.
    branch = f"automation/inputs/{machine}/{run_id}-{uuid.uuid4().hex}"
    run("git", "checkout", "-b", branch)
    run("git", "add", "--", f"hosts/{machine}/flake.lock")
    run("git", "-c", "user.name=Nix input automation", "-c",
        "user.email=nix-automation@localhost", "commit", "-m", f"Update {machine} flake inputs")
    env = clean_env()
    authorization = base64.b64encode(f"x-access-token:{token}".encode()).decode()
    env.update(GIT_CONFIG_COUNT="3", GIT_CONFIG_KEY_0=f"http.{REMOTE}.extraHeader",
               GIT_CONFIG_VALUE_0=f"Authorization: Basic {authorization}",
               GIT_CONFIG_KEY_1="http.followRedirects", GIT_CONFIG_VALUE_1="false",
               GIT_CONFIG_KEY_2="credential.helper", GIT_CONFIG_VALUE_2="",
               GIT_TERMINAL_PROMPT="0")
    run("git", "push", REMOTE, f"HEAD:refs/heads/{branch}", env=env)
    pr = api.request("POST", f"/repos/{REPO}/pulls", dict(
        base="main", head=branch, title=f"Update {machine} flake inputs",
        body=f"Update all flake inputs for **{machine}**, starting from main `{base}`.\n\n"
             + report + "This updater performed no flake checks or system builds. The trusted Nix systems "
             "workflow owns validation, retention and any available host package change report.\n\n"
             "No deployment or activation is performed."))
    print(f"Published {machine} PR #{positive(pr['number'])}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("operation", choices=("prepare", "publish"))
    args = parser.parse_args()
    if os.environ.get("DISPATCH_REF") != "refs/heads/main":
        parser.error("input updates may only be dispatched from main")
    machine, base = os.environ["HOST"], os.environ["BASE_SHA"]
    if args.operation == "prepare":
        prepare(machine, base)
    else:
        token = os.environ.get("NIX_AUTOMATION_TOKEN")
        publish(machine, base, os.environ["RUN_ID"], Forgejo(token), token)


if __name__ == "__main__":
    main()
