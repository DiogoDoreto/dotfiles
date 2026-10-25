"""Best-effort complete-closure reports; never check/build/promote a system."""

import argparse
from collections import defaultdict
import fcntl
import hashlib
import html
import importlib.util
import json
import os
from pathlib import Path
import re
from urllib.parse import quote

from common import Forgejo, HOSTS, REMOTE, REPO, SERVER, host, output, positive, run, sha, warning

spec = importlib.util.spec_from_file_location("cache_roots", Path(__file__).parents[1] / "nix-cache-roots/roots.py")
roots_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(roots_module)
REPORT = Path(".private/tmp/host-package-report")
CATEGORIES = ("additions", "removals", "version_changes", "rebuilds")
LABELS = dict(additions="additions", removals="removals", version_changes="version changes",
              rebuilds="same-version rebuilds")


def closure(path):
    paths = run("nix-store", "--query", "--requisites", str(path)).splitlines()
    if not paths or len(paths) != len(set(paths)) or any(
            not re.fullmatch(r"/nix/store/[0-9a-z]{32}-.+", p) for p in paths):
        raise ValueError("invalid or empty closure listing")
    return set(paths)


def identity(path):
    name = Path(path).name[33:]
    # Nix's conventional DrvName split, not an assertion of package identity.
    match = re.search(r"-(?=[0-9])", name)
    return (name[:match.start()], name[match.end():]) if match else (name, "")


def differences(before, after):
    old, new = defaultdict(list), defaultdict(list)
    old_counts, new_counts = defaultdict(int), defaultdict(int)
    for p in before:
        old_counts[identity(p)[0]] += 1
    for p in after:
        new_counts[identity(p)[0]] += 1
    for p in sorted(before - after):
        old[identity(p)[0]].append(p)
    for p in sorted(after - before):
        new[identity(p)[0]].append(p)
    changes = {key: [] for key in CATEGORIES}
    for name in sorted(old.keys() | new.keys()):
        left, right = old[name], new[name]
        # Only pair unambiguous singleton names, including the unchanged closure.
        # Multiple versions/outputs under the same name remain additions/removals.
        if len(left) == len(right) == old_counts[name] == new_counts[name] == 1:
            key = "rebuilds" if identity(left[0])[1] == identity(right[0])[1] else "version_changes"
            changes[key].append(dict(before=left[0], after=right[0]))
        else:
            changes["removals"].extend(left)
            changes["additions"].extend(right)
    return changes


def pr_state(api, number, source, target=None):
    pr = api.request("GET", f"/repos/{REPO}/pulls/{positive(number)}")
    if (pr["state"] != "open" or pr.get("merged") or
            pr["head"]["repo"]["full_name"] != REPO or pr["base"]["repo"]["full_name"] != REPO or
            sha(pr["head"]["sha"]) != sha(source) or
            (target is not None and pr["base"]["ref"] != target)):
        raise ValueError("PR is closed, foreign, retargeted or no longer at the built SHA")
    return pr


def target_tip(api, branch):
    data = api.request("GET", f"/repos/{REPO}/branches/{quote(branch, safe='')}")
    return sha(data["commit"]["id"])


def prepare(machine, source, number, run_number, attempt, staging_root, api, roots=None):
    host(machine)
    sha(source)
    positive(number)
    positive(run_number)
    positive(attempt)
    # A failed retry must not accidentally publish files from an earlier attempt.
    REPORT.mkdir(parents=True, exist_ok=True)
    for name in ("report.json", "report.md"):
        (REPORT / name).unlink(missing_ok=True)
    pr = pr_state(api, number, source)
    target = pr["base"]["ref"]
    roots = roots or roots_module.Roots()
    with (roots.base / "promotion.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        # Resolve immediately before comparison, never trust the event's base SHA.
        tip = target_tip(api, target)
        pointer = roots.base / "branches" / hashlib.sha256(f"refs/heads/{target}".encode()).hexdigest() / machine
        baseline = roots.selected(pointer, target, machine)
        provenance = json.loads((baseline.parent / "provenance.json").read_text())
        if provenance["commit"] != tip:
            raise ValueError("retained target closure does not match the exact current target tip")
        candidate = roots.stage_path(staging_root)
        candidate_provenance = candidate.parent / "provenance.json"
        candidate_data = json.loads(candidate_provenance.read_text())
        expected = dict(repository=REMOTE, branch=pr["head"]["ref"], host=machine,
                        commit=source, output=roots.output(candidate))
        if candidate_provenance.is_symlink() or candidate_data != expected:
            raise ValueError("candidate root provenance mismatch")
        before, after = closure(baseline), closure(candidate)
        # The lock prevents promotion/cleanup releasing either link during queries.
        if roots.selected(pointer, target, machine) != baseline or target_tip(api, target) != tip:
            raise ValueError("target baseline changed during comparison")
        if (roots.output(candidate) != expected["output"] or candidate_provenance.is_symlink() or
                json.loads(candidate_provenance.read_text()) != expected):
            raise ValueError("candidate root changed during comparison")
        pr_state(api, number, source, target)
    data = dict(host=machine, source=source, target=target, baseline=tip, pr=positive(number),
                run_number=positive(run_number), attempt=positive(attempt),
                run_url=f"{SERVER}/{REPO}/actions/runs/{positive(run_number)}",
                before=sorted(before), after=sorted(after), changes=differences(before, after))
    (REPORT / "report.json").write_text(json.dumps(data, indent=2) + "\n")
    (REPORT / "report.md").write_text(render(data, bounded=False))
    output("ready", "true")
    return data


def marker(machine):
    return f"<!-- nix-host-package-report:{host(machine)} -->"


def code(value):
    return "`" + html.escape(str(value)).replace("`", "&#96;").replace("\n", " ").replace("\r", " ") + "`"


def render(data, bounded=True):
    lines = [marker(data["host"]),
             "<!-- nix-host-report-state " + json.dumps({k: data[k] for k in
                 ("run_number", "attempt", "source", "baseline")}, sort_keys=True) + " -->",
             f"## Host package change report: {data['host']}",
             f"Target {code(data['target'])} at {code(data['baseline'])} → PR {code(data['source'])}",
             f"[Build run]({data['run_url']}) · Full report: Actions artifact "
             f"{code('host-package-report-' + data['host'] + '-' + str(data['attempt']))} (instance retention applies).",
             "", "Complete system closures, including transitive dependencies. Store names are heuristic "
             "identities: only unique names are paired; ambiguous outputs/versions remain additions/removals.", "",
             " · ".join(f"{LABELS[key]}: **{len(data['changes'][key])}**" for key in CATEGORIES), ""]
    if data["before"] == data["after"]:
        lines.append("No system closure changes.")
    remaining = 40 if bounded else float("inf")
    for key in CATEGORIES:
        lines.append(f"### {LABELS[key].title()}")
        for change in data["changes"][key]:
            if remaining <= 0:
                break
            text = (f"{change['before']} → {change['after']}" if isinstance(change, dict) else change)
            if bounded and len(text) > 300:
                text = text[:297] + "…"
            lines.append("- " + code(text))
            remaining -= 1
    if bounded and sum(map(len, data["changes"].values())) > 40:
        lines.append("\nPreview limited to 40 changes; download the artifact for every path/change and both complete closures.")
    return "\n".join(lines) + "\n"


def publish(data, api, bot_id, base=roots_module.BASE):
    machine = host(data["host"])
    positive(data["attempt"])
    positive(data["run_number"])
    # Same persistent single-runner domain as retention. Serializes even concurrent
    # processes; never assume a Forgejo concurrency queue is ordered by run age.
    with (Path(base) / f"report-{positive(data['pr'])}-{machine}.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        # Forgejo 16 repo-specific tokens cannot carry read:user (/user).
        # The operator pins the dedicated account's numeric ID as a repo variable.
        bot_id = positive(bot_id)
        comments = api.pages(f"/repos/{REPO}/issues/{positive(data['pr'])}/comments")
        owned = [c for c in comments if c["user"]["id"] == bot_id and marker(machine) in c["body"]]
        if len(owned) > 1:
            raise ValueError("multiple bot-owned marked comments; refusing ambiguous edit")
        if owned:
            match = re.search(r"<!-- nix-host-report-state (.+?) -->", owned[0]["body"])
            if not match:
                raise ValueError("marked comment has no valid freshness metadata")
            previous = json.loads(match[1])
            prior_order = (positive(previous["run_number"]), positive(previous["attempt"]))
            order = (positive(data["run_number"]), positive(data["attempt"]))
            if prior_order >= order:
                raise ValueError("same or newer report already published; preserving it")
        # Last observation before mutation: head, target ref, target tip must all
        # still match. Forgejo comments have no compare-and-swap API.
        pr_state(api, data["pr"], data["source"], data["target"])
        if target_tip(api, data["target"]) != sha(data["baseline"]):
            raise ValueError("target tip changed before comment publication")
        body = render(data)
        if len(body) > 24000:
            raise ValueError("bounded report exceeded comment budget")
        if owned:
            api.request("PATCH", f"/repos/{REPO}/issues/comments/{positive(owned[0]['id'])}", dict(body=body))
        else:
            api.request("POST", f"/repos/{REPO}/issues/{positive(data['pr'])}/comments", dict(body=body))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("operation", choices=("prepare", "publish"))
    args = parser.parse_args()
    try:
        if args.operation == "prepare":
            prepare(os.environ["HOST"], os.environ["SOURCE_SHA"], os.environ["PR_NUMBER"],
                    os.environ["RUN_NUMBER"], os.environ["RUN_ATTEMPT"], os.environ["STAGING_ROOT"],
                    Forgejo(os.environ.get("NIX_CACHE_JOB_TOKEN")))
        else:
            publish(json.loads((REPORT / "report.json").read_text()),
                    Forgejo(os.environ.get("NIX_AUTOMATION_TOKEN")),
                    os.environ.get("NIX_AUTOMATION_BOT_ID"))
    except Exception as error:
        warning(f"Host package change report {args.operation} skipped/failed: {error}")
        # Reporting cannot change a successful mandatory check/build/retention result.


if __name__ == "__main__":
    main()
