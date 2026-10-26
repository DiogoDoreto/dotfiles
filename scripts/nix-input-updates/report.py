"""Best-effort complete-closure reports and job summaries; never build a baseline."""

import argparse
from collections import Counter, defaultdict
import fcntl
import hashlib
import html
import importlib.util
import json
import os
from pathlib import Path
import re
from urllib.parse import quote

from common import Forgejo, REMOTE, REPO, SERVER, host, output, positive, run, sha, warning

spec = importlib.util.spec_from_file_location("cache_roots", Path(__file__).parents[1] / "nix-cache-roots/roots.py")
roots_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(roots_module)
REPORT = Path(".private/tmp/host-package-report")
CATEGORIES = ("additions", "removals", "version_changes", "rebuilds")
GROUPS = ("changed", "added", "removed")


def closure(path):
    data = json.loads(run("nix", "path-info", "--json", "--recursive", str(path)))
    # Nix versions expose either a path-keyed object or a list with path fields.
    if isinstance(data, dict):
        entries = list(data.items())
    elif isinstance(data, list):
        entries = [(entry.get("path"), entry) for entry in data if isinstance(entry, dict)]
        if len(entries) != len(data):
            raise ValueError("invalid closure metadata")
    else:
        raise ValueError("invalid closure metadata")
    result = {}
    for path, info in entries:
        if (not isinstance(path, str) or not re.fullmatch(r"/nix/store/[0-9a-z]{32}-.+", path)
                or path in result or not isinstance(info, dict)
                or type(info.get("narSize")) is not int or info["narSize"] < 0):
            raise ValueError("invalid closure path or NAR size")
        result[path] = info["narSize"]
    if not result:
        raise ValueError("empty closure metadata")
    return result


def identity(path):
    name = Path(path).name[33:]
    # Nix's conventional DrvName split, not an assertion of package identity.
    match = re.search(r"-(?=[0-9])", name)
    return (name[:match.start()], name[match.end():]) if match else (name, "")


def differences(before, after):
    old, new = defaultdict(list), defaultdict(list)
    old_counts, new_counts = Counter(identity(p)[0] for p in before), Counter(identity(p)[0] for p in after)
    for p in sorted(before - after):
        old[identity(p)[0]].append(p)
    for p in sorted(after - before):
        new[identity(p)[0]].append(p)
    changes = {key: [] for key in CATEGORIES}
    for name in sorted(old.keys() | new.keys()):
        left, right = old[name], new[name]
        # Exact-path audit categories retain conservative singleton pairing.
        if len(left) == len(right) == old_counts[name] == new_counts[name] == 1:
            key = "rebuilds" if identity(left[0])[1] == identity(right[0])[1] else "version_changes"
            changes[key].append(dict(before=left[0], after=right[0]))
        else:
            changes["removals"].extend(left)
            changes["additions"].extend(right)
    return changes


def grouped(before, after):
    old, new = defaultdict(set), defaultdict(set)
    for p in before:
        old[identity(p)[0]].add(p)
    for p in after:
        new[identity(p)[0]].add(p)
    groups = {key: [] for key in GROUPS}
    for name in sorted(old.keys() | new.keys()):
        left, right = old[name], new[name]
        if left == right:
            continue
        key = "changed" if left and right else "added" if right else "removed"
        groups[key].append(dict(name=name, before=sorted(left), after=sorted(right),
                                delta=sum(after[p] for p in right) - sum(before[p] for p in left)))
    return groups


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
    REPORT.mkdir(parents=True, exist_ok=True)
    for name in ("report.json", "report.md", "failure.txt"):
        (REPORT / name).unlink(missing_ok=True)
    pr = pr_state(api, number, source)
    target = pr["base"]["ref"]
    roots = roots or roots_module.Roots()
    with (roots.base / "promotion.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
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
        if any(before[p] != after[p] for p in before.keys() & after.keys()):
            raise ValueError("inconsistent NAR sizes for shared paths")
        # The lock prevents promotion/cleanup releasing either link during queries.
        if roots.selected(pointer, target, machine) != baseline or target_tip(api, target) != tip:
            raise ValueError("target baseline changed during comparison")
        if (roots.output(candidate) != expected["output"] or candidate_provenance.is_symlink() or
                json.loads(candidate_provenance.read_text()) != expected):
            raise ValueError("candidate root changed during comparison")
        pr_state(api, number, source, target)
    data = dict(schema_version=2, host=machine, source=source, target=target, baseline=tip, pr=positive(number),
                run_number=positive(run_number), attempt=positive(attempt),
                run_url=f"{SERVER}/{REPO}/actions/runs/{positive(run_number)}",
                before=sorted(before), after=sorted(after),
                nar_sizes=dict(sorted((before | after).items())),
                changes=differences(set(before), set(after)), groups=grouped(before, after))
    (REPORT / "report.json").write_text(json.dumps(data, indent=2) + "\n")
    (REPORT / "report.md").write_text(render(data, bounded=False))
    output("ready", "true")
    return data


def code(value):
    # HTML code spans keep entities readable (Markdown backticks display them literally).
    return "<code>" + html.escape(str(value)).replace("`", "&#96;").replace("|", "&#124;").replace("\n", " ").replace("\r", " ") + "</code>"


def size(value, signed=False):
    magnitude = abs(value)
    for unit in ("B", "KiB", "MiB", "GiB", "TiB", "PiB"):
        if magnitude < 1024 or unit == "PiB":
            break
        magnitude /= 1024
    text = f"{magnitude:.3g}" if unit != "B" else str(magnitude)
    return ("-" if value < 0 else "+" if signed and value > 0 else "") + text + " " + unit


def versions(paths, shared):
    counts = Counter(identity(p)[1] or "<none>" for p in set(paths) - shared)
    labels = [version + (f" ×{count}" if count > 1 else "") for version, count in sorted(counts.items())]
    if shared:
        labels.append("<unchanged>")
    return ", ".join(labels) or "<none>"


def render(data, bounded=True, artifact_available=True):
    before, after = set(data["before"]), set(data["after"])
    sizes = data["nar_sizes"]
    old_size, new_size = sum(sizes[p] for p in before), sum(sizes[p] for p in after)
    artifact = ("Full report: Actions artifact " + code("host-package-report-" + data["host"] + "-" + str(data["attempt"]))
                + " (instance retention applies)." if artifact_available else
                "**Complete artifact unavailable: upload failed or was skipped. This is only a bounded preview.**")
    lines = [f"## Host package change report: {host(data['host'])}",
             f"Target {code(data['target'])} at {code(data['baseline'])} → PR {code(data['source'])}",
             f"[Build run]({data['run_url']}) · {artifact}", "",
             "Complete system closures, including transitive dependencies. Names/versions are heuristic; "
             "grouping does not imply an unambiguous package upgrade. Sizes are NAR sizes, not disk usage.", "",
             f"**PATHS:** {len(before)} → {len(after)} (+{len(after - before)}, -{len(before - after)})  ",
             f"**SIZE:** {size(old_size)} → {size(new_size)}  ",
             f"**DIFF:** {size(new_size - old_size, signed=True)}", "",
             " · ".join(f"{key}: **{len(data['groups'][key])}**" for key in GROUPS), ""]
    if before == after:
        lines.append("No system closure changes.")
    remaining = 40 if bounded else float("inf")
    for key in GROUPS:
        rows = data["groups"][key]
        if not rows or remaining <= 0:
            continue
        lines.extend([f"### {key.title()}", "", "| Name | Before → After | NAR size delta |",
                      "| --- | --- | ---: |"])
        for row in rows:
            if remaining <= 0:
                break
            shared = set(row["before"]) & set(row["after"])
            name = row["name"]
            text = versions(row["before"], shared) + " → " + versions(row["after"], shared)
            if bounded:
                name = name[:200] + ("…" if len(name) > 200 else "")
                text = text[:500] + ("…" if len(text) > 500 else "")
            lines.append(f"| {code(name)} | {code(text)} | {size(row['delta'], signed=True)} |")
            remaining -= 1
        lines.append("")
    if bounded and sum(map(len, data["groups"].values())) > 40:
        lines.append("Preview limited to 40 grouped rows; report.md contains every group. "
                     "See report.json for exact changed paths and both complete closures.")
    return "\n".join(lines) + "\n"


def write_summary(body):
    path = os.environ.get("FORGEJO_STEP_SUMMARY") or os.environ.get("GITHUB_STEP_SUMMARY")
    if not path:
        raise ValueError("runner did not provide a step summary file")
    # Leave ample headroom under Forgejo's 1 MiB per-step limit; never rely on its truncation.
    if len(body.encode("utf-8")) > 240000:
        raise ValueError("bounded report exceeded summary budget")
    with open(path, "a") as file:
        file.write(body)


def summarize():
    if os.environ.get("REPORT_READY") == "true":
        data = json.loads((REPORT / "report.json").read_text())
        write_summary(render(data, artifact_available=os.environ.get("ARTIFACT_OUTCOME") == "success"))
    else:
        reason = "Comparison step failed before producing a report."
        if os.environ.get("PREPARE_OUTCOME") == "success" and (REPORT / "failure.txt").exists():
            reason = (REPORT / "failure.txt").read_text()[:1000]
        write_summary(f"## Host package change report: {host(os.environ['HOST'])}\n\n"
                      f"Comparison unavailable: {code(reason)}\n\n"
                      "No baseline was built. Mandatory build/retention result is unchanged.\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("operation", choices=("prepare", "summary"))
    args = parser.parse_args()
    try:
        if args.operation == "prepare":
            # Also clear stale state if environment/API initialization fails.
            REPORT.mkdir(parents=True, exist_ok=True)
            for name in ("report.json", "report.md", "failure.txt"):
                (REPORT / name).unlink(missing_ok=True)
            prepare(os.environ["HOST"], os.environ["SOURCE_SHA"], os.environ["PR_NUMBER"],
                    os.environ["RUN_NUMBER"], os.environ["RUN_ATTEMPT"], os.environ["STAGING_ROOT"],
                    Forgejo(os.environ.get("NIX_CACHE_JOB_TOKEN")))
        else:
            summarize()
    except Exception as error:
        warning(f"Host package change report {args.operation} skipped/failed: {error}")
        if args.operation == "prepare":
            REPORT.mkdir(parents=True, exist_ok=True)
            (REPORT / "failure.txt").write_text(str(error)[:1000])
        # Reporting cannot change a successful mandatory check/build/retention result.


if __name__ == "__main__":
    main()
