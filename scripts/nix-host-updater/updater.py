#!/usr/bin/env python3
"""Trusted Forgejo branch builds and crash-recoverable local retention.

No activation, credentials, branch discovery/scheduling, or garbage collection.
The command runner is injectable so tests need neither the live remote nor hosts.
"""

import argparse
from contextlib import contextmanager
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time
import uuid


REMOTE = "ssh://forgejo@git.local.doreto.com.br:2222/diogo/dotfiles.git"
REPOSITORY_ID = hashlib.sha256(REMOTE.encode()).hexdigest()
HOSTS = {"lapdog": "lapdog", "chungus": "chungus", "mini": "dogdot"}
STORE_DIR = "/nix/store"
ACTIVE = {"started", "checking", "building", "promoting"}


class Error(Exception):
    pass


class Drift(Error):
    """The authoritative branch is absent or no longer at the requested SHA."""


def digest(value):
    return hashlib.sha256(value.encode()).hexdigest()


def fsync_dir(path):
    fd = os.open(path, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)


def atomic_json(path, value):
    temporary = path.with_name(path.name + ".new")
    with temporary.open("w", encoding="utf-8") as stream:
        json.dump(value, stream, indent=2, sort_keys=True)
        stream.write("\n")
        stream.flush()
        os.fsync(stream.fileno())
    os.replace(temporary, path)
    fsync_dir(path.parent)


class Commands:
    """No shell, inherited Git/Nix overrides, tokens, or signing-key settings."""

    def __call__(self, argv, *, cwd=None, log=None):
        allowed = ("PATH", "HOME", "USER", "LOGNAME", "LANG", "LC_ALL", "TZ")
        env = {key: os.environ[key] for key in allowed if key in os.environ}
        env.update({
            "GIT_CONFIG_NOSYSTEM": "1",
            "GIT_CONFIG_GLOBAL": "/dev/null",
            "GIT_TERMINAL_PROMPT": "0",
            "GIT_SSH_COMMAND": ("ssh -oBatchMode=yes -oStrictHostKeyChecking=yes "
                                "-oConnectTimeout=15 -oServerAliveInterval=15 -oServerAliveCountMax=2"),
        })
        if log:
            temporary = log.parent / "tmp"
            temporary.mkdir(mode=0o700, exist_ok=True)
            env["TMPDIR"] = str(temporary)
        # Build/check stdout goes to disk too; captured JSON is small for one system.
        with log.open("ab") if log else open(os.devnull, "ab") as stream:
            stream.write((json.dumps(argv) + "\n").encode())
            stream.flush()
            result = subprocess.run(argv, cwd=cwd, env=env, stdout=subprocess.PIPE,
                                    stderr=stream, check=False)
            stream.write(result.stdout)
        if result.returncode:
            raise Error(f"{argv[0]} {argv[1]} failed ({result.returncode}); see {log}")
        return result.stdout.decode()


class Updater:
    def __init__(self, state, commands=None, hook=None):
        self.state = Path(os.path.abspath(state))
        self.commands = commands or Commands()
        self.hook = hook or (lambda phase: None)
        # Private, single-owner local state is a prerequisite, not a security
        # boundary against another process running as the same Unix account.
        for directory in (self.state, self.state / "runs", self.state / "work",
                          self.state / "roots", self.state / "roots" / REPOSITORY_ID):
            directory.mkdir(mode=0o700, parents=True, exist_ok=True)
            if directory.is_symlink() or directory.stat().st_uid != os.getuid():
                raise Error(f"state must be owned by this user and not symlinked: {directory}")
            if directory.stat().st_mode & 0o077:
                raise Error(f"state directory must be private (0700): {directory}")

    def git(self, *args, cwd=None, log=None):
        return self.commands([
            "git", "-c", "core.hooksPath=/dev/null", "-c", "protocol.allow=never",
            "-c", "protocol.ssh.allow=always", *args,
        ], cwd=cwd, log=log).strip()

    def nix(self, *args, cwd=None, log=None):
        return self.commands(["nix", *args], cwd=cwd, log=log)

    @contextmanager
    def locked(self):
        with (self.state / "lock").open("a") as stream:
            fcntl.flock(stream, fcntl.LOCK_EX)
            yield

    def load(self):
        path = self.state / "manifest.json"
        if not path.exists():
            return {"schema": 1, "repository": REMOTE, "sequence": 0,
                    "runs": {}, "roots": {}, "observations": {}, "journal": None}
        try:
            data = json.loads(path.read_text())
            if data["schema"] != 1 or data["repository"] != REMOTE:
                raise ValueError("wrong schema/repository")
            if (not isinstance(data["sequence"], int) or data["sequence"] < 0
                    or not isinstance(data["runs"], dict) or not isinstance(data["roots"], dict)
                    or not isinstance(data["observations"], dict)):
                raise ValueError("invalid manifest structure")
            for run_id, run in data["runs"].items():
                if not re.fullmatch(r"[0-9a-f]{32}", run_id):
                    raise ValueError("invalid run identity")
                if run["host"] not in HOSTS or run["ref"] != "refs/heads/" + run["branch"]:
                    raise ValueError("invalid run target")
                if (not isinstance(run["sequence"], int) or run["sequence"] > data["sequence"]
                        or run["status"] not in ACTIVE | {"succeeded", "failed", "superseded", "interrupted"}):
                    raise ValueError("invalid run state")
            def validate_root(slot, root):
                run = data["runs"][root["run"]]
                if slot != self.slot(run) or any(root[key] != run[key] for key in
                        ("sha", "tree", "output", "sequence", "branch", "ref", "host")):
                    raise ValueError("invalid root identity")
            for slot, root in data["roots"].items():
                validate_root(slot, root)
            for ref, observation in data["observations"].items():
                if (not ref.startswith("refs/heads/") or not isinstance(observation, dict)
                        or not isinstance(observation["observed_at"], (int, float))
                        or (observation["head"] is not None
                            and not re.fullmatch(r"[0-9a-f]{40}", observation["head"]))):
                    raise ValueError("invalid remote observation")
            journal = data["journal"]
            if journal is not None:
                if not isinstance(journal, dict):
                    raise ValueError("invalid promotion journal")
                if journal["phase"] not in {"prepared", "metadata"}:
                    raise ValueError("invalid journal phase")
                validate_root(journal["slot"], journal["new"])
                if journal["old"]:
                    validate_root(journal["slot"], journal["old"])
            return data
        except (ValueError, KeyError, TypeError) as exc:
            raise Error(f"invalid manifest; preserve state and investigate: {exc}") from exc

    def save(self, data):
        atomic_json(self.state / "manifest.json", data)

    def slot(self, run):
        return digest(run["ref"]) + "/" + run["host"]

    def root_path(self, slot):
        if not re.fullmatch(r"[0-9a-f]{64}/(?:lapdog|mini|chungus)", slot):
            raise Error("invalid root slot")
        return self.state / "roots" / REPOSITORY_ID / slot

    def run_dir(self, run_id):
        if not re.fullmatch(r"[0-9a-f]{32}", run_id):
            raise Error("invalid run ID")
        return self.state / "runs" / run_id

    def guard(self, run_id):
        return self.run_dir(run_id) / "output"

    def root_consistent(self, slot, entry):
        """Require both direct symlink targets, not just a resolvable chain."""
        root = self.root_path(slot)
        guard = self.guard(entry["run"])
        try:
            return (root.is_symlink() and os.readlink(root) == str(guard)
                    and guard.is_symlink() and os.readlink(guard) == entry["output"]
                    and guard.exists())
        except OSError:
            return False

    def running(self, run_id):
        with (self.run_dir(run_id) / "lock").open("a") as stream:
            try:
                fcntl.flock(stream, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except BlockingIOError:
                return True
            return False

    def head(self, ref, log):
        answer = self.git("ls-remote", "--refs", REMOTE, ref, log=log)
        if not answer:
            return None
        rows = [line.split() for line in answer.splitlines()]
        if len(rows) != 1 or len(rows[0]) != 2 or rows[0][1] != ref:
            raise Error("unexpected authoritative branch response")
        sha = rows[0][0]
        if not re.fullmatch(r"[0-9a-f]{40}", sha):
            raise Error("unexpected remote object ID (only SHA-1 repositories supported)")
        return sha

    def observe(self, data, run, head):
        data["observations"][run["ref"]] = {"head": head, "observed_at": time.time()}

    def cleanup_guard(self, data, run_id):
        if any(root["run"] == run_id for root in data["roots"].values()):
            return
        if data["journal"]:
            return
        self.guard(run_id).unlink(missing_ok=True)
        fsync_dir(self.run_dir(run_id))

    def finish_promotion(self, data):
        journal = data["journal"]
        run_id, slot = journal["new"]["run"], journal["slot"]
        if not self.root_consistent(slot, journal["new"]):
            raise Error("root/output-link mismatch; preserve all roots and investigate")
        run = data["runs"][run_id]
        data["roots"][slot] = journal["new"]
        run.update(status="succeeded", finished_at=time.time())
        journal["phase"] = "metadata"
        self.save(data)
        self.hook("after_metadata")
        if not self.root_consistent(slot, journal["new"]):
            raise Error("root/output-link mismatch; preserve all roots and investigate")
        old = journal["old"]
        if old and old["run"] != run_id:
            # Keep the cleanup intent durable until the old guard is gone. A
            # crash after unlink is harmless: replay repeats this idempotently.
            if not any(root["run"] == old["run"] for root in data["roots"].values()):
                self.guard(old["run"]).unlink(missing_ok=True)
                fsync_dir(self.run_dir(old["run"]))
        self.hook("after_old_cleanup")
        data["journal"] = None
        self.save(data)

    def recover_locked(self, data):
        journal = data["journal"]
        if journal:
            root = self.root_path(journal["slot"])
            target = os.readlink(root) if root.is_symlink() else None
            if self.root_consistent(journal["slot"], journal["new"]):
                # Publication already happened; complete its local bookkeeping.
                # This records the historical head check, not current freshness.
                self.finish_promotion(data)
            elif ((journal["old"] and self.root_consistent(journal["slot"], journal["old"]))
                  or (journal["old"] is None and target is None and not root.exists())):
                run = data["runs"][journal["new"]["run"]]
                run.update(status="interrupted", error="promotion did not replace branch root")
                data["journal"] = None
                self.save(data)  # Keep its uncertain guard for operator investigation.
            else:
                raise Error("root/journal mismatch; preserve all roots and investigate")
        for run_id, run in data["runs"].items():
            if run["status"] in ACTIVE and not self.running(run_id):
                run.update(status="interrupted", error="run lock is no longer held")
        self.save(data)

    def recover(self):
        with self.locked():
            data = self.load()
            self.recover_locked(data)
        return self.status()

    def set_phase(self, run_id, phase, **fields):
        with self.locked():
            data = self.load()
            data["runs"][run_id].update(status=phase, **fields)
            self.save(data)

    def checkout(self, run, checkout, log):
        self.git("init", "--quiet", str(checkout), log=log)
        self.git("remote", "add", "forgejo", REMOTE, cwd=checkout, log=log)
        # Fetch the named branch, never a workflow-provided merge ref or arbitrary
        # SHA. Full history and the complete layout preserve Git/self semantics.
        self.git("fetch", "--no-tags", "forgejo", run["ref"], cwd=checkout, log=log)
        fetched = self.git("rev-parse", "FETCH_HEAD^{commit}", cwd=checkout, log=log)
        if fetched != run["sha"]:
            raise Drift(f"branch changed during fetch: {fetched}")
        self.git("checkout", "--quiet", "--detach", run["sha"], cwd=checkout, log=log)
        if self.git("rev-parse", "HEAD", cwd=checkout, log=log) != run["sha"]:
            raise Error("checkout does not match exact source commit")
        return self.git("rev-parse", "HEAD^{tree}", cwd=checkout, log=log)

    def locks(self, checkout):
        return {str(path.relative_to(checkout)): hashlib.sha256(path.read_bytes()).hexdigest()
                for path in checkout.rglob("flake.lock") if ".git" not in path.parts}

    def unchanged(self, checkout, locks, log):
        if self.locks(checkout) != locks:
            raise Error("lock-file mutation detected")
        if self.git("status", "--porcelain", "--untracked-files=all", cwd=checkout, log=log):
            raise Error("checkout mutation detected")

    def promote(self, run_id):
        with self.locked():
            data = self.load()
            self.recover_locked(data)
            run = data["runs"][run_id]
            slot = self.slot(run)
            old = data["roots"].get(slot)
            # Sequence numbers are allocated under the same lock at job start.
            # Even a force-push back to an old SHA must not revive an old job.
            if old and old["sequence"] > run["sequence"]:
                raise Drift("newer successful run already owns this branch/host root")
            head = self.head(run["ref"], self.run_dir(run_id) / "commands.log")
            self.observe(data, run, head)
            self.save(data)
            if head != run["sha"]:
                raise Drift(f"branch deleted or changed before promotion: {head}")
            run.update(status="promoting", promotion_head=head, promotion_checked_at=time.time())
            new = {key: run[key] for key in ("sha", "tree", "output", "sequence", "branch", "ref", "host")}
            new.update(run=run_id, promoted_at=time.time())
            data["journal"] = {"slot": slot, "old": old, "new": new, "phase": "prepared"}
            self.save(data)
            self.hook("after_journal")
            root = self.root_path(slot)
            root.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
            fsync_dir(root.parent.parent)
            staged = root.with_name(root.name + ".new")
            staged.unlink(missing_ok=True)
            staged.symlink_to(self.guard(run_id))
            os.replace(staged, root)
            fsync_dir(root.parent)
            self.hook("after_root")
            self.finish_promotion(data)

    def build(self, branch, host, sha=None, action_run=None):
        if host not in HOSTS:
            raise Error("unknown host")
        ref = "refs/heads/" + branch
        self.git("check-ref-format", ref)
        if sha is not None and not re.fullmatch(r"[0-9a-f]{40}", sha):
            raise Error("--sha must be a full lowercase commit SHA")
        run_id = uuid.uuid4().hex
        directory = self.run_dir(run_id)
        directory.mkdir(mode=0o700)
        fsync_dir(directory.parent)
        log = directory / "commands.log"
        with (directory / "lock").open("a") as run_lock:
            fcntl.flock(run_lock, fcntl.LOCK_EX)
            with self.locked():
                data = self.load()
                self.recover_locked(data)
                data["sequence"] += 1
                run = {"branch": branch, "ref": ref, "host": host, "sha": sha,
                       "sequence": data["sequence"], "status": "started",
                       "started_at": time.time(), "action_run": action_run,
                       "log": str(log), "check": "pending", "build": "pending"}
                data["runs"][run_id] = run
                self.save(data)
            try:
                head = self.head(ref, log)
                with self.locked():
                    data = self.load()
                    self.observe(data, run, head)
                    self.save(data)
                if head is None or (sha is not None and sha != head):
                    raise Drift(f"requested source is not the authoritative branch head: {head}")
                run["sha"] = head
                self.set_phase(run_id, "started", sha=head)
                checkout = self.state / "work" / run_id
                tree = self.checkout(run, checkout, log)
                flake = checkout / "hosts" / host
                if not flake.resolve().is_relative_to(checkout) or not (flake / "flake.lock").is_file():
                    raise Error("host flake must be inside checkout and have a committed lock")
                locks = self.locks(checkout)
                # Explicit git+file reference retains revision metadata and the
                # repository root, including imports outside hosts/<host>.
                source = f"git+{checkout.as_uri()}?dir=hosts/{host}&rev={head}"
                flags = ["--no-update-lock-file", "--no-write-lock-file"]
                self.set_phase(run_id, "checking", tree=tree, source=source)
                self.nix("flake", "check", source, *flags, cwd=checkout, log=log)
                self.unchanged(checkout, locks, log)
                self.set_phase(run_id, "building", check="succeeded")
                output_json = self.nix(
                    "build", source + "#nixosConfigurations." + HOSTS[host] + ".config.system.build.toplevel",
                    *flags, "--out-link", str(self.guard(run_id)), "--json",
                    cwd=checkout, log=log,
                )
                self.unchanged(checkout, locks, log)
                result = json.loads(output_json)
                if len(result) != 1 or set(result[0]["outputs"]) != {"out"}:
                    raise Error("expected exactly one system output")
                output = result[0]["outputs"]["out"]
                if not re.fullmatch(re.escape(STORE_DIR) + r"/[0-9a-z]{32}-[^/\s]+", output):
                    raise Error("unexpected system store path")
                if not self.guard(run_id).is_symlink() or os.readlink(self.guard(run_id)) != output:
                    raise Error("Nix did not create the expected registered output link")
                fsync_dir(directory)
                closure = json.loads(self.nix("path-info", "--recursive", "--json", output, log=log))
                if not closure:
                    raise Error("empty or unavailable closure")
                atomic_json(directory / "closure.json", closure)
                self.set_phase(run_id, "building", build="succeeded", output=output,
                               derivation=result[0].get("drvPath"), closure=str(directory / "closure.json"))
                self.promote(run_id)
            except (Error, OSError, ValueError, KeyError, TypeError) as exc:
                with self.locked():
                    data = self.load()
                    # Never overwrite an uncertain publication or remove its guard.
                    if not data["journal"]:
                        current = data["runs"][run_id]
                        phase = current["status"]
                        current.update(status="superseded" if isinstance(exc, Drift) else "failed",
                                       error=str(exc), finished_at=time.time())
                        if phase == "checking":
                            current["check"] = "failed"
                        elif phase == "building" and current["build"] != "succeeded":
                            current["build"] = "failed"
                        self.save(data)
                        self.cleanup_guard(data, run_id)
                    else:
                        data["runs"][run_id]["publication_error"] = str(exc)
                        self.save(data)
                # Runs always return a per-host result; other hosts are independent.
            with self.locked():
                data = self.load()
                pending = data["journal"] and data["journal"]["new"]["run"] == run_id
                return {"run": run_id, **data["runs"][run_id], "recovery_required": bool(pending)}

    def status(self):
        with self.locked():
            data = self.load()
            runs = []
            for run_id, run in data["runs"].items():
                item = {"run": run_id, **run}
                if run["status"] in ACTIVE and not self.running(run_id):
                    item["status"] = "interrupted"
                    item["recovery_required"] = True
                runs.append(item)
            roots = []
            for slot, root in data["roots"].items():
                observation = data["observations"].get(root["ref"])
                latest = max((run for run in runs if self.slot(run) == slot),
                             key=lambda run: run["sequence"])
                path = self.root_path(slot)
                consistent = self.root_consistent(slot, root)
                roots.append({**root, "path": str(path), "root_consistent": consistent,
                              "latest_attempt": latest["status"],
                              "stale": (not consistent or observation is None
                                        or observation["head"] != root["sha"]
                                        or latest["status"] != "succeeded"),
                              "last_remote_observation": observation})
            return {"repository": REMOTE, "freshness": "local observations only; remote not queried",
                    "roots": roots, "runs": runs, "journal": data["journal"]}


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--state", type=Path, required=True,
                        help="private persistent local state directory (0700)")
    commands = parser.add_subparsers(dest="command", required=True)
    build = commands.add_parser("build-branch", help="check/build one trusted branch and host")
    build.add_argument("branch")
    build.add_argument("--host", choices=HOSTS, required=True)
    build.add_argument("--sha", help="required exact head SHA for Actions; omitted for manual capture")
    build.add_argument("--action-run", help="opaque Actions run/job identity, metadata only")
    commands.add_parser("status", help="local facts only; does not query remote or build")
    commands.add_parser("recover", help="repair interrupted local publication; never build or query remote")
    args = parser.parse_args(argv)
    try:
        updater = Updater(args.state)
        if args.command == "build-branch":
            result = updater.build(args.branch, args.host, args.sha, args.action_run)
            code = 0 if result["status"] == "succeeded" and not result["recovery_required"] else 2
        else:
            result = getattr(updater, args.command)()
            code = 0
        print(json.dumps(result, indent=2, sort_keys=True))
        return code
    except (Error, OSError) as exc:
        print(str(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
