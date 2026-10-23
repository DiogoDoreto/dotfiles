"""Promote existing Nix roots. Actions owns checkout, builds, logs and retries."""

import argparse
import base64
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import uuid


REMOTE = "https://git.local.doreto.com.br/diogo/dotfiles.git"
BASE = Path("/var/lib/chungus-nix-roots")
STORE = Path("/nix/store")
HOSTS = ("lapdog", "mini", "chungus")


def command(*args):
    # Do not use checkout-local Git configuration or credentials for the query.
    env = {key: value for key, value in os.environ.items()
           if key in ("PATH", "SSL_CERT_FILE", "NIX_SSL_CERT_FILE", "TMPDIR")}
    env.update(HOME="/nonexistent", GIT_CONFIG_NOSYSTEM="1", GIT_TERMINAL_PROMPT="0")
    if args[:2] == ("git", "ls-remote"):
        token = os.environ.get("NIX_CACHE_JOB_TOKEN")
        if not token:
            raise ValueError("promotion requires the short-lived Actions job token")
        authorization = base64.b64encode(f"x-access-token:{token}".encode()).decode()
        # Checkout's job token also permits reading private repository refs.
        # Scope the header to this fixed repository, never argv or a config file.
        env.update(GIT_CONFIG_COUNT="2",
                   GIT_CONFIG_KEY_0=f"http.{REMOTE}.extraHeader",
                   GIT_CONFIG_VALUE_0=f"Authorization: Basic {authorization}",
                   GIT_CONFIG_KEY_1="http.followRedirects", GIT_CONFIG_VALUE_1="false")
    return subprocess.check_output(args, cwd="/", env=env, text=True, timeout=60).strip()


def directory(path):
    path.mkdir(mode=0o700, parents=True, exist_ok=True)
    if path.is_symlink() or not path.is_dir() or path.stat().st_uid != os.getuid():
        raise ValueError(f"not an owned directory: {path}")
    if path.stat().st_mode & 0o077:
        raise ValueError(f"directory must be private: {path}")


def sync_directory(path):
    fd = os.open(path, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)


class Roots:
    def __init__(self, base=BASE, run=command):
        self.base = Path(base)
        self.run = run
        directory(self.base)
        for name in ("staging", "branches"):
            directory(self.base / name)
        sync_directory(self.base.parent)
        sync_directory(self.base)

    def stage(self):
        """Allocate a persistent out-link name; Nix build registers it, not us."""
        stage = self.base / "staging" / uuid.uuid4().hex
        directory(stage)
        sync_directory(stage.parent)
        return stage / "result"

    def stage_path(self, root):
        root = Path(root)
        if (root.name != "result" or root.parent.parent != self.base / "staging"
                or not re.fullmatch(r"[0-9a-f]{32}", root.parent.name)):
            raise ValueError("not a managed staging root")
        directory(root.parent)
        return root

    def output(self, root):
        if not root.is_symlink():
            raise ValueError(f"root is not a symlink: {root}")
        output = Path(os.readlink(root))
        if (output.parent != STORE or not re.fullmatch(r"[0-9a-z]{32}-.+", output.name)
                or not output.exists() or output.is_symlink()):
            raise ValueError(f"invalid store target: {root}")
        if self.run("nix", "path-info", str(output)) != str(output):
            raise ValueError("store target is not valid")
        if not root.is_symlink() or os.readlink(root) != str(output) or not root.exists():
            raise ValueError("root changed during store validation")
        return str(output)

    def selected(self, pointer, branch, host, expected=None):
        if not pointer.is_symlink():
            raise ValueError("corrupt branch pointer")
        stage = self.stage_path(Path(os.readlink(pointer)) / "result")
        provenance = stage.parent / "provenance.json"
        if provenance.is_symlink():
            raise ValueError("corrupt provenance")
        data = json.loads(provenance.read_text())
        if (not isinstance(data, dict) or not isinstance(data.get("commit"), str)
                or (expected is not None and data != expected)
                or data.get("branch") != branch or data.get("host") != host
                or data.get("repository") != REMOTE
                or not re.fullmatch(r"[0-9a-f]{40}", data.get("commit", ""))
                or data.get("output") != self.output(stage)):
            raise ValueError("root/provenance mismatch")
        if (provenance.is_symlink() or json.loads(provenance.read_text()) != data
                or not pointer.is_symlink() or os.readlink(pointer) != str(stage.parent)):
            raise ValueError("selection changed during validation")
        return stage

    def promote(self, branch, host, commit, staging_root):
        if host not in HOSTS or not re.fullmatch(r"[0-9a-f]{40}", commit):
            raise ValueError("invalid host or commit")
        ref = f"refs/heads/{branch}"
        self.run("git", "check-ref-format", ref)
        stage = self.stage_path(staging_root)
        # Same short lock must be used by any future manual reconciliation.
        with (self.base / "promotion.lock").open("a") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            output = self.output(stage)
            identity = hashlib.sha256(ref.encode()).hexdigest()
            parent = self.base / "branches" / identity
            directory(parent)
            sync_directory(parent.parent)
            pointer = parent / host
            old = self.selected(pointer, branch, host) if os.path.lexists(pointer) else None
            observed = self.run("git", "ls-remote", "--refs", "--exit-code", REMOTE, ref)
            if observed != f"{commit}\t{ref}":
                raise ValueError("source branch no longer points to built commit")
            data = dict(repository=REMOTE, branch=branch, host=host, commit=commit, output=output)
            provenance = stage.parent / "provenance.json"
            if os.path.lexists(provenance):
                if provenance.is_symlink() or json.loads(provenance.read_text()) != data:
                    raise ValueError("staging provenance mismatch")
            else:
                # Written once before publication, not a second mutable manifest.
                with provenance.open("x") as file:
                    json.dump(data, file, sort_keys=True)
                    file.write("\n")
                    file.flush()
                    os.fsync(file.fileno())
            # Persist both the result/provenance and the random directory's
            # entry before publishing a pointer or releasing any previous root.
            sync_directory(stage.parent)
            sync_directory(stage.parent.parent)
            pending = parent / f".{host}-{uuid.uuid4().hex}"
            pending.symlink_to(stage.parent)
            try:
                # Old-root validation may block. Do it before the final new-root
                # check, so corruption during either the remote or old query
                # cannot publish an inconsistent replacement.
                if old is not None and self.selected(pointer, branch, host) != old:
                    raise ValueError("previous selection changed")
                self.selected(pending, branch, host, data)
                os.replace(pending, pointer)
                sync_directory(parent)
                try:
                    self.selected(pointer, branch, host, data)
                except (ValueError, OSError, subprocess.SubprocessError):
                    # Best-effort local rollback, not a recovery journal. Keep
                    # both registered links; never release old on a mismatch.
                    if old is None:
                        pointer.unlink()
                    else:
                        pending.symlink_to(old.parent)
                        os.replace(pending, pointer)
                    sync_directory(parent)
                    raise
                # No blocking old-root query after this final new-root check.
                # Registered indirect links stay put; only the pointer moves.
                if old is not None and old != stage:
                    old.unlink()
            finally:
                pending.unlink(missing_ok=True)
            return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("stage", help="allocate a persistent nix build --out-link")
    promote = commands.add_parser("promote", help="select a successful current-head root")
    promote.add_argument("--branch", required=True)
    promote.add_argument("--host", choices=HOSTS, required=True)
    promote.add_argument("--commit", required=True)
    promote.add_argument("--staging-root", required=True)
    args = parser.parse_args()
    try:
        roots = Roots()
        if args.command == "stage":
            print(roots.stage())
        else:
            print(roots.promote(args.branch, args.host, args.commit, args.staging_root))
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        parser.exit(1, f"nix-cache-roots: {error}\n")


if __name__ == "__main__":
    main()
