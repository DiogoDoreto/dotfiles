"""Local Git + retention tests; real GC uses only a disposable local store."""

import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("roots", HERE.parent / "roots.py")
r = importlib.util.module_from_spec(spec)
spec.loader.exec_module(r)
TMP = HERE.parents[2] / ".private" / "tmp"
TMP.mkdir(parents=True, exist_ok=True)


def git(*args):
    return subprocess.check_output(["git", *map(str, args)], text=True,
                                   stderr=subprocess.DEVNULL).strip()


class Environment(unittest.TestCase):
    def test_query_token_is_ephemeral_scoped_not_argv_and_not_forwarded_to_nix(self):
        with patch.dict(os.environ, {"NIX_CACHE_JOB_TOKEN": "job-secret",
                                     "GIT_CONFIG_COUNT": "99", "NIX_CONFIG": "bad",
                                     "UPDATER_TOKEN": "write-secret"}), \
                patch.object(r.subprocess, "check_output", return_value="") as call:
            r.command("git", "ls-remote", "--refs", "--exit-code", r.REMOTE, "refs/heads/main")
            args, kwargs = call.call_args
            self.assertNotIn("job-secret", str(args))
            self.assertEqual(kwargs["cwd"], "/")
            env = kwargs["env"]
            self.assertEqual(env["GIT_CONFIG_KEY_0"], f"http.{r.REMOTE}.extraHeader")
            self.assertEqual(env["GIT_CONFIG_VALUE_1"], "false")
            self.assertNotIn("UPDATER_TOKEN", env)
            self.assertNotIn("NIX_CONFIG", env)
            self.assertNotIn("NIX_CACHE_JOB_TOKEN", env)
            r.command("nix", "path-info", "/nix/store/unused")
            env = call.call_args.kwargs["env"]
            self.assertNotIn("GIT_CONFIG_VALUE_0", env)
            self.assertNotIn("NIX_CACHE_JOB_TOKEN", env)
        with patch.dict(os.environ, {}, clear=True), self.assertRaises(ValueError):
            r.command("git", "ls-remote", r.REMOTE)


class Roots(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(dir=TMP)
        self.addCleanup(self.tmp.cleanup)
        self.base = Path(self.tmp.name)
        self.remote = self.base / "remote.git"
        git("init", "--bare", self.remote)
        # Use real commits rather than an external repository.
        env = {**os.environ, "GIT_AUTHOR_NAME": "Test", "GIT_AUTHOR_EMAIL": "test@invalid",
               "GIT_COMMITTER_NAME": "Test", "GIT_COMMITTER_EMAIL": "test@invalid"}
        tree = subprocess.check_output(["git", "--git-dir", str(self.remote), "mktree"],
                                       input="", text=True).strip()
        self.sha = subprocess.check_output(
            ["git", "--git-dir", str(self.remote), "commit-tree", tree, "-m", "test"],
            env=env, text=True).strip()
        self.ref("main", self.sha)
        self.store = self.base / "store"
        self.store.mkdir()
        self.patch = patch.object(r, "STORE", self.store)
        self.patch.start()
        self.addCleanup(self.patch.stop)
        self.roots = r.Roots(self.base / "roots", self.command)
        self.offline = False
        self.counter = 0

    def ref(self, branch, sha):
        git("--git-dir", self.remote, "update-ref", f"refs/heads/{branch}", sha)

    def command(self, *args):
        if args[0] == "git":
            if self.offline:
                raise subprocess.CalledProcessError(1, args)
            return git(*(str(self.remote) if arg == r.REMOTE else arg for arg in args[1:]))
        self.assertEqual(args[:2], ("nix", "path-info"))
        return args[2]

    def build(self):
        self.counter += 1
        output = self.store / (f"{self.counter:032d}-system")
        output.mkdir()
        stage = self.roots.stage()
        stage.symlink_to(output)
        return stage

    def promote(self, stage, branch="main", host="mini", sha=None):
        return self.roots.promote(branch, host, sha or self.sha, stage)

    def pointer(self, branch="main", host="mini"):
        identity = r.hashlib.sha256(f"refs/heads/{branch}".encode()).hexdigest()
        return self.roots.base / "branches" / identity / host

    def test_replacement_releases_only_old_registered_link(self):
        first, second, unrelated = self.build(), self.build(), self.build()
        self.promote(first)
        self.promote(second)
        self.assertEqual(self.pointer().resolve(), second.parent)
        self.assertFalse(first.is_symlink())
        self.assertTrue(second.exists())
        self.assertTrue(unrelated.exists())
        self.promote(second)  # Idempotent retry; no mutable manifest.

    def test_failure_before_promotion_preserves_previous_success(self):
        first = self.build()
        self.promote(first)
        incomplete = self.roots.stage()
        with self.assertRaises(ValueError):
            self.promote(incomplete)
        self.assertEqual(self.pointer().resolve(), first.parent)
        self.assertTrue(first.exists())

    def test_stale_deleted_and_unavailable_remote_preserve_previous(self):
        first, second = self.build(), self.build()
        self.promote(first)
        for mode in ("stale", "deleted", "offline"):
            with self.subTest(mode=mode):
                self.ref("main", self.sha)
                self.offline = mode == "offline"
                if mode == "deleted":
                    git("--git-dir", self.remote, "update-ref", "-d", "refs/heads/main")
                with self.assertRaises((ValueError, subprocess.CalledProcessError)):
                    self.promote(second, sha="0" * 40 if mode == "stale" else self.sha)
                self.assertEqual(self.pointer().resolve(), first.parent)
                self.assertTrue(first.exists())
                self.assertTrue(second.exists())
        self.offline = False
        self.ref("main", self.sha)  # recreation may be built/promoted normally
        self.promote(second)

    def test_safe_branch_identity_and_independent_hosts(self):
        for branch in ("feature/a", "révision/雪", "main"):
            self.ref(branch, self.sha)
            for host in r.HOSTS:
                stage = self.build()
                self.promote(stage, branch, host)
                self.assertRegex(self.pointer(branch, host).parent.name, r"^[0-9a-f]{64}$")
        for branch in ("../escape", "a/../../x", "a\nb", "a*", "bad:ref"):
            with self.assertRaises(subprocess.CalledProcessError):
                self.promote(self.build(), branch)

    def test_corrupt_old_root_fails_closed_even_for_other_valid_output(self):
        first, second = self.build(), self.build()
        self.promote(first)
        original = os.readlink(first)
        for target in (os.readlink(second), str(self.base), "/nix/store/missing"):
            first.unlink()
            first.symlink_to(target)
            with self.assertRaises(ValueError):
                self.promote(second)
            self.assertEqual(self.pointer().resolve(), first.parent)
            self.assertTrue(second.exists())
        first.unlink()
        first.symlink_to(original)
        provenance = first.parent / "provenance.json"
        for contents in ("{invalid", "null", "[]", '{"commit": 123}'):
            provenance.write_text(contents)
            with self.assertRaises(ValueError):
                self.promote(second)
        self.assertTrue(first.exists())

    def test_invalid_new_targets_and_paths_do_not_replace_success(self):
        first = self.build()
        self.promote(first)
        for target in (self.base, self.store / "missing", self.store / ("a" * 32 + "-missing")):
            stage = self.roots.stage()
            stage.symlink_to(target)
            with self.assertRaises(ValueError):
                self.promote(stage)
        with self.assertRaises(ValueError):
            self.promote(first, sha="HEAD")
        with self.assertRaises(ValueError):
            self.promote(self.base / "escape" / "result")
        self.assertTrue(first.exists())

    def test_new_link_corruption_during_remote_query_keeps_prior_selection(self):
        first, second, other = self.build(), self.build(), self.build()
        self.promote(first)
        run = self.roots.run
        def corrupt(*args):
            result = run(*args)
            if args[:2] == ("git", "ls-remote"):
                second.unlink()
                second.symlink_to(other.resolve())
            return result
        self.roots.run = corrupt
        with self.assertRaises(ValueError):
            self.promote(second)
        self.assertEqual(self.pointer().resolve(), first.parent)
        self.assertTrue(first.exists())

    def test_new_link_corruption_during_old_validation_never_releases_old(self):
        first, second, other = self.build(), self.build(), self.build()
        self.promote(first)
        run = self.roots.run
        queried = False
        def corrupt(*args):
            nonlocal queried
            result = run(*args)
            if args[:2] == ("git", "ls-remote"):
                queried = True
            if queried and args == ("nix", "path-info", str(first.resolve())):
                second.unlink()
                second.symlink_to(other.resolve())
            return result
        self.roots.run = corrupt
        with self.assertRaises(ValueError):
            self.promote(second)
        self.assertEqual(self.pointer().resolve(), first.parent)
        self.assertTrue(first.exists())

    def test_postpublication_new_corruption_restores_previous_pointer(self):
        first, second, other = self.build(), self.build(), self.build()
        self.promote(first)
        run = self.roots.run
        def corrupt(*args):
            result = run(*args)
            if args[0] == "nix" and self.pointer().resolve() == second.parent:
                second.unlink()
                second.symlink_to(other.resolve())
            return result
        self.roots.run = corrupt
        with self.assertRaises(ValueError):
            self.promote(second)
        self.assertEqual(self.pointer().resolve(), first.parent)
        self.assertTrue(first.exists())

    def test_parent_entries_synced_before_publication_and_old_root_release(self):
        events = []
        sync = r.sync_directory
        def record_sync(path):
            events.append(("sync", path))
            sync(path)
        first = self.build()
        with patch.object(r, "sync_directory", side_effect=record_sync):
            second = self.build()
        self.assertIn(("sync", second.parent.parent), events)
        replace, unlink = os.replace, Path.unlink
        def record_replace(source, target):
            events.append(("publish", target))
            replace(source, target)
        def record_unlink(path, *args, **kwargs):
            events.append(("unlink", path))
            unlink(path, *args, **kwargs)
        with patch.object(r, "sync_directory", side_effect=record_sync), \
                patch.object(r.os, "replace", side_effect=record_replace):
            self.promote(first)  # Creates the new branch-ref directory.
        self.assertLess(events.index(("sync", self.pointer().parent.parent)),
                        events.index(("publish", self.pointer())))
        events.clear()
        with patch.object(r, "sync_directory", side_effect=record_sync), \
                patch.object(r.os, "replace", side_effect=record_replace), \
                patch.object(Path, "unlink", record_unlink):
            self.promote(second)
        published = events.index(("publish", self.pointer()))
        released = events.index(("unlink", first))
        for path in (second.parent, second.parent.parent, self.pointer().parent.parent):
            self.assertLess(events.index(("sync", path)), published)
        self.assertLess(events.index(("sync", self.pointer().parent)), released)

    def test_crash_after_pointer_swap_keeps_both_registered_links(self):
        first, second = self.build(), self.build()
        self.promote(first)
        sync = r.sync_directory
        def interrupted(path):
            if path == self.pointer().parent and self.pointer().resolve() == second.parent:
                raise OSError("interrupted")
            sync(path)
        with patch.object(r, "sync_directory", side_effect=interrupted):
            # Interrupt after pointer publication, before old-root cleanup.
            with self.assertRaises(OSError):
                self.promote(second)
        self.assertEqual(self.pointer().resolve(), second.parent)
        self.assertTrue(first.exists())
        self.assertTrue(second.exists())
        self.promote(second)
        self.assertEqual(self.pointer().resolve(), second.parent)


@unittest.skipUnless(os.environ.get("NIX_CACHE_ROOTS_REAL_NIX") == "1" and shutil.which("nix"),
                     "set NIX_CACHE_ROOTS_REAL_NIX=1 for isolated-store GC")
class RealNix(Roots):
    # Inherit the same corruption/stale/path tests with real Nix validation.
    def setUp(self):
        super().setUp()
        self.build_dir = self.base / "build-tmp"
        self.build_dir.mkdir()
        self.store_url = f"local?store={self.store}&state={self.base}/nix-state&log={self.base}/nix-log"

    def command(self, *args):
        if args[0] == "git":
            return super().command(*args)
        return self.nix(*args[1:])

    def nix(self, *args):
        return subprocess.check_output([
            "nix", "--store", self.store_url, "--option", "sandbox", "false",
            "--option", "build-users-group", "", "--option", "substituters", "",
            "--option", "build-dir", str(self.build_dir), *map(str, args)],
            env={**os.environ, "TMPDIR": str(self.build_dir)}, text=True).strip()

    def build(self, fail=False):
        self.counter += 1
        stage = self.roots.stage()
        expression = '''builtins.derivation {
          name = "tiny-NUM"; system = "x86_64-linux"; builder = "/bin/sh";
          args = [ "-c" "SCRIPT" ];
        }'''.replace("NUM", str(self.counter)).replace("SCRIPT", "exit 1" if fail else "echo tiny > $out")
        self.nix("build", "--expr", expression, "--out-link", stage)
        return stage

    def gc(self):
        self.nix("store", "gc")  # Explicit disposable URL, never production GC.

    def test_real_gc_retention_replacement_failed_build_checkout_removal(self):
        first = self.build()
        first_output = first.resolve()
        self.promote(first)
        second = self.build()
        second_output = second.resolve()
        self.gc()  # Both permanent and in-flight roots survive.
        self.assertTrue(first_output.exists())
        self.assertTrue(second_output.exists())
        checkout = self.base / "ephemeral-checkout"
        checkout.mkdir()
        checkout.rmdir()
        self.promote(second)
        self.gc()
        self.assertFalse(first_output.exists())
        self.assertTrue(second_output.exists())
        with self.assertRaises(subprocess.CalledProcessError):
            self.build(fail=True)
        self.gc()
        self.assertEqual(self.pointer().resolve(), second.parent)
        self.assertTrue(second_output.exists())
        # Moving an indirect root breaks its auto registration. Production never
        # does this: branch pointers move, registered staging links stay put.
        moved = second.with_name("moved-result")
        second.rename(moved)
        self.gc()
        self.assertFalse(second_output.exists())


if __name__ == "__main__":
    unittest.main()
