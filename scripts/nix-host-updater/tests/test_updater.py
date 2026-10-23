import concurrent.futures
import importlib.util
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("updater", HERE.parent / "updater.py")
u = importlib.util.module_from_spec(spec)
spec.loader.exec_module(u)
TMP = HERE.parents[2] / ".private" / "tmp"
TMP.mkdir(parents=True, exist_ok=True)


def git(*args, cwd=None):
    return subprocess.check_output(["git", *args], cwd=cwd, stderr=subprocess.DEVNULL).decode().strip()


class Crash(BaseException):
    pass


class Commands:
    """Real Git to a local authoritative bare repo; fake Nix in a subprocess."""

    def __init__(self, remote, control):
        self.remote = remote
        self.control = control
        self.real = u.Commands()
        self.offline = False

    def __call__(self, args, **kwargs):
        if args[0] == "git":
            if self.offline and "ls-remote" in args:
                raise u.Error("authoritative remote unavailable")
            args = [str(self.remote) if arg == u.REMOTE else arg for arg in args]
            # Test-only transport override; production permits SSH only.
            args[1:1] = ["-c", "protocol.file.allow=always"]
        else:
            assert args[0] == "nix"
            args = [sys.executable, str(HERE / "fake_nix.py"), str(self.control), *args[1:]]
        return self.real(args, **kwargs)


class Builds(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(dir=TMP)
        self.addCleanup(self.temporary.cleanup)
        self.base = Path(self.temporary.name)
        self.repo = self.base / "repo"
        self.repo.mkdir()
        git("init", "-q", "-b", "main", cwd=self.repo)
        git("config", "user.email", "test@example.invalid", cwd=self.repo)
        git("config", "user.name", "Test", cwd=self.repo)
        (self.repo / "shared.txt").write_text("full repository\n")
        for host in u.HOSTS:
            directory = self.repo / "hosts" / host
            directory.mkdir(parents=True)
            (directory / "flake.lock").write_text("{}\n")
            (directory / "flake.nix").write_text("{}\n")
        git("add", ".", cwd=self.repo)
        git("commit", "-qm", "initial", cwd=self.repo)
        self.sha = git("rev-parse", "HEAD", cwd=self.repo)
        self.remote = self.base / "remote.git"
        git("clone", "--bare", str(self.repo), str(self.remote))
        self.store = self.base / "store"
        self.store.mkdir()
        self.patch = patch.object(u, "STORE_DIR", str(self.store))
        self.patch.start()
        self.addCleanup(self.patch.stop)
        self.state = self.base / "state with spaces"
        self.next_control = 0
        self.updater, self.commands, self.control = self.helper()

    def helper(self, hook=None, **control):
        self.next_control += 1
        path = self.base / f"control{self.next_control}.json"
        control.setdefault("output", str(self.store / (f"{self.next_control:032d}" + "-system")))
        path.write_text(json.dumps(control))
        commands = Commands(self.remote, path)
        return u.Updater(self.state, commands, hook), commands, path

    def change(self, **control):
        value = json.loads(self.control.read_text())
        value.update(control)
        self.control.write_text(json.dumps(value))

    def advance(self, branch="main"):
        (self.repo / "revision").write_text(str(os.urandom(8)))
        git("add", ".", cwd=self.repo)
        git("commit", "-qm", "advance", cwd=self.repo)
        sha = git("rev-parse", "HEAD", cwd=self.repo)
        git("push", "-q", str(self.remote), f"HEAD:refs/heads/{branch}", cwd=self.repo)
        return sha

    def root(self, updater=None):
        return (updater or self.updater).status()["roots"][0]

    def wait_ready(self, control):
        import time
        deadline = time.monotonic() + 10
        while not control.with_suffix(".ready").exists():
            if time.monotonic() > deadline:
                self.fail("fake build did not reach synchronization point")
            time.sleep(0.01)

    def test_success_exact_head_full_layout_and_persistent_root(self):
        result = self.updater.build("main", "mini", self.sha, "job/123")
        self.assertEqual(result["status"], "succeeded", result)
        self.assertEqual(result["sha"], self.sha)
        self.assertEqual(result["check"], "succeeded")
        self.assertEqual(result["build"], "succeeded")
        self.assertEqual(result["tree"], git("rev-parse", "HEAD^{tree}", cwd=self.repo))
        root = self.root()
        self.assertFalse(root["stale"])
        self.assertTrue(root["root_consistent"])
        shutil.rmtree(self.state / "work")  # Only our disposable checkout, never roots.
        self.assertTrue(Path(root["path"]).exists())
        calls = [json.loads(line) for line in self.control.with_suffix(".calls").read_text().splitlines()]
        self.assertIn("nixosConfigurations.dogdot.config.system.build.toplevel", calls[1][1])
        self.assertIn(self.sha, calls[0][2])
        self.assertEqual(result["action_run"], "job/123")

    def test_failure_refresh_preserves_success_and_marks_stale(self):
        first = self.updater.build("main", "mini")
        previous = self.root()
        self.advance()
        self.change(fail="check")
        result = self.updater.build("main", "mini")
        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["check"], "failed")
        current = self.root()
        self.assertEqual(current["run"], first["run"])
        self.assertEqual(Path(current["path"]).resolve(), Path(previous["path"]).resolve())
        self.assertTrue(current["stale"])
        self.assertEqual(current["latest_attempt"], "failed")

    def test_failure_phases_never_create_permanent_root(self):
        for phase in ("check", "build", "closure"):
            with self.subTest(phase=phase):
                helper, _, _ = self.helper(fail=phase)
                result = helper.build("main", "lapdog")
                self.assertEqual(result["status"], "failed", result)
                self.assertEqual(helper.status()["roots"], [])
                self.assertFalse(helper.guard(result["run"]).is_symlink())

    def test_mutation_during_check_and_build_rejected(self):
        for phase in ("check", "build"):
            with self.subTest(phase=phase):
                helper, _, _ = self.helper(mutation=phase)
                result = helper.build("main", "mini")
                self.assertEqual(result["status"], "failed", result)
                self.assertIn("mutation", result["error"])
                self.assertEqual(helper.status()["roots"], [])

    def test_all_hosts_independent_after_failure(self):
        failed, _, _ = self.helper(fail="build")
        self.assertEqual(failed.build("main", "mini")["status"], "failed")
        for host in ("lapdog", "chungus"):
            helper, _, _ = self.helper()
            self.assertEqual(helper.build("main", host)["status"], "succeeded")
        self.assertEqual({r["host"] for r in self.updater.status()["roots"]}, {"lapdog", "chungus"})

    def test_any_existing_branch_safe_hashed_identity_no_six_root_limit(self):
        for branch in ("main", "feature/a", "feature/b", "révision/雪", "automation/nix-update/mini", "other", "seventh"):
            git("update-ref", f"refs/heads/{branch}", self.sha, cwd=self.remote)
            helper, _, _ = self.helper()
            self.assertEqual(helper.build(branch, "mini")["status"], "succeeded")
        roots = self.updater.status()["roots"]
        self.assertEqual(len(roots), 7)
        for root in roots:
            self.assertRegex(Path(root["path"]).parent.name, r"^[0-9a-f]{64}$")
        git("update-ref", "refs/heads/unbuilt", self.sha, cwd=self.remote)
        self.assertEqual(len(self.updater.status()["roots"]), 7)

    def test_unsafe_branch_names_and_invalid_sha(self):
        for branch in ("../escape", "a/../../x", "a\nb", "a*", "a?", "x.lock", "bad:ref"):
            with self.subTest(branch=branch), self.assertRaises(u.Error):
                self.updater.build(branch, "mini")
        with self.assertRaises(u.Error):
            self.updater.build("main", "mini", "HEAD")
        self.assertEqual(self.updater.status()["roots"], [])

    def test_synthetic_merge_and_non_head_commit_rejected(self):
        new = self.advance()
        result = self.updater.build("main", "mini", self.sha)
        self.assertEqual(result["status"], "superseded")
        self.assertFalse(self.control.with_suffix(".calls").exists())
        self.assertEqual(self.updater.build("main", "mini", new)["status"], "succeeded")

    def test_branch_drift_delete_or_remote_failure_before_promotion(self):
        for kind in ("advance", "delete", "offline"):
            git("update-ref", "refs/heads/main", self.sha, cwd=self.remote)
            helper, commands, control = self.helper(wait="build")
            with concurrent.futures.ThreadPoolExecutor() as pool:
                future = pool.submit(helper.build, "main", "mini")
                self.wait_ready(control)
                if kind == "advance":
                    self.advance()
                elif kind == "delete":
                    git("update-ref", "-d", "refs/heads/main", cwd=self.remote)
                else:
                    commands.offline = True
                control.with_suffix(".release").touch()
                result = future.result(timeout=10)
            self.assertEqual(result["status"], "failed" if kind == "offline" else "superseded", result)
            self.assertEqual(helper.status()["roots"], [])

    def test_parallel_old_completion_cannot_regress_new_success_even_after_force_push(self):
        old, _, control = self.helper(wait="build")
        with concurrent.futures.ThreadPoolExecutor() as pool:
            future = pool.submit(old.build, "main", "mini")
            self.wait_ready(control)
            live = old.status()["runs"][-1]
            self.assertEqual(live["status"], "building")
            new_sha = self.advance()
            newer, _, _ = self.helper()
            newer_result = newer.build("main", "mini", new_sha)
            self.assertEqual(newer_result["status"], "succeeded", newer_result)
            git("update-ref", "refs/heads/main", self.sha, cwd=self.remote)
            control.with_suffix(".release").touch()
            result = future.result(timeout=10)
        self.assertEqual(result["status"], "superseded")
        self.assertEqual(self.root()["sha"], new_sha)

    def test_concurrent_hosts_do_not_hold_publication_lock_during_build(self):
        waiting, _, control = self.helper(wait="build")
        with concurrent.futures.ThreadPoolExecutor() as pool:
            future = pool.submit(waiting.build, "main", "mini")
            self.wait_ready(control)
            try:
                other, _, _ = self.helper()
                self.assertEqual(other.build("main", "lapdog")["status"], "succeeded")
                self.assertEqual(len(waiting.status()["roots"]), 1)
            finally:
                control.with_suffix(".release").touch()
            self.assertEqual(future.result(timeout=10)["status"], "succeeded")
        self.assertEqual(len(waiting.status()["roots"]), 2)

    def test_head_change_between_observation_and_fetch(self):
        original = self.commands.__call__
        def commands(args, **kwargs):
            if "fetch" in args:
                self.advance()
            return original(args, **kwargs)
        helper = u.Updater(self.state, commands)
        result = helper.build("main", "mini", self.sha)
        self.assertEqual(result["status"], "superseded")
        self.assertIn("during fetch", result["error"])
        self.assertFalse(self.control.with_suffix(".calls").exists())

    def test_successful_replacement_releases_only_previous_guard(self):
        first = self.updater.build("main", "mini")
        unrelated = self.state / "unrelated-root"
        unrelated.symlink_to(self.store)
        newer, _, _ = self.helper()
        result = newer.build("main", "mini")
        self.assertEqual(result["status"], "succeeded")
        self.assertFalse(newer.guard(first["run"]).is_symlink())
        self.assertTrue(newer.guard(result["run"]).exists())
        self.assertTrue(unrelated.is_symlink())

    def test_crashes_around_root_and_manifest_are_recoverable(self):
        first = self.updater.build("main", "mini")
        for phase in ("after_journal", "after_root", "after_metadata", "after_old_cleanup"):
            with self.subTest(phase=phase):
                old = self.root()
                def crash(at):
                    if at == phase:
                        raise Crash()
                helper, _, _ = self.helper(hook=crash)
                with self.assertRaises(Crash):
                    helper.build("main", "mini")
                status = self.updater.status()
                self.assertIsNotNone(status["journal"])
                run_id = status["journal"]["new"]["run"]
                self.assertTrue(helper.guard(run_id).exists())
                recovered = self.updater.recover()
                self.assertIsNone(recovered["journal"])
                root = recovered["roots"][0]
                self.assertTrue(root["root_consistent"])
                self.assertEqual(root["run"], old["run"] if phase == "after_journal" else run_id)
                self.assertTrue(helper.guard(root["run"]).exists())
        self.assertFalse(self.updater.guard(first["run"]).is_symlink())

    def test_crash_first_promotion_before_root_does_not_manufacture_success(self):
        def crash(phase):
            if phase == "after_journal":
                raise Crash()
        helper, _, _ = self.helper(hook=crash)
        with self.assertRaises(Crash):
            helper.build("main", "mini")
        status = self.updater.recover()
        self.assertEqual(status["roots"], [])
        self.assertEqual(status["runs"][0]["status"], "interrupted")

    def test_recovery_rejects_corrupted_guard_after_root_publication(self):
        first = self.updater.build("main", "mini")
        old_guard = self.updater.guard(first["run"])
        old_target = os.readlink(old_guard)
        def crash(phase):
            if phase == "after_root":
                raise Crash()
        helper, _, _ = self.helper(hook=crash)
        with self.assertRaises(Crash):
            helper.build("main", "mini")
        journal = self.updater.status()["journal"]
        new_guard = self.updater.guard(journal["new"]["run"])
        recorded_output = journal["new"]["output"]
        manifest = (self.state / "manifest.json").read_bytes()
        for kind, target in (("other-store-output", old_target),
                             ("unrelated-directory", str(self.base)),
                             ("non-symlink", None)):
            with self.subTest(kind=kind):
                new_guard.unlink()
                if target is None:
                    new_guard.mkdir()
                else:
                    new_guard.symlink_to(target)
                try:
                    self.assertFalse(self.root()["root_consistent"])
                    self.assertTrue(self.root()["stale"])
                    with self.assertRaisesRegex(u.Error, "mismatch"):
                        self.updater.recover()
                    self.assertEqual((self.state / "manifest.json").read_bytes(), manifest)
                    self.assertTrue(old_guard.is_symlink())
                    self.assertEqual(os.readlink(old_guard), old_target)
                    self.assertTrue(new_guard.exists())
                    self.assertEqual(self.updater.status()["journal"], journal)
                finally:
                    if new_guard.is_symlink():
                        new_guard.unlink()
                    else:
                        new_guard.rmdir()
                    new_guard.symlink_to(recorded_output)
        recovered = self.updater.recover()
        self.assertTrue(recovered["roots"][0]["root_consistent"])
        self.assertFalse(old_guard.is_symlink())

    def test_status_rejects_guard_not_directly_targeting_recorded_output(self):
        result = self.updater.build("main", "mini")
        guard = self.updater.guard(result["run"])
        other_output = self.store / ("9" * 32 + "-other-system")
        other_output.mkdir()
        for target in (other_output, self.base, None):
            with self.subTest(target=target):
                guard.unlink()
                if target is None:
                    guard.mkdir()
                else:
                    guard.symlink_to(target)
                try:
                    root = self.root()
                    self.assertFalse(root["root_consistent"])
                    self.assertTrue(root["stale"])
                finally:
                    if guard.is_symlink():
                        guard.unlink()
                    else:
                        guard.rmdir()
                    guard.symlink_to(result["output"])

    def test_guard_revalidated_before_old_guard_cleanup(self):
        first = self.updater.build("main", "mini")
        old_guard = self.updater.guard(first["run"])
        def corrupt(phase):
            if phase == "after_metadata":
                journal = json.loads((self.state / "manifest.json").read_text())["journal"]
                guard = self.updater.guard(journal["new"]["run"])
                guard.unlink()
                guard.symlink_to(self.base)
        helper, _, _ = self.helper(hook=corrupt)
        result = helper.build("main", "mini")
        self.assertTrue(result["recovery_required"])
        self.assertIn("mismatch", result["publication_error"])
        self.assertTrue(old_guard.exists())
        self.assertFalse(self.root()["root_consistent"])
        self.assertTrue(self.root()["stale"])
        with self.assertRaisesRegex(u.Error, "mismatch"):
            self.updater.recover()
        self.assertTrue(old_guard.exists())
        self.assertTrue(helper.guard(result["run"]).is_symlink())
        self.assertIsNotNone(self.updater.status()["journal"])

    def test_status_is_local_and_reports_interrupted_and_missing_output(self):
        result = self.updater.build("main", "mini")
        self.commands.offline = True
        self.assertIn("remote not queried", self.updater.status()["freshness"])
        self.updater.guard(result["run"]).unlink()
        root = self.root()
        self.assertTrue(root["stale"])
        self.assertFalse(root["root_consistent"])

    def child(self, crash_phase=""):
        code = '''
import os,sys
from pathlib import Path
sys.path.insert(0, sys.argv[1])
import test_updater as f
f.u.STORE_DIR = sys.argv[5]
def hook(phase):
    if phase == sys.argv[6]:
        os._exit(99)
commands = f.Commands(Path(sys.argv[3]), Path(sys.argv[4]))
f.u.Updater(sys.argv[2], commands, hook).build("main", "mini")
'''
        return subprocess.Popen([sys.executable, "-c", code, str(HERE), str(self.state),
                                 str(self.remote), str(self.control), str(self.store), crash_phase],
                                start_new_session=True, stdout=subprocess.DEVNULL,
                                stderr=subprocess.PIPE, env={**os.environ, "TMPDIR": str(self.base)})

    def test_process_exit_during_promotion_releases_lock_and_recovers(self):
        process = self.child("after_root")
        _, stderr = process.communicate(timeout=10)
        self.assertEqual(process.returncode, 99, stderr.decode())
        status = self.updater.status()
        self.assertEqual(status["runs"][0]["status"], "interrupted")
        self.assertTrue(status["runs"][0]["recovery_required"])
        recovered = self.updater.recover()
        self.assertEqual(recovered["runs"][0]["status"], "succeeded")
        self.assertTrue(recovered["roots"][0]["root_consistent"])

    def test_cancelled_controller_reports_interrupted_not_failed_or_success(self):
        self.change(wait="build")
        process = self.child()
        try:
            self.wait_ready(self.control)
            self.assertEqual(self.updater.status()["runs"][0]["status"], "building")
        finally:
            os.killpg(process.pid, signal.SIGTERM)
            process.communicate(timeout=10)
        self.assertEqual(self.updater.status()["runs"][0]["status"], "interrupted")
        recovered = self.updater.recover()
        self.assertEqual(recovered["runs"][0]["status"], "interrupted")
        self.assertEqual(recovered["roots"], [])

    def test_corrupt_manifest_or_root_mismatch_fails_closed(self):
        self.updater.build("main", "mini")
        root = Path(self.root()["path"])
        def crash(phase):
            if phase == "after_journal":
                raise Crash()
        helper, _, _ = self.helper(hook=crash)
        with self.assertRaises(Crash):
            helper.build("main", "mini")
        root.unlink()
        root.symlink_to(self.base)
        with self.assertRaisesRegex(u.Error, "mismatch"):
            self.updater.recover()
        self.assertTrue(root.is_symlink())
        manifest = self.state / "manifest.json"
        manifest.write_text("{invalid")
        with self.assertRaisesRegex(u.Error, "invalid manifest"):
            self.updater.status()

    def test_private_state_required(self):
        other = self.base / "public"
        other.mkdir(mode=0o755)
        with self.assertRaises(u.Error):
            u.Updater(other)
        link = self.base / "link"
        link.symlink_to(self.state)
        with self.assertRaises(u.Error):
            u.Updater(link)


class Environment(unittest.TestCase):
    def test_commands_do_not_inherit_credentials_or_git_nix_overrides(self):
        with patch.dict(os.environ, {"NIX_CONFIG": "secret-key-files = private",
                                     "GIT_CONFIG_COUNT": "1", "FORGEJO_TOKEN": "secret",
                                     "AWS_SECRET_ACCESS_KEY": "secret"}):
            output = u.Commands()([sys.executable, "-c", "import os,json; print(json.dumps(dict(os.environ)))"])
        env = json.loads(output)
        for key in ("NIX_CONFIG", "GIT_CONFIG_COUNT", "FORGEJO_TOKEN", "AWS_SECRET_ACCESS_KEY"):
            self.assertNotIn(key, env)
        self.assertIn("StrictHostKeyChecking=yes", env["GIT_SSH_COMMAND"])


if __name__ == "__main__":
    unittest.main()
