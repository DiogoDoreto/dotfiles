"""Opt-in actual builds/GC, strictly confined to a disposable local store."""

import json
import os
from pathlib import Path
import shutil
import subprocess
import unittest

import test_updater as fixtures

u = fixtures.u
git = fixtures.git


class RealCommands(fixtures.Commands):
    def __init__(self, remote, control, store_url, build_dir):
        super().__init__(remote, control)
        self.store_url = store_url
        self.build_dir = build_dir

    def __call__(self, args, **kwargs):
        if args[0] == "git":
            return super().__call__(args, **kwargs)
        return self.real([
            "nix", "--store", self.store_url,
            "--option", "sandbox", "false", "--option", "build-users-group", "",
            "--option", "substituters", "", "--option", "build-dir", str(self.build_dir),
            *args[1:],
        ], **kwargs)


@unittest.skipUnless(os.environ.get("NIX_HOST_UPDATER_REAL_NIX") == "1" and shutil.which("nix"),
                     "set NIX_HOST_UPDATER_REAL_NIX=1 for isolated-store builds/GC")
class RealNix(unittest.TestCase):
    def setUp(self):
        # Reuse only the local Git/private temporary-directory fixture, not its
        # fake-build tests. There is no access to the authoritative live remote.
        self.fixture = fixtures.Builds(methodName="test_success_exact_head_full_layout_and_persistent_root")
        self.fixture.setUp()
        self.addCleanup(self.fixture.doCleanups)
        f = self.fixture
        self.build_dir = f.base / "build-tmp"
        self.build_dir.mkdir()
        self.store_url = f"local?store={f.store}&state={f.base}/nix-state&log={f.base}/nix-log"
        self.commands = RealCommands(f.remote, f.control, self.store_url, self.build_dir)
        self.updater = u.Updater(f.state, self.commands)
        self.write_flakes("tiny-one")

    def write_flakes(self, name, fail=False):
        f = self.fixture
        for host, output in u.HOSTS.items():
            (f.repo / "hosts" / host / "flake.nix").write_text('''{
  outputs = { self }: {
    nixosConfigurations.OUTPUT.config.system.build.toplevel = builtins.derivation {
      name = "NAME";
      system = "x86_64-linux";
      builder = "/bin/sh";
      args = [ "-c" "SCRIPT" ];
    };
  };
}
'''.replace("OUTPUT", output).replace("NAME", name).replace("SCRIPT", "exit 1" if fail else "echo tiny > $out"))
            (f.repo / "hosts" / host / "flake.lock").write_text(
                json.dumps({"nodes": {"root": {}}, "root": "root", "version": 7}) + "\n")
        git("add", ".", cwd=f.repo)
        git("commit", "-qm", name, cwd=f.repo)
        git("push", "-q", str(f.remote), "HEAD:refs/heads/main", cwd=f.repo)

    def gc(self):
        # This explicit URL is mandatory: never fall through to the daemon store.
        result = subprocess.run(["nix", "--store", self.store_url, "store", "gc"],
                                capture_output=True, text=True,
                                env={**os.environ, "TMPDIR": str(self.build_dir)})
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_registered_retention_replacement_failure_and_crash_under_real_gc(self):
        first = self.updater.build("main", "mini")
        self.assertEqual(first["status"], "succeeded", first)
        first_output = Path(first["output"])
        self.gc()
        self.assertTrue(first_output.exists())
        # The temporary build link remains registered before any branch pointer
        # is replaced, and both old/new closures survive a concurrent real GC.
        from test_updater import Crash
        self.write_flakes("tiny-two")
        def crash(phase):
            if phase == "after_journal":
                raise Crash()
        interrupted = u.Updater(self.fixture.state, self.commands, crash)
        with self.assertRaises(Crash):
            interrupted.build("main", "mini")
        journal = self.updater.status()["journal"]
        new_output = Path(journal["new"]["output"])
        self.gc()
        self.assertTrue(first_output.exists())
        self.assertTrue(new_output.exists())
        self.updater.recover()
        second = self.updater.build("main", "mini")
        self.assertEqual(second["status"], "succeeded", second)
        # The discarded interrupted operation intentionally remains rooted for
        # investigation, so it cannot make a root-replacement assertion pass.
        self.write_flakes("tiny-three")
        third = self.updater.build("main", "mini")
        self.assertEqual(third["status"], "succeeded", third)
        shutil.rmtree(self.fixture.state / "work")
        self.gc()
        self.assertFalse(first_output.exists())
        self.assertTrue(Path(third["output"]).exists())
        self.write_flakes("tiny-failed", fail=True)
        failed = self.updater.build("main", "mini")
        self.assertEqual(failed["status"], "failed", failed)
        self.gc()
        self.assertTrue(Path(third["output"]).exists())
        self.assertTrue(self.updater.status()["roots"][0]["stale"])


if __name__ == "__main__":
    unittest.main()
