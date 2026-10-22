"""Exercise the real runner CLI with fake disk commands; never access an array.

Usage: python3 test_snapraid_btrfs_runner.py /path/to/snapraid-btrfs-runner.py
"""

import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


RUNNER = Path(sys.argv.pop(1)).resolve()


class RunnerSafetyTests(unittest.TestCase):
    def run_runner(self, *, diff="", fail_sync=False, cleanup=True):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            calls = root / "calls"
            wrapper = root / "fake-wrapper"
            wrapper.write_text(
                f"#!{sys.executable}\n"
                "import os, sys\n"
                f"commands = {{'diff', 'sync', 'pool', 'cleanup', 'scrub'}}\n"
                "command = next(arg for arg in sys.argv[1:] if arg in commands)\n"
                f"with open({str(calls)!r}, 'a') as out: out.write(command + '\\n')\n"
                f"if command == 'diff': print({diff!r}); sys.exit(2 if {bool(diff)!r} else 0)\n"
                f"if command == 'sync' and {fail_sync!r}: sys.exit(1)\n"
            )
            wrapper.chmod(0o700)
            array_config = root / "array.conf"
            array_config.touch()
            config = root / "runner.conf"
            config.write_text(
                "[snapraid-btrfs]\n"
                f"executable = {wrapper}\n"
                f"cleanup = {str(cleanup).lower()}\n"
                "pool = true\n"
                "[snapraid]\n"
                f"executable = {sys.executable}\n"
                f"config = {array_config}\n"
                "deletethreshold = 1\n"
                "[snapper]\n"
                f"executable = {sys.executable}\n"
                "[scrub]\nenabled = true\nplan = full\n"
                "[logging]\n[email]\n[smtp]\n"
            )
            result = subprocess.run(
                [sys.executable, str(RUNNER), "-c", str(config)],
                capture_output=True, text=True, timeout=15,
                env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"},
            )
            return result, calls.read_text().splitlines()

    def test_empty_diff_still_syncs_before_cleanup(self):
        result, calls = self.run_runner()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(calls, ["diff", "sync", "pool", "cleanup", "scrub"])

    def test_failed_sync_blocks_all_post_sync_commands(self):
        for diff in ("", "add file"):
            with self.subTest(diff=diff):
                result, calls = self.run_runner(diff=diff, fail_sync=True)
                self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
                self.assertEqual(calls, ["diff", "sync"])

    def test_changed_files_still_sync(self):
        result, calls = self.run_runner(diff="add file")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(calls, ["diff", "sync", "pool", "cleanup", "scrub"])

    def test_delete_threshold_blocks_sync_and_cleanup(self):
        result, calls = self.run_runner(diff="remove first\nremove second")
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertEqual(calls, ["diff"])

    def test_cleanup_can_remain_disabled(self):
        result, calls = self.run_runner(cleanup=False)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(calls, ["diff", "sync", "pool", "scrub"])


if __name__ == "__main__":
    unittest.main()
