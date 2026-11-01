#!/usr/bin/env python3
"""Run with python3 modules/tests/test-t3code.py. Evaluates Nix; builds/activates nothing."""

import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
TMP = ROOT / ".private/tmp"
TMP.mkdir(parents=True, exist_ok=True)
EXPR = f"""
let flake = builtins.getFlake {json.dumps('git+file://' + str(ROOT) + '?dir=hosts/chungus')};
in import {ROOT / 'modules/tests/t3code.nix'} {{ inherit flake; }}
"""


class T3CodeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.fixtures = json.loads(subprocess.check_output(
            ["nix", "eval", "--impure", "--json", "--expr", EXPR], cwd=ROOT, text=True
        ))

    def test_module_contracts(self):
        self.assertEqual(self.fixtures["checks"], "passed")

    def test_desktop_activation(self):
        for mode, desired in [("enabled", True), ("disabled", False)]:
            with self.subTest(mode=mode), tempfile.TemporaryDirectory(dir=TMP) as home:
                script = self.fixtures["activation"][mode]
                # Equivalent installed tools allow testing without realizing packages.
                for tool in ("jq", "cat"):
                    executable = shutil.which(tool)
                    self.assertIsNotNone(executable, f"{tool} must be installed")
                    script = re.sub(r"/nix/store/[^/]+/bin/" + tool, executable, script)
                subprocess.run(["bash", "-n"], input=script, text=True, check=True)
                env = dict(os.environ, HOME=home)
                env.pop("DRY_RUN", None)
                settings = Path(home) / ".t3/userdata/desktop-settings.json"

                def activate(dry_run=False, check=True):
                    invocation_env = dict(env)
                    if dry_run:
                        invocation_env["DRY_RUN"] = "1"
                    return subprocess.run(
                        ["bash", "-e", "-c", 'run() { if [[ ! -v DRY_RUN ]]; then "$@"; fi; };\n' + script],
                        env=invocation_env, check=check, capture_output=True, text=True,
                    )

                activate(dry_run=True)
                self.assertFalse(settings.exists())
                activate()
                self.assertEqual(json.loads(settings.read_text()), {"localEnvironmentEnabled": desired})
                settings.write_text(json.dumps({"localEnvironmentEnabled": not desired, "theme": "dark"}))
                previous = settings.read_bytes()
                activate(dry_run=True)
                self.assertEqual(settings.read_bytes(), previous)
                activate()
                self.assertEqual(json.loads(settings.read_text()), {"localEnvironmentEnabled": desired, "theme": "dark"})
                previous = settings.read_bytes()
                activate()
                self.assertEqual(settings.read_bytes(), previous)
                # Invalid existing settings must fail safely, never be overwritten.
                settings.write_text("{invalid json")
                self.assertNotEqual(activate(check=False).returncode, 0)
                self.assertEqual(settings.read_text(), "{invalid json")
                self.assertEqual(list(settings.parent.glob(".desktop-settings.*")), [])


if __name__ == "__main__":
    unittest.main()
