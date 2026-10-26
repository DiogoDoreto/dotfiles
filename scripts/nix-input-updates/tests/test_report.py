"""Grouped closure reporting, size accounting and failure-path regressions."""

import json
import os
import sys
import unittest
from unittest.mock import patch

import test_updates as fixtures
from test_updates import A, Temporary, report_data, store
import report as r


class Metadata(unittest.TestCase):
    def test_nix_object_and_list_formats(self):
        p = store(1, "app-1")
        for data in ({p: {"narSize": 123}}, [{"path": p, "narSize": 123}]):
            with patch.object(r, "run", return_value=json.dumps(data)) as command:
                self.assertEqual(r.closure("root"), {p: 123})
            self.assertEqual(command.call_args.args, ("nix", "path-info", "--json", "--recursive", "root"))

    def test_invalid_sizes_and_paths(self):
        p = store(1, "app-1")
        invalid = [None, {}, [], {p: None}, {p: {}}, {p: {"narSize": True}},
                   {p: {"narSize": -1}}, {p: {"narSize": 1.5}}, {p: {"narSize": "123"}},
                   {"bad": {"narSize": 100}}, [None], [{"narSize": 100}],
                   [{"path": p, "narSize": 1}, {"path": p, "narSize": 1}]]
        for data in invalid:
            with self.subTest(data=data), patch.object(r, "run", return_value=json.dumps(data)), \
                    self.assertRaises(ValueError):
                r.closure("root")


class Grouping(unittest.TestCase):
    def test_multiple_versions_counts_shared_outputs_and_size_reconciliation(self):
        shared = store(1, "lib-1")
        before = {shared: 100, store(2, "lib-1"): 200, store(3, "old-1"): 300,
                  store(4, "unit-old.service"): 0}
        after = {shared: 100, store(5, "lib-1"): 200, store(6, "lib-1"): 200,
                 store(7, "lib-2-dev"): 400, store(8, "new-1"): 500,
                 store(9, "unit-new.service"): 0}
        groups = r.grouped(before, after)
        self.assertEqual([x["name"] for x in groups["changed"]], ["lib"])
        row = groups["changed"][0]
        self.assertEqual(r.versions(row["after"], {shared}), "1 ×2, 2-dev, <unchanged>")
        self.assertEqual(row["delta"], 600)
        rows = sum(groups.values(), [])
        self.assertEqual(sum(row["delta"] for row in rows), sum(after.values()) - sum(before.values()))
        old_paths, new_paths = [], []
        for row in rows:
            old_paths.extend(set(row["before"]) - set(row["after"]))
            new_paths.extend(set(row["after"]) - set(row["before"]))
        self.assertCountEqual(old_paths, before.keys() - after.keys())
        self.assertCountEqual(new_paths, after.keys() - before.keys())
        self.assertEqual(groups, r.grouped(dict(reversed(list(before.items()))),
                                          dict(reversed(list(after.items())))))
        self.assertEqual(r.versions([store(1, "unit-test.service")], set()), "<none>")

    def test_upgrade_rebuild_and_identical(self):
        before = {store(1, "app-1"): 100, store(2, "lib-1"): 100}
        after = {store(3, "app-2"): 100, store(4, "lib-1"): 100}
        self.assertEqual(len(r.grouped(before, after)["changed"]), 2)
        self.assertFalse(any(r.grouped(before, before).values()))
        text = r.render(report_data())
        self.assertIn("1 → 2", text)
        self.assertIn("**DIFF:** 0 B", text)
        self.assertIn("(+1, -1)", text)

    def test_size_format(self):
        for value, expected in ((0, "0 B"), (100, "+100 B"), (-1024, "-1 KiB"),
                                (1024**2, "+1 MiB"), (1024**3, "+1 GiB")):
            self.assertEqual(r.size(value, signed=True), expected)


class PreparationSafety(fixtures.Preparation):
    def test_shared_path_size_mismatch_skips(self):
        with patch.object(r, "closure", side_effect=[{store(1, "system"): 100},
                                                     {store(1, "system"): 200}]), \
                self.assertRaisesRegex(ValueError, "inconsistent NAR sizes"):
            self.prepare()
        self.assertFalse((self.base / ".private/output").exists())

    def test_closed_foreign_retargeted_and_network_errors(self):
        original = self.api.pr
        for state in (dict(original, state="closed"), dict(original, merged=True),
                      dict(original, head=dict(sha=A, ref="feature", repo=dict(full_name="foreign/repo")))):
            self.api.pr = state
            with self.assertRaises(ValueError):
                self.prepare()
        self.api.pr = original
        with self.assertRaises(ValueError):
            r.pr_state(self.api, 3, A, "other")
        self.api.fail = True
        with self.assertRaises(ValueError):
            self.prepare()
        self.assertFalse(self.api.writes)


class FailureSummary(Temporary):
    def test_prepare_failure_records_reason_and_clears_stale_files(self):
        r.REPORT.mkdir(parents=True)
        for name in ("report.json", "report.md", "failure.txt"):
            (r.REPORT / name).write_text("stale")
        with patch.dict(os.environ, {"HOST": "mini"}, clear=True), \
                patch.object(sys, "argv", ["report.py", "prepare"]), patch.object(r, "warning") as warn:
            r.main()
        warn.assert_called_once()
        self.assertFalse((r.REPORT / "report.json").exists())
        self.assertFalse((r.REPORT / "report.md").exists())
        self.assertNotEqual((r.REPORT / "failure.txt").read_text(), "stale")
        with patch.dict(os.environ, {"HOST": "mini", "PREPARE_OUTCOME": "success", "REPORT_READY": ""}):
            r.summarize()
        self.assertIn("Comparison unavailable", (self.base / ".private/summary").read_text())

    def test_summary_failure_warns_without_failing_build(self):
        with patch.dict(os.environ, {}, clear=True), patch.object(sys, "argv", ["report.py", "summary"]), \
                patch.object(r, "warning") as warn:
            r.main()
        warn.assert_called_once()


if __name__ == "__main__":
    unittest.main()
