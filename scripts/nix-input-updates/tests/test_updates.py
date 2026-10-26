"""Offline tests. Git uses disposable local repos; Nix and Forgejo are fakes."""

import copy
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import Mock, patch

HERE = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(HERE))
import common as c
import update as u
import report as r

TMP = HERE.parents[1] / ".private/tmp"
TMP.mkdir(parents=True, exist_ok=True)
A, B, C = "a" * 40, "b" * 40, "c" * 40


def store(n, name):
    return f"/nix/store/{n:032d}-{name}"


class Temporary(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(dir=TMP)
        self.addCleanup(self.tmp.cleanup)
        self.base = Path(self.tmp.name)
        self.old_cwd = Path.cwd()
        os.chdir(self.base)
        self.addCleanup(os.chdir, self.old_cwd)
        (self.base / ".private").mkdir()
        self.env = patch.dict(os.environ, {"GITHUB_OUTPUT": str(self.base / ".private/output"),
                                         "GITHUB_STEP_SUMMARY": str(self.base / ".private/summary")})
        self.env.start()
        self.addCleanup(self.env.stop)


class Update(Temporary):
    def setUp(self):
        super().setUp()
        c.run("git", "init", "-b", "main")
        Path(".gitignore").write_text(".private/\n")
        for machine in c.HOSTS:
            path = Path(f"hosts/{machine}")
            path.mkdir(parents=True)
            (path / "flake.lock").write_text("old\n")
        c.run("git", "add", ".")
        c.run("git", "-c", "user.name=Test", "-c", "user.email=test@invalid", "commit", "-m", "base")
        self.sha = c.run("git", "rev-parse", "HEAD")
        self.calls = []
        self.change = True

    def command(self, *args, **kwargs):
        self.calls.append((args, kwargs))
        if args[0] == "nix":
            self.assertEqual(args[:4], ("nix", "flake", "update", "--flake"))
            if self.change:
                (Path(args[4]) / "flake.lock").write_text("new\n")
            return ""
        return c.run(*args, **kwargs)

    def test_noop_has_summary_and_publish_refuses(self):
        self.change = False
        with patch.object(u, "run", self.command):
            u.prepare("mini", self.sha)
            api = Mock()
            with self.assertRaises(ValueError):
                u.publish("mini", self.sha, "1", api, "fake")
        api.request.assert_not_called()
        self.assertIn("changed=false", Path(".private/output").read_text())
        self.assertIn("already up to date", Path(".private/summary").read_text())
        self.assertFalse(any(args[:2] == ("git", "push") for args, _ in self.calls))

    def test_changed_update_pushes_fresh_lock_only_branches_from_same_base(self):
        # This is only a local bare repository, never the real Forgejo remote.
        remote = self.base / ".private/remote.git"
        remote.parent.mkdir(exist_ok=True)
        c.run("git", "init", "--bare", str(remote))
        api = Mock()
        api.request.return_value = {"number": 7}
        branches = []
        with patch.object(u, "run", self.command), patch.object(u, "REMOTE", str(remote)):
            for machine in ("mini", "lapdog"):
                c.run("git", "checkout", "--detach", self.sha)
                u.prepare(machine, self.sha)
                u.publish(machine, self.sha, "8", api, "test-secret")
                branches.append(c.run("git", "branch", "--show-current"))
                self.assertEqual(c.run("git", "rev-parse", "HEAD^"), self.sha)
                self.assertEqual(c.run("git", "diff", "--name-only", "HEAD^", "HEAD"),
                                 f"hosts/{machine}/flake.lock")
        self.assertNotEqual(*branches)
        self.assertEqual(len(c.run("git", "--git-dir", str(remote), "for-each-ref").splitlines()), 2)
        for call in api.request.call_args_list:
            self.assertEqual(call.args[2]["base"], "main")
        for args, kwargs in self.calls:
            self.assertNotIn("test-secret", str(args))
            if args[:2] == ("git", "push"):
                self.assertEqual(kwargs["env"]["GIT_CONFIG_VALUE_1"], "false")
                self.assertNotIn("NIX_AUTOMATION_TOKEN", kwargs["env"])
            self.assertNotIn("build", args)
            self.assertNotIn("check", args)

    def test_push_and_pr_failures_fail_host_without_touching_other_locks(self):
        for fail_at in ("push", "pr"):
            c.run("git", "checkout", "--detach", self.sha)
            api = Mock()
            api.request.side_effect = ValueError("PR API unavailable")
            def failing(*args, **kwargs):
                if args[:2] == ("git", "push"):
                    if fail_at == "push":
                        raise subprocess.CalledProcessError(1, ["git", "push"])
                    return ""  # Never push to the real remote in an offline test.
                return self.command(*args, **kwargs)
            with patch.object(u, "run", failing):
                u.prepare("mini", self.sha)
                with self.assertRaises((ValueError, subprocess.CalledProcessError)):
                    u.publish("mini", self.sha, "8", api, "fake")
            self.assertEqual(api.request.call_count, 0 if fail_at == "push" else 1)
            self.assertEqual(Path("hosts/chungus/flake.lock").read_text(), "old\n")
            self.assertEqual(Path("hosts/lapdog/flake.lock").read_text(), "old\n")

    def test_dirty_or_wrong_base_never_updates(self):
        with patch.object(u, "run", self.command):
            with self.assertRaises(ValueError):
                u.prepare("mini", A)
            Path("untracked").touch()
            with self.assertRaises(ValueError):
                u.prepare("mini", self.sha)
        self.assertFalse(any(args[0] == "nix" for args, _ in self.calls))

    def test_cross_host_change_and_staged_change_fail_closed(self):
        def bad(*args, **kwargs):
            if args[0] == "nix":
                Path("hosts/chungus/flake.lock").write_text("unexpected")
                return ""
            return c.run(*args, **kwargs)
        with patch.object(u, "run", bad), self.assertRaises(ValueError):
            u.prepare("mini", self.sha)
        c.run("git", "reset", "--hard", "HEAD")
        c.run("git", "add", "hosts/mini/flake.lock")
        # A changed staged lock is not silently committed by publishing.
        Path("hosts/mini/flake.lock").write_text("staged")
        c.run("git", "add", "hosts/mini/flake.lock")
        with self.assertRaises(ValueError):
            u.changed_lock("mini")

    def test_invalid_host_and_nonmain_dispatch(self):
        for host in ("inspiron7520", "../mini", "rescue"):
            with self.assertRaises(ValueError):
                u.prepare(host, self.sha)
        with patch.dict(os.environ, {"DISPATCH_REF": "refs/heads/feature"}), \
                patch.object(sys, "argv", ["update.py", "prepare"]), self.assertRaises(SystemExit):
            u.main()

    def test_nix_failure_does_not_produce_publish_state(self):
        with patch.object(u, "run", side_effect=subprocess.CalledProcessError(1, ["nix"])), \
                self.assertRaises(subprocess.CalledProcessError):
            u.prepare("mini", self.sha)
        self.assertFalse(u.STATE.exists())


class Difference(unittest.TestCase):
    def test_all_categories_transitive_paths_and_exact_coverage(self):
        same = store(1, "same-1")
        before = {same, store(2, "lib-1"), store(3, "app-1"), store(4, "removed-1")}
        after = {same, store(5, "lib-2"), store(6, "app-1"), store(7, "added-1")}
        changes = r.differences(before, after)
        self.assertEqual({k: len(v) for k, v in changes.items()},
                         dict(additions=1, removals=1, version_changes=1, rebuilds=1))
        old, new = set(changes["removals"]), set(changes["additions"])
        for key in ("version_changes", "rebuilds"):
            old.update(x["before"] for x in changes[key])
            new.update(x["after"] for x in changes[key])
        self.assertEqual(old, before - after)
        self.assertEqual(new, after - before)

    def test_multiple_versions_or_outputs_not_falsely_paired(self):
        before = {store(1, "lib-1"), store(2, "lib-2")}
        after = {store(1, "lib-1"), store(3, "lib-3")}
        changes = r.differences(before, after)
        self.assertFalse(changes["version_changes"])
        self.assertFalse(changes["rebuilds"])
        self.assertEqual(len(changes["additions"]), 1)
        self.assertEqual(len(changes["removals"]), 1)

    def test_identical(self):
        self.assertFalse(any(r.differences({store(1, "system")}, {store(1, "system")}).values()))


class FakeAPI:
    def __init__(self):
        self.pr = dict(state="open", merged=False,
                       head=dict(sha=A, ref="feature", repo=dict(full_name=c.REPO)),
                       base=dict(ref="main", repo=dict(full_name=c.REPO)))
        self.tip = B
        self.writes = []
        self.fail = False

    def request(self, method, path, data=None):
        if self.fail:
            raise ValueError("offline")
        if method != "GET":
            self.writes.append((method, path, data))
            return {}
        if "/pulls/" in path:
            return copy.deepcopy(self.pr)
        if "/branches/" in path:
            return {"commit": {"id": self.tip}}
        raise AssertionError(path)

def report_data():
    before, after = {store(1, "app-1")}, {store(2, "app-2")}
    return dict(host="mini", source=A, target="main", baseline=B, pr=3,
                run_number=10, attempt=1, run_url=f"{c.SERVER}/{c.REPO}/actions/runs/10",
                schema_version=2, before=sorted(before), after=sorted(after),
                nar_sizes={p: 100 for p in before | after},
                groups=r.grouped(dict.fromkeys(before, 100), dict.fromkeys(after, 100)),
                changes=r.differences(before, after))


class Summary(Temporary):
    def setUp(self):
        super().setUp()
        self.data = report_data()
        r.REPORT.mkdir(parents=True)
        (r.REPORT / "report.json").write_text(json.dumps(self.data))

    def test_summary_file_append_and_forgejo_precedence(self):
        with patch.dict(os.environ, {"REPORT_READY": "true", "ARTIFACT_OUTCOME": "success"}):
            r.summarize()
            self.assertIn("app", Path(".private/summary").read_text())
            with patch.dict(os.environ, {"FORGEJO_STEP_SUMMARY": str(self.base / ".private/forgejo")}):
                r.summarize()
                r.write_summary("extra\n")
            self.assertTrue(Path(".private/forgejo").read_text().endswith("extra\n"))
        self.assertNotIn("nix-host-report-state", Path(".private/summary").read_text())

    def test_artifact_failure_keeps_preview_with_warning(self):
        with patch.dict(os.environ, {"REPORT_READY": "true", "ARTIFACT_OUTCOME": "failure"}):
            r.summarize()
        body = Path(".private/summary").read_text()
        self.assertIn("Complete artifact unavailable", body)
        self.assertIn("app", body)
        self.assertIn("bounded preview", body)

    def test_unavailable_comparison_does_not_use_stale_report(self):
        (r.REPORT / "failure.txt").write_text("baseline <missing>\n|bad|")
        with patch.dict(os.environ, {"HOST": "mini", "REPORT_READY": "", "PREPARE_OUTCOME": "success"}):
            r.summarize()
        body = Path(".private/summary").read_text()
        self.assertIn("Comparison unavailable", body)
        self.assertIn("&lt;missing&gt;", body)
        self.assertNotIn("app", body)
        Path(".private/summary").unlink()
        with patch.dict(os.environ, {"HOST": "mini", "REPORT_READY": "", "PREPARE_OUTCOME": "failure"}):
            r.summarize()
        self.assertNotIn("missing", Path(".private/summary").read_text())

    def test_missing_summary_env_and_size_guard(self):
        with patch.dict(os.environ, {}, clear=True), self.assertRaises(ValueError):
            r.write_summary("report")
        with self.assertRaises(ValueError):
            r.write_summary("×" * 120001)

    def test_bounded_preview_full_report_and_escaping(self):
        before = dict.fromkeys(self.data["before"], 100)
        after = {store(i + 10, f"evil{i}-`<tag>|" + "x" * 500): 200 for i in range(500)}
        self.data.update(after=sorted(after), nar_sizes=before | after,
                         groups=r.grouped(before, after), changes=r.differences(set(before), set(after)))
        bounded, full = r.render(self.data), r.render(self.data, bounded=False)
        self.assertLess(len(bounded.encode()), 240000)
        self.assertIn("Preview limited", bounded)
        self.assertNotIn("<tag>", bounded)
        self.assertIn("&#96;", bounded)
        self.assertIn("&#124;", bounded)
        self.assertGreater(len(full), len(bounded) * 10)
        self.assertIn("Exact changed paths", full)
        self.data.update(after=self.data["before"], groups={k: [] for k in r.GROUPS})
        self.assertIn("No system closure changes", r.render(self.data))


class Preparation(Temporary):
    def setUp(self):
        super().setUp()
        self.api = FakeAPI()
        self.roots = Mock()
        self.roots.base = self.base / "roots"
        self.roots.base.mkdir()
        self.baseline = self.base / "baseline/result"
        self.candidate = self.base / "candidate/result"
        for path in (self.baseline, self.candidate):
            path.parent.mkdir()
        (self.baseline.parent / "provenance.json").write_text(json.dumps(dict(commit=B)))
        self.candidate_data = dict(repository=c.REMOTE, branch="feature", host="mini", commit=A,
                                   output=store(2, "system"))
        self.write_candidate()
        self.roots.selected.return_value = self.baseline
        self.roots.stage_path.return_value = self.candidate
        self.roots.output.return_value = self.candidate_data["output"]
        self.closure = patch.object(r, "closure", side_effect=lambda p: {store(1 if p == self.baseline else 2, "system"): 100})
        self.closure.start()
        self.addCleanup(self.closure.stop)

    def write_candidate(self):
        (self.candidate.parent / "provenance.json").write_text(json.dumps(self.candidate_data))

    def prepare(self):
        return r.prepare("mini", A, 3, 10, 1, self.candidate, self.api, self.roots)

    def test_exact_baseline_full_json_and_report(self):
        self.prepare()
        data = json.loads((r.REPORT / "report.json").read_text())
        self.assertEqual((data["source"], data["baseline"]), (A, B))
        self.assertEqual(data["before"], [store(1, "system")])
        self.assertIn("ready=true", Path(".private/output").read_text())
        self.assertTrue((r.REPORT / "report.md").exists())

    def test_missing_corrupt_stale_baseline_and_candidate_mismatch_no_ready(self):
        for failure in (FileNotFoundError(), ValueError("corrupt")):
            self.roots.selected.side_effect = failure
            with self.assertRaises(type(failure)):
                self.prepare()
        self.roots.selected.side_effect = None
        self.api.tip = C
        with self.assertRaises(ValueError):
            self.prepare()
        self.api.tip = B
        self.candidate_data["host"] = "chungus"
        self.write_candidate()
        with self.assertRaises(ValueError):
            self.prepare()
        self.assertFalse(Path(".private/output").exists())

    def test_target_advancing_during_comparison_and_pr_head_change_skip(self):
        original = self.api.request
        count = 0
        def advance(method, path, data=None):
            nonlocal count
            if "/branches/" in path:
                count += 1
                if count == 2:
                    self.api.tip = C
            return original(method, path, data)
        self.api.request = advance
        with self.assertRaises(ValueError):
            self.prepare()
        self.assertFalse(Path(".private/output").exists())
        self.api.request = original
        self.api.tip = B
        def moved(p):
            self.api.pr["head"]["sha"] = C
            return {store(1, "system"): 100}
        with patch.object(r, "closure", side_effect=moved), self.assertRaises(ValueError):
            self.prepare()

    def test_candidate_provenance_or_output_change_during_query_skips(self):
        for mode in ("provenance", "output"):
            self.roots.output.return_value = self.candidate_data["output"]
            self.write_candidate()
            def corrupt(path):
                if path == self.candidate:
                    if mode == "provenance":
                        (self.candidate.parent / "provenance.json").write_text("{}")
                    else:
                        self.roots.output.return_value = store(3, "other-system")
                return {store(1, "system"): 100}
            with patch.object(r, "closure", side_effect=corrupt), self.assertRaises(ValueError):
                self.prepare()
            self.assertFalse(Path(".private/output").exists())

    def test_comparison_error_clears_previous_files_and_main_warns_not_fails(self):
        self.prepare()
        self.roots.selected.side_effect = OSError("unavailable")
        with self.assertRaises(OSError):
            self.prepare()
        self.assertFalse((r.REPORT / "report.json").exists())
        with patch.object(sys, "argv", ["report.py", "summary"]), patch.object(r, "warning") as warn:
            r.main()
        warn.assert_called_once()


class WorkflowContracts(Temporary):
    def test_capture_selector_outputs_one_sha_and_exact_matrix_without_injection(self):
        workflow = (HERE.parents[1] / ".forgejo/workflows/nix-input-update.yml").read_text()
        block = workflow.split("        run: |\n", 1)[1].split("  update:", 1)[0]
        script = "\n".join(line[10:] for line in block.splitlines() if line.startswith("          "))
        for selected in (*c.HOSTS, "all", "bad", 'mini; touch injected'):
            Path(".private/output").unlink(missing_ok=True)
            env = dict(c.clean_env(), DISPATCH_SHA=A, SELECTED_HOST=selected,
                       GITHUB_OUTPUT=str(self.base / ".private/output"))
            result = subprocess.run(["bash", "-c", script], env=env, capture_output=True, text=True)
            if selected in (*c.HOSTS, "all"):
                self.assertEqual(result.returncode, 0, result.stderr)
                values = dict(line.split("=", 1) for line in Path(".private/output").read_text().splitlines())
                self.assertEqual(values["sha"], A)
                self.assertEqual(json.loads(values["hosts"]), list(c.HOSTS) if selected == "all" else [selected])
            else:
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse(Path(".private/output").exists())
        self.assertFalse(Path("injected").exists())
        self.assertIn("github.ref == 'refs/heads/main'", workflow)
        self.assertIn("fail-fast: false", workflow)
        self.assertIn("ref: ${{ env.BASE_SHA }}", workflow)

    def test_bot_secret_only_updater_and_summary_after_upload(self):
        updater = (HERE.parents[1] / ".forgejo/workflows/nix-input-update.yml").read_text()
        builder = (HERE.parents[1] / ".forgejo/workflows/nix-build.yml").read_text()
        self.assertNotIn("secrets.NIX_AUTOMATION_TOKEN", updater.split("      - name: Publish fresh")[0])
        self.assertNotIn("NIX_AUTOMATION_TOKEN", builder)
        self.assertNotIn("NIX_AUTOMATION_BOT_ID", builder)
        self.assertNotIn("report.py publish", builder)
        self.assertEqual(updater.count("secrets.NIX_AUTOMATION_TOKEN"), 1)
        self.assertIn("report.py summary", builder)
        self.assertIn("steps.report.outcome != 'skipped'", builder)
        self.assertIn("ARTIFACT_OUTCOME: ${{ steps.artifact.outcome }}", builder)
        self.assertEqual(builder.count("continue-on-error: true"), 3)
        self.assertIn("::warning::", builder)
        self.assertIn("github.event_name == 'pull_request'", builder)
        self.assertLess(builder.index("nix-cache-roots promote"), builder.index("report.py prepare"))


class Environment(unittest.TestCase):
    def test_no_tokens_or_injected_config_forwarded_to_subprocess(self):
        with patch.dict(os.environ, dict(NIX_AUTOMATION_TOKEN="bot", NIX_CACHE_JOB_TOKEN="job",
                                         ACTIONS_RUNTIME_TOKEN="runtime", GIT_CONFIG_COUNT="99", NIX_CONFIG="bad")), \
                patch.object(c.subprocess, "check_output", return_value="") as call:
            c.run("nix", "flake", "update", "--flake", "./hosts/mini")
            env = call.call_args.kwargs["env"]
            for key in ("NIX_AUTOMATION_TOKEN", "NIX_CACHE_JOB_TOKEN", "ACTIONS_RUNTIME_TOKEN",
                        "GIT_CONFIG_COUNT", "NIX_CONFIG"):
                self.assertNotIn(key, env)

    def test_api_http_and_network_errors_do_not_echo_response_or_token(self):
        import urllib.error
        api = c.Forgejo("do-not-print-this")
        api.opener = Mock()
        errors = [urllib.error.HTTPError(c.API + "/private", 403, "sensitive body", {}, None),
                  urllib.error.URLError("sensitive connection details")]
        for error in errors:
            api.opener.open.side_effect = error
            with self.assertRaises(ValueError) as caught:
                api.request("POST", "/private", {"body": "data"})
            self.assertNotIn("sensitive", str(caught.exception))
            self.assertNotIn("do-not-print-this", str(caught.exception))
        with self.assertRaises(ValueError):
            api.request("GET", "//foreign.example/token")

    def test_api_paginates_and_fails_on_incomplete_or_redirect(self):
        api = c.Forgejo("fake")
        # A server-clamped short page is not necessarily the final page.
        api.request = Mock(side_effect=[[{}] * 20, [{"id": 2}], []])
        self.assertEqual(len(api.pages("/comments")), 21)
        self.assertIn("page=3", api.request.call_args.args[1])
        api.request = Mock(return_value={})
        with self.assertRaises(ValueError):
            api.pages("/comments")
        self.assertIsNone(c.NoRedirect().redirect_request(None, None, None, None, None, None))
        with self.assertRaises(ValueError):
            c.Forgejo("")

    def test_closure_query_errors_and_malformed_output(self):
        for result in ("", "/some/path", store(1, "valid") + "\n" + store(1, "valid")):
            with patch.object(r, "run", return_value=result), self.assertRaises(ValueError):
                r.closure("root")
        with patch.object(r, "run", side_effect=subprocess.CalledProcessError(1, ["nix-store"])), \
                self.assertRaises(subprocess.CalledProcessError):
            r.closure("root")


if __name__ == "__main__":
    unittest.main()
