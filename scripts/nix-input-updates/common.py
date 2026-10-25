"""Fixed-repository Forgejo access and credential-free subprocesses."""

import json
import os
import re
import subprocess
import urllib.error
import urllib.request

SERVER = "https://git.local.doreto.com.br"
REPO = "diogo/dotfiles"
API = f"{SERVER}/api/v1"
REMOTE = f"{SERVER}/{REPO}.git"
HOSTS = ("lapdog", "mini", "chungus")


def sha(value):
    if not isinstance(value, str) or not re.fullmatch(r"[0-9a-f]{40}", value):
        raise ValueError("invalid commit SHA")
    return value


def host(value):
    if value not in HOSTS:
        raise ValueError("invalid host")
    return value


def positive(value):
    if not re.fullmatch(r"[1-9][0-9]*", str(value)):
        raise ValueError("invalid run/PR identifier")
    return int(value)


def clean_env():
    # Never forward bot/job tokens, injected Git configuration, or NIX_CONFIG.
    return {k: v for k, v in os.environ.items() if k in (
        "PATH", "HOME", "SSL_CERT_FILE", "NIX_SSL_CERT_FILE", "TMPDIR",
        "NIX_REMOTE", "NIX_PATH")}


def run(*args, env=None):
    return subprocess.check_output(args, text=True, env=env or clean_env(), timeout=1800).strip()


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):
        return None


class Forgejo:
    def __init__(self, token):
        if not token:
            raise ValueError("missing Forgejo token")
        self.token = token
        self.opener = urllib.request.build_opener(NoRedirect)

    def request(self, method, path, data=None):
        if not path.startswith("/") or path.startswith("//"):
            raise ValueError("invalid API path")
        req = urllib.request.Request(API + path, method=method,
                                     data=None if data is None else json.dumps(data).encode(),
                                     headers={"Authorization": f"token {self.token}",
                                              "Content-Type": "application/json"})
        try:
            with self.opener.open(req, timeout=60) as response:
                return json.load(response)
        except urllib.error.HTTPError as error:
            # Do not log response bodies, headers or credentials.
            raise ValueError(f"Forgejo {method} failed (HTTP {error.code})") from None
        except urllib.error.URLError:
            raise ValueError(f"Forgejo {method} unavailable") from None

    def pages(self, path):
        result = []
        for page in range(1, 10001):
            data = self.request("GET", f"{path}?limit=50&page={page}")
            if not isinstance(data, list):
                raise ValueError("invalid paginated response")
            # Instances may clamp `limit` below 50. Only an empty page proves
            # exhaustion; a short nonempty page must not hide an existing marker.
            if not data:
                return result
            result.extend(data)
        raise ValueError("pagination limit exceeded; refusing incomplete comment inventory")


def output(key, value):
    with open(os.environ["GITHUB_OUTPUT"], "a") as file:
        file.write(f"{key}={value}\n")


def warning(message):
    # Escape workflow-command data; errors may contain repository-controlled text.
    text = str(message).replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")
    print(f"::warning::{text}")
