---
name: freshrss-workbook
description: >
  Operational workbook for debugging and maintaining FreshRSS on the mini/dogdot
  server. Use when the user reports FreshRSS is broken, returning errors, or
  behaving unexpectedly — covers log locations, database access, migration issues,
  auth troubleshooting, and the service stack.
---

# FreshRSS Workbook

## Service Stack

FreshRSS on mini is served entirely within Caddy + PHP-FPM. There is no
dedicated `freshrss.service` — the relevant systemd units are:

| Unit | Role |
|---|---|
| `phpfpm-freshrss.service` | PHP-FPM worker pool for FreshRSS |
| `caddy.service` | Reverse proxy + PHP FastCGI handler |
| `authentik.service` | SSO/OIDC server (forward-auth gate) |
| `authentik-worker.service` | Authentik background worker |
| `freshrss-updater.service` | Periodic feed fetch (systemd timer) |

FreshRSS config: `hosts/mini/services/freshrss.nix`

## Where Things Live

| Resource | Path |
|---|---|
| FreshRSS data root | `/var/lib/freshrss/` |
| User database (SQLite) | `/var/lib/freshrss/users/diogo/db.sqlite` |
| User app log | `/var/lib/freshrss/users/diogo/log.txt` |
| System/API log | `/var/lib/freshrss/users/_/log.txt` |
| Global config | `/var/lib/freshrss/config.php` |
| User config | `/var/lib/freshrss/users/diogo/config.php` |
| Applied migrations marker | `/var/lib/freshrss/applied_migrations.txt` |
| Caddy access log | `/var/log/caddy/access-freshrss.local.doreto.com.br.log` |
| PHP binary | `/nix/store/*-php-with-extensions-*/bin/php` (check `systemctl cat phpfpm-freshrss` for exact path) |
| FreshRSS app | `/nix/store/*-FreshRSS-*/` (check Caddy config for exact hash) |

## First-Response Checklist

When FreshRSS is broken, run these in parallel:

```bash
# Service health
systemctl status phpfpm-freshrss.service caddy.service authentik.service

# Recent errors
tail -30 /var/lib/freshrss/users/diogo/log.txt
journalctl -u phpfpm-freshrss.service -n 50 --no-pager
journalctl -u freshrss-updater.service -n 20 --no-pager

# Recent HTTP responses (parse status + URI from JSON access log)
tail -20 /var/log/caddy/access-freshrss.local.doreto.com.br.log \
  | grep -o '"ts":[0-9.]*\|"uri":"[^"]*"\|"status":[0-9]*' | paste - - -
```

Key things to look for:
- **500 on `/i/?...`** — FreshRSS web UI crashing (PHP error or SQL error)
- **500 on `/api/greader.php/...`** — API also broken, likely schema/migration issue
- **302 loop** — auth redirect loop (Authentik or session problem)
- **`/i/?rid=...` → 500** — FreshRSS attempting migration, failing

## PHP Errors

PHP errors for FreshRSS go to **syslog** via PHP-FPM's `catch_workers_output`.
Search the journal:

```bash
# FreshRSS fatal errors (logged by killApp())
journalctl --since "1 hour ago" --no-pager | grep -i 'FreshRSS\|fatal\|parse error'

# PHP-FPM worker errors
journalctl -t php-fpm --since "1 hour ago" --no-pager
```

FreshRSS also has its own logger. Application-level errors (SQL errors, feed
errors, auth warnings) go to the user log:

```bash
tail -50 /var/lib/freshrss/users/diogo/log.txt | grep -i 'error\|warning'
```

## Database Access (SQLite)

`sqlite3` is not in PATH on mini. Use the PHP binary instead:

```bash
# Get exact PHP binary path from the service
PHP=$(systemctl cat phpfpm-freshrss | grep ExecStart | grep -o '/nix/store[^ ]*php[^ ]*/bin/php')

# List columns of a table
sudo -u freshrss $PHP -r "
\$db = new SQLite3('/var/lib/freshrss/users/diogo/db.sqlite');
\$r = \$db->query('PRAGMA table_info(entry)');
while (\$row = \$r->fetchArray(SQLITE3_ASSOC)) echo \$row['name'] . PHP_EOL;
"

# Run arbitrary SQL
sudo -u freshrss $PHP -r "
\$db = new SQLite3('/var/lib/freshrss/users/diogo/db.sqlite');
echo \$db->querySingle('SELECT COUNT(*) FROM entry') . PHP_EOL;
"
```

Always run DB operations as the `freshrss` user (owns the file).

## Migration Issues

FreshRSS uses two migration systems:

1. **Minz_Migrator** — PHP files in `app/migrations/` (currently empty in 1.29.x).
   Tracked by `/var/lib/freshrss/applied_migrations.txt`. File must exist (even
   empty) for FreshRSS to start; if missing, it shows the install page.

2. **DAO auto-migration** — SQL `ALTER TABLE` statements defined in
   `app/SQL/install.sql.sqlite.php`. Run automatically on first request after
   upgrade when the DAO detects a missing column via `PRAGMA table_info`.

### Diagnosing a missed migration

Symptom: `SQL error FreshRSS_EntryDAO::listIdsWhere["HY000",1,"no such column: e.X"]`

The column `X` was added in a recent FreshRSS version but the ALTER TABLE
migration never ran. Check which columns are present:

```bash
sudo -u freshrss $PHP -r "
\$db = new SQLite3('/var/lib/freshrss/users/diogo/db.sqlite');
\$r = \$db->query('PRAGMA table_info(entry)');
while (\$row = \$r->fetchArray(SQLITE3_ASSOC)) echo \$row['name'] . PHP_EOL;
"
```

Cross-reference with `app/SQL/install.sql.sqlite.php` to find what's missing:
```bash
grep 'ALTER TABLE.*entry ADD COLUMN' \
  /nix/store/*-FreshRSS-*/app/SQL/install.sql.sqlite.php
```

### Applying a migration manually

```bash
sudo -u freshrss $PHP -r "
\$db = new SQLite3('/var/lib/freshrss/users/diogo/db.sqlite');
\$db->exec('ALTER TABLE entry ADD COLUMN lastModified BIGINT');
\$db->exec('CREATE INDEX IF NOT EXISTS entry_last_modified_index ON entry (lastModified)');
echo 'Done' . PHP_EOL;
"
```

After manually applying, restart PHP-FPM:
```bash
systemctl restart phpfpm-freshrss.service
```

### History: lastModified (1.29.0)

In June 2026, upgrading nixpkgs to FreshRSS 1.29.1 caused 500 errors because
the `lastModified` column (added in 1.29.0) was missing from the existing
SQLite database. The DAO auto-migration was failing silently. Fixed by applying
the ALTER TABLE manually.

## Auth / Authentik Forward Auth

FreshRSS uses Caddy `forward_auth` → Authentik outpost on `localhost:9000`.

- `/api/*` paths bypass SSO (GReader/Fever API use their own password auth)
- `/outpost.goauthentik.io/*` paths go directly to Authentik (OAuth callbacks)
- All other paths require Authentik auth; `X-Authentik-Username` is copied to
  `Remote-User` header, which FreshRSS reads as `HTTP_REMOTE_USER`

Check Authentik outpost log for auth decisions:
```bash
journalctl -u authentik.service --since "1 hour ago" --no-pager \
  | grep freshrss
```

A successful forward-auth shows `"status":0` in the outpost log (pass-through).
A redirect to login shows `"status":302`.

### TRUSTED_PROXY

FreshRSS only trusts `HTTP_REMOTE_USER` from trusted IPs. Configured in:
```nix
services.phpfpm.pools.freshrss.phpEnv = {
  TRUSTED_PROXY = "127.0.0.1/32 ::1/128 192.168.0.0/16 ...";
};
```

## postPatch Warning

The Nix package override with `postPatch` to fix `httpUtil.php` was removed in
June 2026 because FreshRSS 1.29.1 already contains the upstream fix
(`array_filter` with callback for empty string stripping). If the patch is
re-added for future versions, verify it doesn't double-apply — the first `sed`
matches `array_unique(` and adding `array_filter(` around it will corrupt the
file if upstream already wraps it. Always check the actual file after patching:

```bash
grep -n 'array_filter\|array_unique' \
  /nix/store/*-FreshRSS-*/app/Utils/httpUtil.php
```

## CLI Tools

FreshRSS ships PHP CLI scripts at `app/cli/` in the Nix store. Useful ones:

```bash
FRESHRSS=/nix/store/*-FreshRSS-*/
PHP=$(systemctl cat phpfpm-freshrss | grep ExecStart | grep -o '/nix/store[^ ]*/bin/php')

# Run as freshrss user with correct env
sudo -u freshrss FRESHRSS_DATA_PATH=/var/lib/freshrss \
  $PHP $FRESHRSS/cli/user-info.php --user diogo

sudo -u freshrss FRESHRSS_DATA_PATH=/var/lib/freshrss \
  $PHP $FRESHRSS/cli/actualize-user.php --user diogo
```
