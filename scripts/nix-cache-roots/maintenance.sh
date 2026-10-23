# The runner's native shutdown timeout is 25h; stop waiting after 24h so a
# cancellation at that deadline can never be mistaken for a successful drain.
set -euo pipefail
unset NIX_CACHE_JOB_TOKEN

runner=forgejo-runner-chungus.service
restart=false
drained=false

if [[ ! -s "$CREDENTIALS_DIRECTORY/repository-token" ]]; then
  echo 'Missing or empty repository-read credential; refusing maintenance' >&2
  exit 1
fi

restore_runner() {
  status=$?
  trap - EXIT INT TERM
  if "$restart"; then
    if "$drained"; then
      systemctl start "$runner" || status=1
    else
      # A timed-out drain may still be stopping. Queue restoration, don't wait
      # another hour or force-kill its remaining jobs. No reconciliation/GC ran.
      systemctl --no-block start "$runner" || status=1
    fi
  fi
  exit "$status"
}

[[ $(systemctl show "$runner" --property=LoadState --value) == loaded ]]
case $(systemctl show "$runner" --property=ActiveState --value) in
  active) restart=true ;;
  inactive) ;;
  *) echo 'Runner is transitioning or failed; refusing maintenance' >&2; exit 1 ;;
esac
trap restore_runner EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

echo 'Draining runner: stop accepting jobs and wait for current jobs to finish'
timeout --foreground --kill-after=10s 24h systemctl stop "$runner"
[[ $(systemctl show "$runner" --property=Result --value) == success ]]
drained=true

echo 'Reconciling deleted branches and abandoned staging roots'
# Only this unprivileged subprocess receives the repository-read token. Neither
# systemctl, the GC service, argv nor store-resident files receive its value.
NIX_CACHE_JOB_TOKEN=$(< "$CREDENTIALS_DIRECTORY/repository-token") \
  env -u CREDENTIALS_DIRECTORY \
  setpriv --reuid=chungus-nix --regid=chungus-nix --clear-groups \
    --bounding-set=-all --inh-caps=-all --no-new-privs \
    nix-cache-roots reconcile --apply

echo 'Running the existing generation-retention, GC and optimisation policy'
systemctl start nixos-generation-cleanup.service
