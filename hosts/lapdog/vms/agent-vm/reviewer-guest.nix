{ inputs, pkgs, ... }:
let
  package = inputs.host-command-review.packages.${pkgs.stdenv.hostPlatform.system}.guest;
in
{
  environment.systemPackages = [ package ];
  home-manager.users.agent = { lib, pkgs, ... }: {
    # Native Pi owns its JSON merge and preserves all unrelated top-level/server
    # settings. Only the reserved host-review entry is replaced. No static
    # home.file mcp.json symlink and no Pi/T3 extension or HTTP listener.
    home.activation.host-review-mcp = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run ${pkgs.llm-agents.pi}/bin/pi mcp add host-review \
        --exposure deferred \
        --description 'Propose host commands with a reason via propose_command; track returned IDs with get_command_status or request_cancellation. Execution approval and output release are separate. If submission is ambiguous, do not blindly repeat; reconcile with the human reviewer.' \
        -- ${package}/bin/host-review-mcp
    '';
  };
}
