# Values that the host integration and guest system must agree on.
# Network identities and state paths use the shared module defaults.
{
  autostart = true;
  paseo.enable = true;
  shares = [
    {
      tag = "projects";
      source = "/home/dog/projects";
      mountPoint = "/home/agent/projects";
      readOnly = false;
    }
  ];
}
