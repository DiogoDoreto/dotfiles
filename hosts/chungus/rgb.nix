{ pkgs, ... }:

{
  # Allow OpenRGB to access the SMBus for RAM RGB control.
  # Intel ACPI tables claim the SMBus by default, blocking i2c access to DIMMs.
  boot.kernelParams = [ "acpi_enforce_resources=lax" ];

  # spd5118 claims the DDR5 SPD hubs (0x51/0x53), blocking OpenRGB's DIMM
  # detection flow even though the RGB controllers themselves are accessible.
  boot.blacklistedKernelModules = [ "spd5118" ];

  # Grant userspace access to /dev/i2c-* and create the i2c group.
  hardware.i2c.enable = true;

  # OpenRGB SDK server — controls RAM and GPU RGB via SMBus/USB.
  # motherboard auto-detected as "intel" from hardware.cpu.intel.updateMicrocode.
  # Save a profile in the GUI then set startupProfile = "<name>" to auto-apply it.
  services.hardware.openrgb = {
    enable = true;
    package = pkgs.openrgb.withPlugins [
      pkgs.openrgb-plugin-effects
      pkgs.openrgb-plugin-hardwaresync
    ];
  };

  users.users.dog.extraGroups = [ "i2c" ];
}
