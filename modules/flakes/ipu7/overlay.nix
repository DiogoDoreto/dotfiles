final: prev:
let
  ipu7-camera-bins = final.callPackage ./pkgs/ipu7-camera-bins.nix { };
in
{
  inherit ipu7-camera-bins;

  ipu7x-camera-hal = final.callPackage ./pkgs/ipu7-camera-hal.nix {
    inherit ipu7-camera-bins;
    ipuVersion = "ipu7x";
  };

  ipu75xa-camera-hal = final.callPackage ./pkgs/ipu7-camera-hal.nix {
    inherit ipu7-camera-bins;
    ipuVersion = "ipu75xa";
  };

  # Extend kernel packages with ipu7-drivers
  linuxKernel = prev.linuxKernel // {
    packagesFor =
      kernel:
      (prev.linuxKernel.packagesFor kernel).extend (
        kFinal: kPrev: {
          ipu7-drivers = kFinal.callPackage ./pkgs/ipu7-drivers { };
        }
      );
  };

  # Extend gstreamer scope with icamerasrc IPU7 variants
  gst_all_1 = prev.gst_all_1 // {
    icamerasrc-ipu7x = final.callPackage ./pkgs/icamerasrc-ipu7.nix {
      inherit (prev.gst_all_1) apple-sdk_gstreamer;
      ipu6-camera-hal = prev.ipu6-camera-hal or (throw "ipu6-camera-hal not available");
      ipu7x-camera-hal = final.ipu7x-camera-hal;
      ipuVariant = "ipu7";
    };
    icamerasrc-ipu75xa = final.callPackage ./pkgs/icamerasrc-ipu7.nix {
      inherit (prev.gst_all_1) apple-sdk_gstreamer;
      ipu6-camera-hal = prev.ipu6-camera-hal or (throw "ipu6-camera-hal not available");
      ipu7x-camera-hal = final.ipu75xa-camera-hal;
      ipuVariant = "ipu7";
    };
  };
}
