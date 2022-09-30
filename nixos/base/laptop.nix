# Additional configuration shared by all my laptops
{ ... }:
{
  imports = [ ./. ];

  # Touchpad settings
  services.xserver.libinput = {
    enable = true;

    touchpad = {
      clickMethod = "clickfinger";
      tapping = false;
    };
  };

  # fingerprint reader
  services.fprintd.enable = true;

  # for changing monitor brightness
  #
  # (mis-detected by nixos-generate-config)
  hardware.acpilight.enable = true;

  # for Intel CPUs?
  hardware.enableRedistributableFirmware = true;
}

