# Additional configuration shared by all my laptops
{ ... }:
{
  imports = [ ./. ];

  # Use KDE as default session because I have not really configured
  # XMonad for laptop use. (One day...)
  #
  # set to "" to get an error with all available session names
  services.displayManager.defaultSession = "plasma";

  # Touchpad settings
  services.libinput = {
    enable = true;

    touchpad = {
      clickMethod = "clickfinger";
      tapping = false;
    };
  };

  # battery/power info
  systemd.services.upower.enable = true;

  # fingerprint reader
  services.fprintd.enable = true;

  # for changing monitor brightness
  #
  # (mis-detected by nixos-generate-config)
  hardware.acpilight.enable = true;

  # for Intel CPUs?
  hardware.enableRedistributableFirmware = true;
}

