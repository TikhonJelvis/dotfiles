{ config, pkgs, ... }:
{
  imports = [ ./base
              ./hardware-configuration/x1.nix
            ];

  users.mutableUsers = false;
  users.users.tikhon.passwordFile = "/home/tikhon/pass";

  networking = {
    hostName = "tikhon-nixos-x1";

    networkmanager.enable = true;

    useDHCP = false;
    interfaces.enp0s31f6.useDHCP = true;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  users.users.tikhon.extraGroups = [ "networkmanager" ];

  # touch pad
  services.xserver.libinput = {
    enable = true;

    touchpad = {
      clickMethod = "clickfinger";
      tapping = false;
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  system.stateVersion = "21.11";
}
