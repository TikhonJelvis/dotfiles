{ config, pkgs, ... }:
{
  imports = [ ./base
              ./hardware-configuration/x1.nix
            ];

  users.mutableUsers = false;
  users.users.tikhon.hashedPassword = "$6$CHDRwI4R8XXAj131$o.gcXEondaWAYr4bipBbU5C0fofuSuApe5eJEASsLCBgHY30sZ9qKL96hMGXAV7GssNJzcgaPHCZpsOSj4yju0";

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

    clickMethod = "clickfinger";
    tapping = false;
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  system.stateVersion = "21.11";
}
