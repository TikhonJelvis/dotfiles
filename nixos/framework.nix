{ config, pkgs, ... }:
{
  imports = [ ./base
              ./hardware-configuration/framework.nix
            ];

  users.mutableUsers = false;
  users.users.tikhon.passwordFile = "/home/tikhon/pass";

  networking = {
    hostName = "tikhon-nixos-framework";
  };

  services.xserver.libinput = {
    enable = true;

    touchpad = {
      clickMethod = "clickfinger";
      tapping = false;
    };
  };

  # the systemd-boot EFI boot loader (rather than GRUB/etc)
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  system.stateVersion = "22.05";
}
