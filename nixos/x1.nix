{ config, pkgs, ... }:
{
  imports = [ ./base/laptop.nix
              ./hardware-configuration/x1.nix
            ];

  users.mutableUsers = false;
  users.users.tikhon.passwordFile = "/home/tikhon/pass";

  networking = {
    hostName = "tikhon-nixos-x1";

    interfaces.enp0s31f6.useDHCP = true;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  # force DPI for reasonable behavior on the laptop's 14" 4k display
  services.xserver.dpi = 96 * 2;

  hardware.video.hidpi.enable = true;

  # the systemd-boot EFI boot loader (rather than GRUB/etc)
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  system.stateVersion = "21.11";
}
