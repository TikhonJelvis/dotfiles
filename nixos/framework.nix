{ config, pkgs, ... }:
{
  imports = [ ./base/laptop.nix
              ./hardware-configuration/framework.nix
            ];

  users.mutableUsers = false;
  users.users.tikhon.passwordFile = "/home/tikhon/pass";

  networking = {
    hostName = "tikhon-nixos-framework";
  };

  # the systemd-boot EFI boot loader (rather than GRUB/etc)
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # kernel settings as suggested in
  # nixos-hardware/framework/default.nix
  boot.kernelParams = [
    # better power consumption
    "mem_sleep_default=deep"
    "nvme.noacpi=1"
  ];

  # try using 5.18 as suggested on GitHub
  #
  # https://github.com/NixOS/nixpkgs/issues/183955#issuecomment-1210468614
  boot.kernelPackages = pkgs.linuxPackages_5_18;

  system.stateVersion = "22.05";
}
