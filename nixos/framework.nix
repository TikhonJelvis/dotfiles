{ config, pkgs, lib, ... }:
let
  # TODO: better way of managing sources?
  # Probably just switch to flakes instead...
  sources = import ../nix/sources.nix;
in
{
  imports = [
    ./base/laptop.nix
    ./hardware-configuration/framework.nix
    (sources.nixos-hardware + "/framework/13-inch/12th-gen-intel")
  ];

  users.mutableUsers = false;
  users.users.tikhon.hashedPasswordFile = "/home/tikhon/pass";

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

  system.stateVersion = "22.05";
}
