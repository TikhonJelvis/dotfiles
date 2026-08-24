# TODO: factor out commonalities between this and framework.nix?
{ config, pkgs, lib, ... }:
let
  # TODO: better way of managing sources?
  # Probably just switch to flakes instead...
  sources = import ../nix/sources.nix;
in
{
  imports = [
    ./base/laptop.nix
    ./hardware-configuration/framework-pro.nix
    (sources.nixos-hardware + "/framework/13-inch/amd-ai-300-series")
  ];

  users.mutableUsers = false;
  users.users.tikhon.hashedPasswordFile = "/home/tikhon/pass";

  networking = {
    hostName = "tikhon-nixos-framework-pro";
  };

  # firmware updates: see nixos-hardware docs[1][2] for details
  #
  # fwupdmgr refresh
  # fwupdmgr update
  #
  # [1]: https://github.com/NixOS/nixos-hardware/tree/master/framework
  # [2]: https://github.com/NixOS/nixos-hardware/tree/master/framework/13-inch/amd-ai-300-series
  services.fwupd.enable = true;

  # the systemd-boot EFI boot loader (rather than GRUB/etc)
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  system.stateVersion = "22.05";
}
