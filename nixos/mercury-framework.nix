{ config, pkgs, lib, ... }:
let
  # TODO: better way of managing sources?
  # Probably just switch to flakes instead...
  sources = import ../nix/sources.nix;

  kolide-launcher =
    pkgs.callPackage (import (sources.nix-agent + "/kolide-launcher.nix")) {};
in
{
  imports = [
    ./base/laptop.nix
    ./hardware-configuration/mercury-framework.nix
    (sources.nixos-hardware + "/framework/13-inch/7040-amd")
    (sources.nix-agent + "/modules/kolide-launcher")
  ];

  # TODO: switch to immutable users + hashed password file?
  # users.mutableUsers = false;
  # users.users.tikhon.hashedPasswordFile = "/home/tikhon/pass";

  networking = {
    hostName = "tikhon-nixos-mercury";
  };

  # manually add the crt file to dotfiles
  security.pki.certificateFiles = [ ./secrets/internal.mercury.com.ca.crt ];

  services.kolide-launcher.enable = true;

  # connect machine to tailscale with:
  #
  # > sudo tailscale up
  #
  # find tailscale ip4 address:
  #
  # > tailscale ip -4
  services.tailscale.enable = true;

  # kolide-launcher provides an executable called 'launcher'
  #
  # useful for debugging kolide issues with the 'launcher doctor'
  # command
  environment.systemPackages = [ kolide-launcher ];

  # seems like nixos-hardware wants to use pipewire for sound on
  # Framework laptops, which requires disabling pusleaudio
  hardware.pulseaudio.enable = lib.mkForce false;

  # the systemd-boot EFI boot loader (rather than GRUB/etc)
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # for decrypting full-disk encryption on boot
  boot.initrd.luks.devices."luks-967a54c3-b736-4889-81fd-56467140246f".device = "/dev/disk/by-uuid/967a54c3-b736-4889-81fd-56467140246f";

  # kernel settings as suggested in
  # nixos-hardware/framework/default.nix
  boot.kernelParams = [
    # better power consumption
    "mem_sleep_default=deep"
    "nvme.noacpi=1"
  ];

  system.stateVersion = "24.11";
}
