{ config, pkgs, ... }:

{
  imports =
    [ ./base-configuration.nix
    ];

  networking.hostname = "tikhon-nixos-berkeley";

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        devices = [ "nodev" ];
        efiSupport = true;
        useOSProber = true;
        version = 2;
        extraEntries = ''
          menuentry "Restart" {
            reboot
          }
          menuentry "Turn Off" {
            halt
          }
        '';
      };
    };
  };

  system.stateVersion = "20.03";
}
