{ config, pkgs, ... }:

{
  imports =
    [ ./base
      ./hardware-configuration/aero.nix
    ];

  networking = {
    hostName = "tikhon-nixos-aero";

    networkmanager.enable = true;
  };

  users.users.tikhon.extraGroups = [ "networkmanager" ];

  # Trying to fix 1:30 (!) stop job during boot
  #
  # See: https://github.com/NixOS/nixpkgs/issues/60900
  systemd.services.systemd-user-sessions.enable = false;

  # Touchpad settings.
  services.xserver.libinput = {
    enable = true;

    clickMethod = "clickfinger";
    tapping = false;
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        version = 2;
        devices = [ "nodev" ];
        efiSupport = true;
        useOSProber = false;
        splashImage = ./grub/breeze/background.png;
        theme = ./grub/breeze;
        extraEntries = ''
          menuentry "Windows 10" --class windows {
            insmod part_gpt
            insmod fat
            insmod search_fs_uuid
            insmod chain
            search --no-floppy --fs-uuid --set=root 8AC1-64B9
            chainloader /EFI/Microsoft/Boot/bootmgfw.efi
          }
          menuentry "Restart" --class restart {
            reboot
          }
          menuentry "Turn Off" --class shutdown {
            halt
          }
        '';
      };
    };
  };

  system.stateVersion = "20.03";
}
