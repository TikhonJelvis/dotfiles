{ config, pkgs, ... }:

{
  imports =
    [ ./base
      ./hardware-configuration/berkeley.nix
    ];

  environment.systemPackages = with pkgs; [ wacomtablet ];

  services.xserver.wacom.enable = true;

  networking = {
    hostName = "tikhon-nixos-berkeley";

    interfaces.enp35s0.ipv4.addresses = [
      { address = "192.168.0.37"; prefixLength = 24; }
    ];
  };

  services.openssh = {
    enable = true;
    allowSFTP = false;
    listenAddresses = [ { addr = "192.168.0.37"; port = 22; } ];

    passwordAuthentication = false;
    challengeResponseAuthentication = false;
    banner = "tikhon-berkeley-nixos";
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        devices = [ "nodev" ];
        efiSupport = true;
        useOSProber = false;
        version = 2;
        splashImage = ./grub/breeze/background.png;
        theme = ./grub/breeze;
        extraEntries = ''
          menuentry "Windows" --class windows {
            insmod part_gpt
            insmod fat
            insmod search_fs_uuid
            insmod chain
            search --no-floppy --fs-uuid --set=root CEA0-0302
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
