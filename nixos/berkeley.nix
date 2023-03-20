{ config, pkgs, ... }:

{
  imports =
    [ ./base/desktop.nix
      ./base/borg.nix
      ./hardware-configuration/berkeley.nix
    ];

  networking = {
    hostName = "tikhon-nixos-berkeley";

    networkmanager.unmanaged = ["enp35s0"];
    interfaces.enp35s0.useDHCP = false;
    interfaces.enp35s0.ipv4.addresses = [
      { address = "192.168.0.37"; prefixLength = 24; }
    ];
    interfaces.enp35s0.ipv4.routes = [
      {
        address = "0.0.0.0";
        via = "192.168.0.1";
        options = { metric = "0"; };
        prefixLength = 0;
      }
    ];
  };

  services.borgbackup.jobs.borgbase.repo = "i2344ym0@i2344ym0.repo.borgbase.com:repo";

  # TODO: try nix-serve-ng instead?
  # Expose a Nix cache to the local network
  services.nix-serve = {
    enable = true;
    secretKeyFile = "/var/cache-priv-key.pem";
    port = 8080;
  };

  services.openssh = {
    enable = true;
    allowSFTP = false;
    listenAddresses = [ { addr = "192.168.0.37"; port = 22; } ];

    passwordAuthentication = false;
    kbdInteractiveAuthentication = false;
    banner = "tikhon-berkeley-nixos";
  };

  boot = {
    loader = {
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
