{ config, pkgs, ... }:

{
  imports =
    [ ./base
      ./berkeley/borg.nix
      ./hardware-configuration/berkeley.nix
    ];

  environment.systemPackages = with pkgs; [ wacomtablet ];

  services.xserver.wacom.enable = true;

  # Propriatery NVIDIA drivers—hopefully works better than open source
  # version
  services.xserver.videoDrivers = [ "nvidia" ];

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

  services.printing.enable = true;
  hardware.printers = let
    name = "Brother_HL-L3270CDW_series";
  in {
    ensurePrinters = [{
      inherit name;
      deviceUri = "dnssd://Brother%20HL-L3270CDW%20series._ipp._tcp.local/?uuid=e3248000-80ce-11db-8000-b42200424c0f";
      model = "everywhere";
      location = "1505 downstairs office";
      description = "Color laser printer in my downstairs office";
    }];
    ensureDefaultPrinter = name;
  };
  services.avahi.nssmdns = true; # Needed for CUPS to find the printer.

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
