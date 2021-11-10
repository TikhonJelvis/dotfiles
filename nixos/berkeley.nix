{ config, pkgs, ... }:

{
  imports =
    [ ./base
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

  services.borgbackup.jobs = {
    "borgbase" = {
      paths = [
        "/home"
      ];
      exclude = [
        # XDG cache directory
        "/home/*/.cache"

        # Bigger app data directories (> 250M)
        "/home/*/.local/share/Trash"
        "/home/*/.local/share/baloo"
        "/home/*/.local/share/Steam"

        # Build outputs
        "**/target"        # Rust, Scala... etc
        "/home/*/.cargo"   # Global Rust cache
        "**/dist"          # Python, Haskell... etc
        "**/dist-newstyle" # Haskell
        "**/_site"         # Hakyll
        "**/_cache"        # Hakyll

        # Big projects
        "**/Programming/nixpkgs"
      ];
      repo = "h05ln3up@h05ln3up.repo.borgbase.com:repo";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /root/borg-passphrase";
        # Passphrase also saved in 1Password as
        # tikhon-nixos-berkeley-passphrase under borgbase
      };
      environment.BORG_RSH = "ssh -i /root/.ssh/borgbase";
      compression = "auto,zstd";
      startAt = "daily";
    };
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
