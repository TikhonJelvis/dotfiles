# This is my "base" configuration—settings that are shared across all
# my machines.
#
# Configurations for each machine import this file and overwrite any
# machine-specific options they need. At minimum, they set:
#
#  * networking.hostName

{ config, pkgs, lib, ... }:

{
  imports =
    [
      ./fonts
    ];

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  programs.steam.enable = true;

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [
    (self: super: {
      steam = super.steam.override {
        # Temporary (?) fix for Steam bug
        extraPkgs = pkgs: with pkgs; [ pango harfbuzz libthai ];
      };
    })
  ];

  nix = {
    binaryCaches = [
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
      "https://iohk.cachix.org"
      "https://hydra.iohk.io"
      "https://nix-community.cachix.org"
      "https://nix-tools.cachix.org"
      "https://nixcache.reflex-frp.org"

      # Cachix caches I set up myself
      "https://haskell-org.cachix.org"
      "https://rl-book.cachix.org"
      "https://theta-idl.cachix.org"

      # Caches on my home network
      # "tikhon-nixos-berkeley.local"
    ];
    binaryCachePublicKeys = [
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-tools.cachix.org-1:ebBEBZLogLxcCvipq2MTvuHlP7ZRdkazFSQsbs0Px1A="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="

      "haskell-org.cachix.org-1:qGKBoWNtLfLvv61PudtpiyvMFQCwlam7mBbpZJ+iaMk="
      "rl-book.cachix.org-1:0/mq4Bx1ZfXxA1hzSfjFMBzViwfg39wkqTPLEKff4RM="
      "theta-idl.cachix.org-1:B6I1LwtM4zjDt3+KagdvW9Ma9cQ6rUCPIbuE0FyO9fw="

      "tikhon-nixos-berkeley.local:qsrRFBYXe0YyQ+/94GgIIMnR8tZGGKPLHL8mdV25Kkg="
    ];
    gc = {
      automatic = true;
      dates = "23:00";
      options = "--delete-older-than 30d";
    };
    extraOptions = ''
      http-connections = 0
    '';
  };

  boot = {
    extraModulePackages = [ pkgs.linuxPackages.v4l2loopback ];
    kernelModules = [ "v4l2loopback" ];
  };

  networking = {
    nameservers = [ "1.1.1.1" "8.8.8.8" ];

    # The global useDHCP flag is deprecated.
    useDHCP = false;

    networkmanager.enable = true;

    firewall = {
      enable = true;

      allowedTCPPorts = [
        # My own hosting/etc
        80
        8000
        8080

        # NDI communication
        5960

        # NDI channels
        5961
        5962
        5963
        5964

        # Synergy
        24800
      ];
      allowedUDPPorts = [
        # SRT
        2142
        # As far as I can tell, SRT does not have a default port
        # number that's used by convention, so I just chose one
        # arbitrarily.

        # mDNS for NDI
        5353
      ];
    };

    hosts = {
      "192.168.0.37" = [ "tikhon-nixos-berkeley.local" ];
    };
  };

  # Avahi config needed for OBS-NDI
  services.avahi = {
    enable = true;
    publish = {
      enable = true;
      userServices = true;
    };
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  services.openssh.enable = true;

  # Sound
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;

    daemon.config = {
      "default-sample-rate" = 48000;
    };
  };

  # Scanning
  hardware.sane = {
    enable = true;

    disabledDefaultBackends = [
      "net" # not using any network scanners right now...
      "v4l" # webcam—not useful as scanner and slows down scanimage -L
    ];
  };

  # Bluetooth
  hardware.bluetooth = {
    enable = true;

    settings.General.Enable = "Source,Sink,Media,Socket";
  };
  services.blueman.enable = true;

  # Window management/desktop/etc
  services.dbus = {
    enable = true;
    packages = [ pkgs.dconf ];
  };
  programs.dconf.enable = true;

  # Trying to a warning from reflex-dom apps:
  # > Could not determine the accessibility bus address
  services.gnome.at-spi2-core.enable = true;

  services.gnome.gnome-keyring.enable = true;

  systemd.services.upower.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "caps:ctrl_modifier";

    # Set Xft.dpi to services.xserver.dpi if it's explicitly
    # configured
    displayManager.sessionCommands =
      lib.optionalString (config.services.xserver.dpi != null) ''
        ${pkgs.xorg.xrdb}/bin/xrdb -merge <${pkgs.writeText "Xresources" ''
          Xft.dpi: ${toString config.services.xserver.dpi}
          *dpi: ${toString config.services.xserver.dpi}
        ''}
      '';

    # 2022-04-21: sddm started crashing after updating
    # nixpkgs-unstable; I couldn't figure out why, but enabling
    # lightdm instead works around the issue
    displayManager = {
      # sddm.enable = true;
      lightdm.enable = true;
      defaultSession = "home-manager";
    };

    desktopManager.plasma5.enable = true;
    desktopManager.session = [{
      name = "home-manager";
      start = ''
        ${pkgs.stdenv.shell} $HOME/.xsession-hm &
        waitPID=$!
      '';
    }];
  };

  # Don't forget to set up a password file on each machine
  users.users = {
    tikhon = {
      isNormalUser = true;
      extraGroups = [
        "wheel" # enable ‘sudo’
        "docker"
        "scanner" # sane
        "lp" # sane
        "networkmanager"
      ];
    };
  };
}
