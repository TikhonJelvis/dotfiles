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

  # MongoDB for CX Score development/etc
  services.mongodb = {
    enable = true;
    package = pkgs.mongodb-6_0;
    extraConfig = ''
      net.port: 27018
    '';
  };

  # Redis for CX Score development/etc
  services.redis.servers.cxscore = {
    enable = true;
    port = 6379;
  };

  nix = {
    settings = {
      substituters = [
        # 2023-09-05: temporarily disabling some cachix caches because
        # cachix is giving me 502 errors:

        "https://cache.nixos.org"
        "https://hercules-ci.cachix.org"
        "https://iohk.cachix.org"
        "https://nix-community.cachix.org"
        "https://nix-tools.cachix.org"
        "https://nixcache.reflex-frp.org"

        # Cachix caches I set up myself
        # "https://haskell-org.cachix.org"
        # "https://rl-book.cachix.org"
        # "https://theta-idl.cachix.org"
      ];

      trusted-substituters = [
        # Caches on my home network
        #
        # Won't work when I'm /not/ on my home network
        "http://tikhon-nixos-berkeley.local:8080"
        "http://tikhon-nixos-o11.local:8080"
      ];

      trusted-public-keys = [
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
        "tikhon-nixos-o11.local:J/cmWNKj/cOF+E6D1BToOJ2yLD1Q1kZWNt4bj1vZJSI="
      ];
    };

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
    # config.boot.kernelPackages instead of pkgs.linuxPackages so that
    # this is consistent when changing the kernel version in other
    # parts of my config
    extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
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
      "192.168.0.42" = [ "tikhon-nixos-o11.local" ];
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
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;

    daemon.config = {
      "default-sample-rate" = 48000;
    };
  };
  # realtime scheduling priority for Pulse Audio
  security.rtkit.enable = true;


  # Scanning
  hardware.sane = {
    enable = true;

    disabledDefaultBackends = [
      "net" # not using any network scanners right now...
      "v4l" # webcam—not useful as scanner and slows down scanimage -L
    ];
  };

  # For QMK
  services.udev.packages = [ pkgs.qmk-udev-rules ];

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

  services.gnome = {
    # Trying to a warning from reflex-dom apps:
    # > Could not determine the accessibility bus address
    at-spi2-core.enable = true;

    # For saving wifi passwords with nm-applet/etc
    gnome-keyring.enable = true;

    # WebKitGtk needs this for https:// requests
    glib-networking.enable = true;
  };

  # using lightdm instead of sddm to suppress kwallet dialog at
  # startup
  services.displayManager = {
    # lightdm.enable = true;
    defaultSession = "home-manager";
  };

  services.xserver = {
    enable = true;
    xkb.layout = "us";
    xkb.options = "caps:ctrl_modifier";

    # Set Xft.dpi to services.xserver.dpi if it's explicitly
    # configured
    displayManager.sessionCommands =
      lib.optionalString (config.services.xserver.dpi != null) ''
        ${pkgs.xorg.xrdb}/bin/xrdb -merge <${pkgs.writeText "Xresources" ''
          Xft.dpi: ${toString config.services.xserver.dpi}
          *dpi: ${toString config.services.xserver.dpi}
        ''}
      '';

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
      description = "Tikhon Jelvis";
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
