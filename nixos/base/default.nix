# This is my "base" configuration—settings that are shared across all
# my machines.
#
# Configurations for each machine import this file and overwrite any
# machine-specific options they need. At minimum, they set:
#
#  * networking.hostName

{ config, pkgs, ... }:

{
  imports =
    [./fonts.nix
    ];

  nixpkgs.config.allowUnfree = true;

  nix = {
    binaryCaches = [
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
      "https://iohk.cachix.org"
      "https://hydra.iohk.io"
      "https://nix-community.cachix.org"
      "https://nix-tools.cachix.org"
      "https://nixcache.reflex-frp.org"
    ];
    binaryCachePublicKeys = [
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-tools.cachix.org-1:ebBEBZLogLxcCvipq2MTvuHlP7ZRdkazFSQsbs0Px1A="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
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
    nameservers = ["1.1.1.1" "8.8.8.8"];

    # The global useDHCP flag is deprecated.
    useDHCP = false;
    interfaces.enp35s0.useDHCP = true;
    interfaces.wlp37s0.useDHCP = true;

    firewall = {
      enable = true;

      allowedTCPPorts = [
        5960 # NDI communication

        5961 5962 5963 5964 # NDI Channels

        24800 # Synergy
      ];
      allowedUDPPorts = [
        2142 # SRT
        # As far as I can tell, SRT does not have a default port
        # number that's used by convention, so I just chose one
        # arbitrarily.

        5353 # mDNS for NDI
      ];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Services

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  # Avahi config needed to work with OBS-NDI
  services.avahi = {
    enable = true;
    publish = {
      enable = true;
      userServices = true;
    };
  };

  services.openssh.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e";

    displayManager.sddm.enable = true;

    desktopManager.plasma5.enable = true;
    desktopManager.session = [{
      name = "home-manager";
      start = ''
        ${pkgs.stdenv.shell} $HOME/.xsession-hm &
        waitPID=$!
      '';
    }];
  };

  # Don't forget to set a password with ‘passwd’.
  users.users = {
    tikhon = {
      isNormalUser = true;
      extraGroups = [
        "wheel" # Enable ‘sudo’ for the user.
        "docker"
      ];
    };
  };
}
