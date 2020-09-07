# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./fonts.nix
    ];

  nixpkgs.config.allowUnfree = true;

  nix = {
    binaryCaches = [
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
      "https://iohk.cachix.org"
      "https://nix-community.cachix.org"
      "https://nix-tools.cachix.org"
      "https://nixcache.reflex-frp.org"
    ];
    binaryCachePublicKeys = [
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-tools.cachix.org-1:ebBEBZLogLxcCvipq2MTvuHlP7ZRdkazFSQsbs0Px1A="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    ];
    gc = {
      automatic = true;
      dates = "23:00";
      options = "--delete-older-than 30d";
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

  networking = {
    hostName = "tikhon-nixos-pc";
    nameservers = ["1.1.1.1" "8.8.8.8"];

    # The global useDHCP flag is deprecated.
    useDHCP = false;
    interfaces.enp35s0.useDHCP = true;
    interfaces.wlp37s0.useDHCP = true;

    firewall = {
      allowedTCPPorts = [
        24800 # Synergy
      ];
    };

    # Proxy settings (if needed in the future):
    # proxy.default = "http://user:password@proxy:port/";
    # proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # TODO: Get wifi working
    wireless.enable = false;
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
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

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.tikhon = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
