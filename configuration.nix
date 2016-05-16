# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.gummiboot.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;

  # Sets my default timezone to Pacific:
  time.timeZone = "US/Pacific";

  networking.hostName = "nixos"; # Define your hostname.
  networking.hostId = "c26f66e1";
  # networking.wireless.enable = true;  # Enables wireless.
  networking.networkmanager.enable = true;

  hardware.pulseaudio.enable = true;

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "lat9w-16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget

    firefox
    calibre

    gitAndTools.gitFull

    cabal2nix

    aspell
    aspellDicts.en

    gcc
    nasm

    emacs
    emacs24Packages.haskellMode
    emacs24Packages.autoComplete
    emacs24Packages.jabber
    emacs24Packages.quack

    emacs24PackagesNg.evil
    emacs24PackagesNg.markdown-mode
    emacs24PackagesNg.multiple-cursors
    emacs24PackagesNg.popup
    emacs24PackagesNg.structured-haskell-mode
    emacs24PackagesNg.undo-tree

    ghc
    cabal-install

    kde4.plasma-nm
    kde4.kdemultimedia
    kde4.kdegraphics
    kde4.kdeutils
    kde4.applications
    kde4.kdegames
    kde4.kdebindings
    kde4.kdeaccessibility
    kde4.kde_baseapps
    kde4.kactivities
    kde4.kdeadmin
    kde4.kdenetwork
    kde4.kdeplasma_addons
    kde4.kdetoys
    kde4.kde_wallpapers
    kde4.oxygen_icons
    kde4.kdebase_workspace
    kde4.kdelibs
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # NTP (time server) settings:
  services.ntp.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.kdm.enable = true;
  services.xserver.desktopManager.kde4.enable = true;

  # Touchpad settings
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.tapButtons = false;
  services.xserver.synaptics.maxSpeed = "2";
  services.xserver.synaptics.twoFingerScroll = true;
  services.xserver.synaptics.additionalOptions = ''
      Option "EmulateMidButtonTime" "0"
      Option "ClickFinger1" "1"
      Option "ClickFinger2" "2"
      Option "ClickFinger3" "3"
    '';

  users.extraUsers.tikhon = {
    isNormalUser = true;
    home = "/home/tikhon";
    description = "Tikhon Jelvis";
    extraGroups = ["wheel" "networkmanager"];
  };

  # Add a systemd emacs daemon server
  systemd.user.services.emacs = {
    description = "emacs daemon";
    environment = {
      GTK_DATA_PREFIX = config.system.path;
      SSH_AUTH_SOCK = "%t/ssh-agent";
      GTK_PATH = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";
      NIX_PROFILES = "${pkgs.lib.concatStringsSep " " config.environment.profiles}";
      TERMINFO_DIRS = "/run/current-system/sw/share/terminfo";
      ASPELL_CONF = "dict-dir /run/current-system/sw/lib/aspell";
    };
    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.bash}/bin/bash -c 'source ${config.system.build.setEnvironment}; exec emacs --daemon'";
      ExecStop = "${pkgs.emacs}/bin/emacsclient --eval (kill-emacs)";
      Restart = "always";
    };
    wantedBy = [ "default.target" ];
  };

  systemd.services.emacs.enable = true;
}
