{ pkgs, config, ... }:
{
  imports = [
    ./base

    ./emacs/linux.nix

    ./firefox
    ./xmonad

    ./video
  ];

  home = {
    username = "tikhon";
    homeDirectory = "/home/tikhon";

    keyboard.options = ["caps:ctrl_modifier"];

    packages = with pkgs; [
      chromium
      discord
      file
      gimp
      inkscape
      krita
      slack
      kdePackages.spectacle
      synergy
      unison-ucm
      vlc
      zoom-us
    ];

    sessionVariables = {
      NIX_SSL_CERT_FILE   = "/etc/ssl/certs/ca-certificates.crt";
    };
  };

  xdg.enable = true;

  programs.git.userEmail = "tikhon@jelv.is";

  services.lorri.enable = true;

  # 2023-08-15: Not working in 23.05 (or 23.11)
  #
  # See: https://github.com/nix-community/home-manager/issues/4226
  #
  # services.dropbox = {
  #   enable = true;
  #   path = "${config.home.homeDirectory}/Dropbox";
  # };
}
