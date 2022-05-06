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

    packages = with pkgs; [
      chromium
      discord
      file
      gimp
      gwenview
      inkscape
      krita
      slack
      spectacle
      synergy
      vlc
      zoom-us
    ];

    sessionVariables = {
      NIX_SSL_CERT_FILE   = "/etc/ssl/certs/ca-certificates.crt";
      HOME_MANAGER_CONFIG = toString ./linux.nix;
    };
  };

  xdg.enable = true;

  programs.git.userEmail = "tikhon@jelv.is";

  services.lorri.enable = true;

  services.dropbox = {
    enable = true;
    path = "${config.home.homeDirectory}/Dropbox";
  };
}
