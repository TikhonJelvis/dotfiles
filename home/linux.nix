{ pkgs, config, ... }:
{
  imports = [
    ./base

    ./emacs/linux.nix

    ./firefox
    ./xmonad
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
      gwenview
      inkscape
      krita
      slack
      spectacle
      synergy
      unison-ucm
      xournal
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

  services.dropbox = {
    enable = true;
    path = "${config.home.homeDirectory}/Dropbox";
  };
}
