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

    packages = with pkgs;
      [ krita chromium gwenview slack spectacle synergy zoom-us ];
  };

  programs.git.userEmail = "tikhon@jelv.is";

  services.lorri.enable = true;

  services.dropbox = {
    enable = true;
    path = "${config.home.homeDirectory}/Dropbox";
  };
}
