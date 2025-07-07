{ pkgs, ... }:
{
  imports = [
    ./base
    ./emacs/darwin.nix
  ];

  home = {
    username = "tikhon";
    homeDirectory = "/Users/tikhon";
  };

  programs.git = {
    ignores = [ ".DS_Store" ];

    userEmail = "tikhon@jelv.is";
  };
}
