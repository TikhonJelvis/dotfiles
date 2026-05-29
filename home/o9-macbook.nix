{ pkgs, ... }:
{
  imports = [
    ./base
    ./emacs/darwin.nix
  ];

  home = {
    username = "tikhon.jelvis";
    homeDirectory = "/Users/tikhon.jelvis";
  };

  programs.git = {
    ignores = [ ".DS_Store" ];

    userEmail = "tikhon@jelv.is";
  };
}
