{ pkgs, ... }:
{
  imports = [
    ./base
    ./base/darwin.nix
    ./emacs/darwin.nix
  ];

  home = {
    username = "tikhon.jelvis";
    homeDirectory = "/Users/tikhon.jelvis";
  };

  programs.git = {
    ignores = [ ".DS_Store" ];
    settings.user.email = "tikhon@jelv.is";
  };
}
