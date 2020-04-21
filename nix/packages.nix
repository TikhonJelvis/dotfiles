# Packages that I want on every machine that I use.
#
# To clear the user environment and install all these packages, run
# the switch script in this directory.
let
  pkgs = import <nixpkgs> {};
  aspell-dicts = d: [d.en];
in
{
  # emacs = pkgs.emacs.override { imagemagick = pkgs.imagemagickBig; };
  inherit (pkgs) imagemagick nix ghc stylish-haskell;
  aspell = pkgs.aspellWithDicts aspell-dicts;
}
