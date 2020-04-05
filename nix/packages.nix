# Packages that I want on every machine that I use.
#
# To clear the user environment and install all these packages, run
# the switch script in this directory.
with import <nixpkgs> {};
let
  aspell-dicts = d: [d.en];
in
{
  inherit nix ghc stylish-haskell;
  aspell = pkgs.aspellWithDicts aspell-dicts;
}
