{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
haskellPackages.ghcWithPackages (pkgs: with pkgs; [ xmonad xmonad-contrib ])

