args@{ confPath, ... }:
let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
import "${sources.home-manager}/home-manager/home-manager.nix" (args // { inherit pkgs; })
