{ sources ? import ./nix/sources.nix
, pkgs    ? import sources.nixpkgs {}
}:
let
  HOME_MANAGER_CONFIG =
    if pkgs.stdenv.isDarwin
    then toString ./home/semgrep-macbook.nix
    else toString ./home/linux.nix;
  NIX_PATH = "nixpkgs=${sources.nixpkgs}:nixpkgs/nixos=${sources.nixpkgs}:nixos-config=/etc/nixos/configuration.nix";
in pkgs.mkShell rec {
  buildInputs = [ pkgs.cachix pkgs.niv pkgs.home-manager ];

  shellHook = ''
    export HOME_MANAGER_CONFIG='${HOME_MANAGER_CONFIG}'
    export NIX_PATH='${NIX_PATH}'
  '';
}
