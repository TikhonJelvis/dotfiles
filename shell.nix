{ sources ? import ./nix/sources.nix
, pkgs    ? import sources.nixpkgs {}
}:
pkgs.mkShell rec {
  buildInputs = [ pkgs.cachix pkgs.niv pkgs.home-manager ];

  HOME_MANAGER_CONFIG =
    if pkgs.stdenv.isDarwin
    then toString ./home/semgrep-macbook.nix
    else toString ./home/linux.nix;

  NIX_PATH = "nixpkgs=${sources.nixpkgs}:nixpkgs/nixos=${sources.nixpkgs}:nixos-config=/etc/nixos/configuration.nix";
}
