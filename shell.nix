{ sources ? import ./nix/sources.nix
, pkgs    ? import sources.nixpkgs {}
}:
pkgs.mkShell rec {
  buildInputs = [ pkgs.cachix pkgs.niv ];

  HOME_MANAGER_CONFIG =
    if pkgs.stdenv.isDarwin
    then toString ./home/target-macbook.nix
    else toString ./home/linux.nix;
}
