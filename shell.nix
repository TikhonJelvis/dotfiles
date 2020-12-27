{ sources ? import ./nix/sources.nix
, pkgs    ? import sources.nixpkgs {}
}:
let
  home-manager = (import sources.home-manager { inherit pkgs; }).home-manager;
in
pkgs.mkShell rec {
  buildInputs = [ pkgs.cachix pkgs.niv home-manager ];

  HOME_MANAGER_CONFIG =
    if pkgs.stdenv.isDarwin
    then toString ./home/target-macbook.nix
    else toString ./home/linux.nix;
}
