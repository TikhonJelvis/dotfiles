let
  sources = import ./nix/sources.nix;
  pkgs-source = sources.nixpkgs-darwin;
  pkgs = import pkgs-source {};
  home-manager = (import sources.home-manager { inherit pkgs; }).home-manager;
in
pkgs.mkShell rec {
  name = "home-manager-shell";
  buildInputs = [ pkgs.cachix pkgs.niv home-manager ];

  NIX_PATH =
    "nixpkgs=${pkgs-source}:home-manager=${sources.home-manager}";
  HOME_MANAGER_CONFIG =
    if pkgs.stdenv.isDarwin
    then "home/target-macbook.nix"
    else "home/linux.nix";
}
