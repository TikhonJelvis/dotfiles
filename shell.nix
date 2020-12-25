let
  sources = import ./nix/sources.nix;
  pkgs-source = sources.nixpkgs-darwin;
  pkgs = import pkgs-source {};
  home-manager = (import sources.home-manager { inherit pkgs; }).home-manager;
in
pkgs.mkShell rec {
  name = "home-manager-shell";
  buildInputs = [ pkgs.cachix pkgs.niv home-manager ];

  NIX_PATH = pkgs.lib.concatStringsSep ":"
    ([ "nixpkgs=${pkgs-source}"
       "home-manager=${sources.home-manager}"
     ] ++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin)
       [
         "nixos-config=/etc/nixos/configuration.nix"
         "nixpkgs/nixos=${sources.nixos-stable}"
       ]
    );
  HOME_MANAGER_CONFIG =
    if pkgs.stdenv.isDarwin
    then "home/target-macbook.nix"
    else "home/linux.nix";
}
