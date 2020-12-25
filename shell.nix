let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs-darwin {};
  home-manager = (import sources.home-manager { inherit pkgs; }).home-manager;
in
pkgs.mkShell rec {
  name = "home-manager-shell";
  buildInputs = [ pkgs.cachix pkgs.niv home-manager ];
  shellHook = ''
    export NIX_PATH="nixpkgs=${sources.nixpkgs-darwin}:home-manager=${sources.home-manager}"
    export HOME_MANAGER_CONFIG="./home.nix"
    export DARWIN="${pkgs.lib.boolToString pkgs.stdenv.isDarwin}"
  '';
}
