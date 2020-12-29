let
  sources = import ../../nix/sources.nix;
in
{
  allowUnfree = true;

  packageOverrides = pkgs: {
    stable = import sources.nixpkgs-darwin {};
    nur = import sources.NUR {
      inherit pkgs;
    };
  };
}
