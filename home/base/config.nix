let
  sources = import ../../nix/sources.nix;
in
{
  allowUnfree = true;

  packageOverrides = pkgs: {
    nur = import sources.NUR {
      inherit pkgs;
    };
  };
}
