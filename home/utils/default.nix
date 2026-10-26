{ pkgs, ... }:
{
  home.file."local/bin/epage".source = "${import ./haskell {}}/bin/epage";

  home.packages = [
    (pkgs.writers.writeHaskellBin "hm"
      { libraries = [ pkgs.haskellPackages.turtle ]; }
      (builtins.readFile ./hm.hs))
  ];
}
