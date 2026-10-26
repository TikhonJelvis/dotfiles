{ pkgs, ... }:
let
  epage = pkgs.writers.writeHaskellBin "epage" { } (builtins.readFile ./epage.hs);
in {
  home.file."local/bin/epage".source = "${epage}/bin/epage";

  home.packages = [
    (pkgs.writers.writeHaskellBin "hm"
      { libraries = [ pkgs.haskellPackages.turtle ]; }
      (builtins.readFile ./hm.hs))
  ];
}
