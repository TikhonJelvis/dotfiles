{ pkgs, ... }:
{
  home.file."local/bin/epage".source = "${import ./haskell {}}/bin/epage";
}
