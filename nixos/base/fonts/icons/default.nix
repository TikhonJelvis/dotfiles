{ pkgs, ... }:
let
  icon-font = pkgs.stdenv.mkDerivation {
    name = "tikhon-icon-font";
    src = ./font;
    installPhase = ''
      mkdir -p $out/share/fonts
      cp *.ttf $out/share/fonts/truetype
    '';
  };
in
{
  fonts.fonts = [ icon-font ];
}
