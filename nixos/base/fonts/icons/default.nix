{ pkgs, ... }:
let
  icon-font = pkgs.stdenv.mkDerivation {
    name = "tikhon-icon-font";
    src = ./font;
    installPhase = ''
      mkdir -p $out/share/fonts/truetype
      mv tikhon-emacs-icons.ttf $out/share/fonts/truetype/
    '';
  };
in
{
  fonts.packages = [ icon-font ];
}
