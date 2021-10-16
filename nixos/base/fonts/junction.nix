{ pkgs, lib, ... }:
let
  version = "1.0";
  junction = (pkgs.fetchzip rec {
    name = "junction-${version}";
    url = "https://github.com/theleagueof/junction/archive/master.zip";
    sha256 = "110jsijn3i26d2487c9680w2z8g3bxflxjlsc4yhyc3wbmb7v1b8";

    postFetch = ''
      mkdir -p $out/share/fonts
      unzip -j $downloadedFile \*.ttf -d $out/share/fonts/truetype
      unzip -j $downloadedFile \*.otf -d $out/share/fonts/opentype
    '';

    meta = with lib; {
      description = "A humanist sans-serif, and the first open-source type project started by The League of Moveable Type. It has been updated (2014) to include additional weights (light/bold) and expanded international support.";
      homepage = "https://www.theleagueofmoveabletype.com/junction";
      license = licenses.ofl;
      platforms = platforms.all;
    };
  });
in
{
  fonts.fonts = [ junction ];
}

