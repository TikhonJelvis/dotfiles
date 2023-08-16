{ pkgs, lib, ... }:
let
  version = "1.0";
  junction = (pkgs.fetchzip rec {
    name = "junction-${version}";
    url = "https://github.com/theleagueof/junction/archive/master.zip";
    sha256 = "110jsijn3i26d2487c9680w2z8g3bxflxjlsc4yhyc3wbmb7v1b8";

    # Modified based on code in nixpkgs PR #157381, fixing an issue
    # caused by #173430
    postFetch = ''
      mkdir -p $out/share/fonts/truetype
      mv $out/webfonts/*.ttf -t $out/share/fonts/truetype

      mkdir -p $out/share/fonts/opentype
      mv $out/*.otf -t $out/share/fonts/opentype

      shopt -s extglob dotglob
      rm -rf $out/!(share)
      shopt -u extglob dotglob
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
  fonts.packages = [ junction ];
}

