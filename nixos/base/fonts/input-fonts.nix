{ pkgs, lib, ... }:
let
  # Custom settings for Input font—variations for "a", "g", "*"... etc
  input-fonts-url = { a, g, i, l, zero, asterisk, braces, line-height, preset }:
    let options = "a=${a}&g=${g}&i=${i}&l=${l}&zero=${zero}&braces=${braces}&line-height=${line-height}&preset=${preset}"; in
    "https://input.djr.com/build/?fontSelection=whole&${options}&accept=I+do&email=&.zip";
  input-fonts-src = { options, version, sha256 }:
    let releaseDate = "2015-06-24"; in
    pkgs.fetchzip {
      inherit sha256;
      name = "input-fonts-${version}";
      url = input-fonts-url options;
      stripRoot = false;

      extraPostFetch = ''
        # Reset the timestamp to release date for determinism.
        PATH=${lib.makeBinPath [ pkgs.python3.pkgs.fonttools ]}:$PATH
        for ttf_file in $out/Input_Fonts/*/*/*.ttf; do
          ttx_file=$(dirname "$ttf_file")/$(basename "$ttf_file" .ttf).ttx
          ttx "$ttf_file"
          rm "$ttf_file"
          touch -m -t ${builtins.replaceStrings [ "-" ] [ "" ] releaseDate}0000 "$ttx_file"
          ttx --recalc-timestamp "$ttx_file"
          rm "$ttx_file"
        done
      '';
    };

  input-font = { sha256, options }: pkgs.input-fonts.overrideAttrs (old: rec {
    src = input-fonts-src { inherit sha256 options; inherit (pkgs.input-fonts) version; };
  });
in
{
  fonts.fonts = [
    (input-font {
      options = {
        a           = "0";
        g           = "ss";
        i           = "serifs";
        l           = "serifs_round";
        zero        = "0";
        asterisk    = "height";
        braces      = "straight";
        preset      = "default";
        line-height = "1.3";
      };
      sha256 = "18g89p526295c9h17y1gcwlfb6phxrw6nlhdvn4z2czapg0pzngh";
    })
  ];

  nixpkgs.config.input-fonts.acceptLicense = true;
}
