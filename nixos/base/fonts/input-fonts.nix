{ pkgs, lib, ... }:
let
  # Custom settings for Input font—variations for "a", "g", "*"... etc
  input-fonts-url = { a, g, i, l, zero, asterisk, braces, line-height }:
    let options = "a=${a}&g=${g}&i=${i}&l=${l}&zero=${zero}&braces=${braces}&line-height=${line-height}"; in
    "https://input.djr.com/build/?fontSelection=whole&${options}&preset=default&accept=I+do&email=&.zip";
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
        g           = "1";
        i           = "3";
        l           = "4";
        zero        = "0";
        asterisk    = "1";
        braces      = "1";
        line-height = "1.3";
      };
      sha256 = "0c59ygh3kmpzyjwsr6sh3x29279qjcabk198f29plp5l5wz1dalf";
    })
  ];

  nixpkgs.config.input-fonts.acceptLicense = true;
}
