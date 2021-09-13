{ pkgs, lib, ... }:
let
  ffmpeg-srt = pkgs.ffmpeg.overrideAttrs (oldAttrs: rec {
    configureFlags = oldAttrs.configureFlags ++ [ "--enable-libsrt" ];
    buildInputs = oldAttrs.buildInputs ++ [ pkgs.srt ];
  });

  wrapOBS = pkgs.wrapOBS.override {
    obs-studio = pkgs.obs-studio.override {
      ffmpeg = ffmpeg-srt;
    };
  };

  obs-studio = wrapOBS {
    plugins = with pkgs.obs-studio-plugins; [
      obs-ndi
    ];
  };
in
{
  home.packages = [ obs-studio ];
}
