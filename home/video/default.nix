{ pkgs, lib, ... }:
let
  obs-studio = pkgs.wrapOBS {
    plugins = with pkgs.obs-studio-plugins; [
      obs-teleport
    ];
  };
in
{
  home.packages = [ obs-studio ];
}
