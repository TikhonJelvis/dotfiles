{ pkgs, ... }:
let
  obs = pkgs.wrapOBS {
    plugins = with pkgs.obs-studio-plugins; [
      obs-ndi
    ];
  };
in
{
  home.packages = [ obs ];
}
