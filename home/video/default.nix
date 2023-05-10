{ pkgs, lib, ... }:
let
  obs-studio = pkgs.wrapOBS {
    plugins = with pkgs.obs-studio-plugins; [
      # 2023-05-08: disabling NDI because prefetching the file isn't
      # working
      # obs-ndi
    ];
  };
in
{
  home.packages = [ obs-studio ];
}
