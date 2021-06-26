{ pkgs, ... }:
{
  home.packages = with pkgs; [ obs-studio ];

  # Managing the OBS-NDI plugin.
  #
  # Could be extended into an "OBS plugin" set of modules, probably...
  # home.file.".config/obs-studio/plugins/obs-ndi/bin/64bit/obs-ndi.so".source =
  #   "${pkgs.obs-ndi}/lib/obs-plugins/obs-ndi.so";
  # home.file.".config/obs-studio/plugins/obs-ndi/data/locale/en-US.ini".source =
  #   "${pkgs.obs-ndi}/share/obs/obs-plugins/obs-ndi/locale/en-US.ini";
}
