{ pkgs, ... }:

let
  breeze-wallpaper = "${pkgs.breeze-qt5}/share/wallpapers/Next/contents/images/2560x1440.jpg";
in
{
  xsession = {
    enable = true;
    scriptPath = ".xsession-hm";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };

    initExtra = if builtins.pathExists breeze-wallpaper
                then "feh --bg-fill ${breeze-wallpaper}"
                else builtins.trace "WARNING: xmonad: background image for feh does not exist" "";
  };

  programs.feh = {
    enable = true;
  };

  home.packages = with pkgs; [ dmenu xorg.xmessage ];
}
