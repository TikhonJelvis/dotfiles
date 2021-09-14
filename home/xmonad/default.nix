{ pkgs, ... }:

let
  breeze-wallpaper = "${pkgs.breeze-qt5}/share/wallpapers/Next/contents/images/2560x1440.png";
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

    initExtra = "feh --bg-fill ${breeze-wallpaper}";
  };

  programs.feh = {
    enable = true;
  };

  home.packages = with pkgs; [ dmenu xorg.xmessage ];
}
