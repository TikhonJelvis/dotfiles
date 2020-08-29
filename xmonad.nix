{ pkgs, ... }:

{
  xsession = {
    enable = true;
    scriptPath = ".xsession-hm";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad/xmonad.hs;
    };
  };

  home.packages = with pkgs; [ dmenu xorg.xmessage ];
}
