{ pkgs, ... }:

let
  breeze-wallpaper = "${pkgs.breeze-qt5}/share/wallpapers/Next/contents/images/2560x1440.jpg";
  wallpaper =
    if builtins.pathExists breeze-wallpaper
    then "feh --bg-fill ${breeze-wallpaper} &\n"
    else builtins.trace "WARNING: xmonad: background image for feh does not exist" "";
  polybar = ''
    nm-applet --sm-disable --indicator &
  '';
in
{
  home.packages = with pkgs; [ dmenu xorg.xmessage networkmanagerapplet ];

  xsession = {
    enable = true;
    scriptPath = ".xsession-hm";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };

    initExtra = wallpaper + polybar;
  };

  programs.feh = {
    enable = true;
  };

  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
    theme = "Indego";
  };

  services.polybar = {
    enable = true;
    script = "polybar top &";

    settings = {
      text = {
        tray-padding = 3;
        tray-detached = false;
        tray-scale = 1.0;

        wm-restack = "generic";
      };

      "module/date" = {
        type = "internal/date";
        date = "%Y-%m-%d";
        time = "%H:%M";

        format = "<label>";
        format-prefix = "🕓 ";
        label = "%time%";
      };

      "module/battery" = {
        type = "internal/battery";
        battery = "BAT0";
        adapter = "AC";
      };

      "bar/top" = {
        width = "100%";
        height = "20";
        radius = "4.0";
        fixed-center = true;
        line-size = 1;

        tray-position = "right";
        tray-padding = 5;

        modules-right = "date battery";

        font = [
          "DejaVu Sans:style=Book:size=12;3"
          "Twitter Color Emoji:style=Regular:scale=7;3"
        ];
      };

      "global/wm" = {
        margin-bottom = 0;
        margin-top = 0;
      };
    };
  };
}
