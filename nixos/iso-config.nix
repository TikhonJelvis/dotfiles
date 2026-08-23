{ config, pkgs, lib, ... }:
let
  password-file = /home + "/${config.default-user.name}/pass";
in
{
  imports = [
    ./base/user.nix
  ];

  environment.systemPackages = with pkgs;
    [ git emacs gparted firefox xterm ];

  environment.etc."dotfiles".source = builtins.fetchGit { url = ../.; };
  environment.etc."bin/install-helper" = {
    source = ./bin/install-helper;
    mode = "0755";
  };
  environment.etc."nixos-configs/new-user".text = config.default-user.name;
  environment.etc."nixos-configs/password-hash" =
    lib.mkIf (builtins.pathExists password-file) {
      source = password-file;
    };

  # SSH
  services.openssh.enable = true;

  # enough of an X setup to run gparted and nm applet (via i3):
  services.xserver = {
    enable = true;
    displayManager.startx.enable = true;
    desktopManager.xterm.enable = false;
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [ networkmanagerapplet dmenu i3status ];
      extraSessionCommands = ''
        ${pkgs.networkmanagerapplet}/bin/nm-applet &
      '';
      configFile = pkgs.writeText "i3-live-config" ''
        set $mod Mod1
        font pango:monospace 10

        bindsym $mod+Return exec i3-sensible-terminal
        bindsym $mod+d exec dmenu_run
        bindsym $mod+Shift+e exit

        bar {
            status_command ${pkgs.i3status}/bin/i3status
        }
      '';
    };
  };
  services.displayManager.defaultSession = "none+i3";

  # to let me choose a WiFi network from a GUI widget:
  environment.etc."icewm/startup".text = ''
    #!/bin/sh
    nm-applet &
  '';
}
