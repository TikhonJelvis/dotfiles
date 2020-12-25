{ pkgs, config, ... }:
{
  imports = [
    ./base

    ./emacs/linux.nix

    ./firefox
    ./xmonad
    ./video
  ];

  home = {
    username = "tikhon";
    homeDirectory = "/home/tikhon";

    packages = with pkgs;
      [ krita chromium gwenview slack spectacle synergy zoom-us ];
  };

  programs.git.userEmail = "tikhon@jelv.is";

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;

    ## use lorri if available
    stdlib = ''
      eval "`declare -f use_nix | sed '1s/.*/_&/'`"
      use_nix() {
        if type lorri &>/dev/null; then
          echo "direnv: using lorri from PATH ($(type -p lorri))"
          eval "$(lorri direnv)"
        else
          _use_nix
        fi
      }
    '';
  };

  services.lorri.enable = true;

  services.dropbox = {
    enable = true;
    path = "${config.home.homeDirectory}/Dropbox";
  };
}
