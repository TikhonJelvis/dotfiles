# Maestral module definition based on Peter Hoeg's code:
# https://github.com/NixOS/nixpkgs/issues/235345#issuecomment-1586233679
#
# as well as the built-in home-manager Dropbox module:
# https://github.com/nix-community/home-manager/blob/master/modules/services/dropbox.nix
{ config, lib, pkgs, ... }:
let cfg = config.services.maestral;
    inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
    ini = pkgs.formats.ini { };

    maestral-ini = ini.generate "maestral-override.ini" {
      app = {
        analytics = "False"; # "False" has to be capitalized
        notification_level = 30; # SYNCISSUE
        update_notification_interval = 0;
      };
    };
in {
  options = {
    services.maestral = {
      enable = mkEnableOption "Maestral";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.maestral ];

    systemd.user.services = {
      maestral = rec {
        Unit = { description = "Maestral (Dropbox)"; };
        Install = { WantedBy = [ "default.target" ]; };

        Service = {
          Type = "forking";
          PIDFile = "${config.home.homeDirectory}/.maestral/maestral.pid";

          Restart = "on-failure";
          PrivateTmp = true;
          ProtectSystem = "full";
          Nice = 10;

          ExecStartPre = ''
          /bin/sh -c '${pkgs.crudini}/bin/crudini --merge $HOME/.config/maestral/maestral.ini < ${maestral-ini}'
        '';
          ExecStart = "${lib.getBin pkgs.maestral}/bin/maestral start --foreground";
          ExecStop = "${lib.getBin pkgs.maestral}/bin/maestral stop";
          ExecReload = "${lib.getBin pkgs.coreutils}/bin/kill -HUP $MAINPID";
          
          WatchdogSec = "60s";
          RuntimeDirectory = "maestral";
        };
      };
    };
  };
}
