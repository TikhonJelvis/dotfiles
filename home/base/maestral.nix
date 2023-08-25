# Maestral module definition based on Peter Hoeg's code:
# https://github.com/NixOS/nixpkgs/issues/235345#issuecomment-1586233679
#
# as well as the built-in home-manager Dropbox module:
# https://github.com/nix-community/home-manager/blob/master/modules/services/dropbox.nix
{ config, lib, pkgs, ... }:
let cfg = config.services.maestral;
    inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
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
        Unit = { Description = "Maestral (Dropbox)"; };
        Install = { WantedBy = [ "default.target" ]; };

        Service = {
          ExecStart = "${pkgs.maestral}/bin/maestral start -f";
          ExecStop = "${pkgs.maestral}/bin/maestral stop";
          Restart = "on-failure";
          Nice = 10;
        };
      };
    };
  };
}
