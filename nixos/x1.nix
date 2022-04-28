{ config, pkgs, ... }:
{
  imports = [ ./base ];

  networking = {
    hostName = "tikhon-nixos-x1";

    networkmanager.enable = true;
  };

  users.users.tikhon.extraGroups = [ "networkmanager" ];
}
