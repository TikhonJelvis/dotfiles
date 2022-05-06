{ pkgs, config, ... }:
{
  imports = [
    ./linux.nix
    ./video
  ];

  home.sessionVariables = {
    HOME_MANAGER_CONFIG = toString ./berkeley.nix;
  };
}
