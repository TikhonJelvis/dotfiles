{ pkgs, config, ... }:
{
  imports = [
    ./linux.nix
  ];

  home.sessionVariables = {
    HOME_MANAGER_CONFIG = toString ./framework.nix;
  };
}
