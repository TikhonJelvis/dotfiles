{ pkgs, config, ... }:
{
  imports = [
    ./linux.nix
    ./video
  ];

  home.sessionVariables = {
    HOME_MANAGER_CONFIG = toString ./o11.nix;
  };
}
