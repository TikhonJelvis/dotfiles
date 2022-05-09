{ pkgs, config, ... }:
{
  imports = [
    ./linux.nix
  ];

  programs.rofi.extraConfig = {
    # Hack: should be set based off the xserver.dpi setting in my
    # NixOS config, but I don't know how to expose that to
    # home-manager
    dpi = 96 * 2;
  };

  home.sessionVariables = {
    HOME_MANAGER_CONFIG = toString ./x1.nix;
  };
}
