{ pkgs, ... }:
{
  fonts.packages = [
    (import ./input-font.nix).input-font
  ];

  nixpkgs.config.input-fonts.acceptLicense = true;
}
