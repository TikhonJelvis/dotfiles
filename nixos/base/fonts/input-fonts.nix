{ pkgs, lib, ... }:
let input = import ./input-font.nix { inherit pkgs lib; };
    input-font = input.input-font;
in
{
  fonts.packages = [
    input-font
  ];

  nixpkgs.config.input-fonts.acceptLicense = true;
}
