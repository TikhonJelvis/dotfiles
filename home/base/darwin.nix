{ pkgs, lib, config, ... }:
let
  input-font = (import ./input-font.nix { inherit pkgs lib; }).input-font;
in
{
  fonts.fontconfig.enable = true;
  home.packages = [
    pkgs.nerd-fonts.symbols-only
    input-font
  ];
}
