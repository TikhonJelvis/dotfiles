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

  programs = {
    bash.initExtra = ''
      eval "$(/opt/homebrew/bin/brew shellenv)"
    '';

    zsh.initContent = ''
      eval "$(/opt/homebrew/bin/brew shellenv)"
      source ~/.orbstack/shell/init.zsh 2>/dev/null || :
    '';
  };
}
