{ pkgs, lib, ... }:

{
  imports = [
    ./input-fonts.nix
    ./junction.nix
  ];

  fonts = {
    fonts = with pkgs; [
      corefonts
      cozette
      dejavu_fonts
      emacs-all-the-icons-fonts
      eb-garamond
      fira-code
      hack-font
      hasklig
      inconsolata
      noto-fonts
      powerline-fonts
      source-code-pro
      ubuntu_font_family
    ];

    enableDefaultFonts = true;
  };
}
