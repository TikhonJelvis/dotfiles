{ pkgs, lib, ... }:

{
  imports = [
    ./input-fonts.nix
  ];

  fonts = {
    fonts = with pkgs; [
      corefonts
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
