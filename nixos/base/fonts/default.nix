{ pkgs, lib, ... }:

{
  imports = [
    ./input-fonts.nix
    ./junction.nix
    ./icons
  ];

  fonts = {
    fonts = with pkgs; [
      corefonts
      cozette
      dejavu_fonts
      emacs-all-the-icons-fonts
      emojione
      eb-garamond
      fira-code
      hack-font
      hasklig
      inconsolata
      noto-fonts
      powerline-fonts
      roboto
      roboto-mono
      source-code-pro
      symbola
      twitter-color-emoji
      ubuntu_font_family
    ];

    enableDefaultFonts = true;
  };
}
