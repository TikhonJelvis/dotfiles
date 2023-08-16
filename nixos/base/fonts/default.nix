{ pkgs, lib, ... }:

{
  imports = [
    ./input-fonts.nix
    ./junction.nix
    ./icons
  ];

  fonts = {
    packages = with pkgs; [
      corefonts
      cozette
      dejavu_fonts
      emacs-all-the-icons-fonts
      emojione
      eb-garamond
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

    enableDefaultPackages = true;
  };
}
