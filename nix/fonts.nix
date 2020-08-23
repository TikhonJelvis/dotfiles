{ pkgs, ... }:

{
  fonts = {
    fonts = with pkgs; [
      corefonts
      dejavu_fonts
      emacs-all-the-icons-fonts
      eb-garamond
      inconsolata
      powerline-fonts
      source-code-pro
      ubuntu_font_family
    ];
  };
}
