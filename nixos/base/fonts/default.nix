{ pkgs, lib, ... }:

let
  # for reference: this is how we can pull in *all* Nerd fonts
  #
  # fonts.packages = [...] ++ all-nerd-fonts
  #
  # for now, though, I should only need pkgs.nerd-fonts.symbols-only
  all-nerd-fonts =
    builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
in {
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
      nerd-fonts.symbols-only
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
