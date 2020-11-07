{ pkgs, ... }:

let
  sources = import ../nix/sources.nix;
  emacs-overlay = import sources.emacs-overlay;
  emacs-darwin = import ./emacs-darwin.nix { inherit pkgs; };

  emacs = if pkgs.stdenv.isDarwin
          then emacs-darwin
          else pkgs.emacsGcc;
in
{
  nixpkgs.overlays = [ emacs-overlay ];

  # External packages only needed for my Emacs setup:
  home.packages = with pkgs; [
    # Temporarily(?) disabled on macOS:
    # python-language-server
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = emacs;
      config = ./init.el;
    };
  };

  home.file = {
    ".emacs.d" = {
      source = ./.emacs.d;
      recursive = true;
    };
    ".emacs" = {
      source = ./init.el;
    };
  };
}
