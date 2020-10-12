{ pkgs, ... }:

let
  sources = import ../nix/sources.nix;
  emacs-overlay = import sources.emacs-overlay;
in
{
  nixpkgs.overlays = [ emacs-overlay ];

  # External packages only needed for my Emacs setup:
  home.packages = with pkgs; [ python-language-server ];

  services.emacs = {
    enable = true;

    # temporarily disabled; see:
    # https://github.com/nix-community/emacs-overlay/issues/58
    # client.enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacsGcc;
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
