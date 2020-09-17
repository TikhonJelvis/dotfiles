{ pkgs, ... }:

let
  sources = import ./nix/sources.nix;
  emacs-overlay = import sources.emacs-overlay;
in
{
  nixpkgs.overlays = [ emacs-overlay ];

  services.emacs = {
    enable = true;

    # temporarily disabled; see:
    # https://github.com/nix-community/emacs-overlay/issues/58
    # client.enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs;
      config = ./emacs/init.el;
    };
  };

  home.file = {
    ".emacs.d" = {
      source = ./emacs;
      recursive = true;
    };
  };
}
