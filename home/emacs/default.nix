{ pkgs, config, lib, ... }:
{
  options.emacs = lib.mkOption {
    type = lib.types.package;
    default = pkgs.emacs;
    description = "Base Emacs packages to start from.";
  };

  config = {
    nixpkgs.overlays = [ (import config.sources.emacs-overlay) ];

    programs.emacs = {
      enable = true;
      package = pkgs.emacsWithPackagesFromUsePackage {
        package = config.emacs;
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
  };
}
