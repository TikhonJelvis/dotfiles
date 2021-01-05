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

    home.packages = with pkgs;
      [ nodePackages.pyright ];

    home.file = {
      ".emacs" = {
        text = ''
          (defun dotfile (file)
            "Return an absolute path to the given relative path within
          the dotfiles directory that was used to generate this
          config."
            (expand-file-name file "${toString ./..}"))

          (load "${toString ./init.el}")
        '';
      };
    };
  };
}
