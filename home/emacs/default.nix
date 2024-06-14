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
        override = epkgs: epkgs // {
          # patch treemacs to fix eager macro-expand error
          #
          # see several related issues:
          #
          # https://github.com/emacs-lsp/dap-mode/issues/673
          # https://github.com/emacs-lsp/lsp-metals/issues/81
          # https://github.com/Alexander-Miller/treemacs/issues/982
          treemacs = epkgs.melpaPackages.treemacs.overrideAttrs(old: {
            patches = [ ./treemacs-treelib-el.patch ];
          });

          lean4-mode = pkgs.callPackage ./lean4-mode.nix {
            inherit pkgs lib epkgs;
          };
        };
      };
    };

    home.packages = with pkgs;
      [ yaml-language-server taplo-lsp nerdfonts ];

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
