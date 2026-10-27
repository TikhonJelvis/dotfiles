{ pkgs, config, lib, ... }:
let
  # Tree-sitter grammars built via Nix instead of downloaded at
  # runtime by `treesit-install-language-grammar'.
  #
  # Saved to ~/.emacs.d/tree-sitter
  #
  # Keys are language names as treesit knows them.
  grammars = with pkgs.tree-sitter-grammars; {
    bash = tree-sitter-bash;
    css = tree-sitter-css;
    dockerfile = tree-sitter-dockerfile;
    html = tree-sitter-html;
    javascript = tree-sitter-javascript;
    json = tree-sitter-json;
    lua = tree-sitter-lua;
    markdown = tree-sitter-markdown;
    markdown-inline = tree-sitter-markdown-inline;
    python = tree-sitter-python;
    rust = tree-sitter-rust;
    toml = tree-sitter-toml;
    tsx = tree-sitter-tsx;
    typescript = tree-sitter-typescript;
    yaml = tree-sitter-yaml;
  };

  treesitGrammars = pkgs.runCommand "emacs-treesit-grammars" { } ''
    mkdir -p $out
    ${lib.concatStrings (lib.mapAttrsToList (lang: grammar: ''
      ln -s ${grammar}/parser $out/libtree-sitter-${lang}.so
    '') grammars)}
  '';
in {
  options.emacs = lib.mkOption {
    type = lib.types.package;
    default = pkgs.emacs31;
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
          inherit (pkgs.emacsPackages) ghostel;

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
      [ pkgs.pyright yaml-language-server taplo ];

    home.file = {
      ".emacs" = {
        text = ''
          (defun dotfile (file)
            "Return an absolute path to the given relative path within
          the dotfiles directory that was used to generate this
          config."
            (expand-file-name file "${toString ./..}"))

          (defconst is-work ${if config.work.enable then "t" else "nil"}
            "Non-nil on work machines (the `work.enable' Nix option).")

          (load "${toString ./init.el}")
        '';
      };

      ".emacs.d/tree-sitter".source = treesitGrammars;
    };
  };
}
