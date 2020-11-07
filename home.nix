{ config, pkgs, ... }:

let
  sources = import ./nix/sources.nix;
  sessionVariables = {
    EDITOR = "emacsclient --create-frame --alternate-editor emacs";
    PS1    = "λ x → \W>";
  };

  aspell-with-dicts = pkgs.aspellWithDicts (d: [d.en d.ru]);

  # Different kinds of packages I use
  packages = with pkgs;
    let
      development  = [ python3 ghc niv ];
      utils        = [ aspell-with-dicts ];
    in development ++ utils;
in
{
  imports = [ ./emacs ];

  nixpkgs.config = {
    allowUnfree = true;

    packageOverrides = pkgs: {
      stable = import sources."nixpkgs-stable" {};
      nur = import sources.NUR {
        inherit pkgs;
      };
    };
  };

  home = {
    inherit packages sessionVariables;

    username = "z0028sn";
    homeDirectory = "/Users/z0028sn";

    file = {
      "." = {
        source = ./home;
        recursive = true;
      };
    };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage when
    # a new Home Manager release introduces backwards incompatible
    # changes.
    #
    # You can update Home Manager without changing this value. See the
    # Home Manager release notes for a list of state version changes
    # in each release.
    stateVersion = "20.09";
  };

  fonts.fontconfig.enable = true;

  news.display = "silent";

  programs = {
    home-manager.enable = true;

    bash = {
      enable = true;
      inherit sessionVariables;
      initExtra = ''
        if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi
        if [ -e $HOME/local/etc/ssl/certs/tgt-ca-bundle.crt ]; then
          export NIX_SSL_CERT_FILE=/Users/z0028sn/local/etc/ssl/certs/tgt-ca-bundle.crt;
        fi
      '';
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;

      ## use lorri if available
      stdlib = ''
        eval "`declare -f use_nix | sed '1s/.*/_&/'`"
        use_nix() {
          if type lorri &>/dev/null; then
            echo "direnv: using lorri from PATH ($(type -p lorri))"
            eval "$(lorri direnv)"
          else
            _use_nix
          fi
        }
      '';
    };

    git = {
      enable = true;
      ignores = [ "*~" ];

      userName = "Tikhon Jelvis";
      userEmail = "tikhon@jelv.is";

      extraConfig = {
        ui.color = "always";
        github.user = "TikhonJelvis";
        core.fileMode = false;
      };
    };
  };
}
