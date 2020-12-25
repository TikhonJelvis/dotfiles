{ config, pkgs, ...}:
let
  sessionVariables = {
    EDITOR = "emacsclient --create-frame --alternate-editor emacs";
    PS1    = "λ x → \W>";
  };

  aspell-with-dicts = pkgs.aspellWithDicts (d: [d.en d.ru]);

  packages = with pkgs;
    let
      development  = [ ghc lorri niv python3 ];
      utils        = [ aspell-with-dicts ];
    in development ++ utils;
in
{
  imports = [ ./sources.nix ];

  nixpkgs.config = {
    allowUnfree = true;

    packageOverrides = pkgs: {
      stable = import config.sources."nixpkgs-stable" {};
      nur = import config.sources.NUR {
        inherit pkgs;
      };
    };
  };

  programs.direnv = {
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

  home = {
    inherit packages sessionVariables;

    file = {
      "." = {
        source = ../files;
        recursive = true;
      };
      ".aspell.conf" = ''
        ${config.home.homeDirectory}/.nix-profile/lib/aspell
      '';
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
    bash = {
      enable = true;
      inherit sessionVariables;

      initExtra = ''
        if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]
        then
          . $HOME/.nix-profile/etc/profile.d/nix.sh
        fi
      '';
    };

    git = {
      enable = true;
      ignores = [ "*~" ];

      userName = "Tikhon Jelvis";

      extraConfig = {
        ui.color = "always";
        github.user = "TikhonJelvis";
        core.fileMode = false;
      };
    };
  };
}
