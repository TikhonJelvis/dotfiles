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
      applications = [ spectacle synergy zoom-us ];
      development  = [ ghc niv ];
      utils        = [ aspell-with-dicts unzip ];
    in applications ++ development ++ utils;
in
{
  imports = [ ./emacs.nix ./firefox.nix ./xmonad.nix ];

  nixpkgs.config = {
    allowUnfree = true;

    packageOverrides = pkgs: {
      stable = import sources."nixpkgs-20.03" {};
      nur = import sources.NUR {
        inherit pkgs;
      };
    };
  };

  home = {
    inherit packages sessionVariables;

    username = "tikhon";
    homeDirectory = "/home/tikhon";

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

  programs = {
    home-manager.enable = true;

    bash = {
      enable = true;
      inherit sessionVariables;
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

      userName = "tikhon";
      userEmail = "tikhon@jelv.is";

      extraConfig = {
        ui.color = "always";
        github.user = "TikhonJelvis";
        core.fileMode = false;
      };
    };
  };

  services = {
    dropbox = {
      enable = true;
      path = "${config.home.homeDirectory}/Dropbox";
    };

    lorri.enable = true;
  };
}
