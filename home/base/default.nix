{ config, pkgs, lib, ...}:
{
  imports = [ ./sources.nix ];

  home = {
    packages = with pkgs;
      let
        development  = [ ghc lorri niv python3 ];
        aspell       = pkgs.aspellWithDicts (d: [d.en d.ru]);
      in development ++ [ aspell ];

    sessionVariables = {
      EDITOR = "emacsclient --create-frame --alternate-editor emacs";
      PS1    = "λ x → \\W>";

      PATH = lib.makeBinPath [
        "$HOME/.nix-profile"
        "$HOME/local"

        "/nix/var/nix/profiles/default"
        "/run/current-system/sw"
        "/run/wrappers"
      ];
      MANPATH = "$HOME/.nix-profile/share/man";

      NIX_PROFILES = "/nix/var/nix/profiles/default $HOME/.nix-profile";

      NIX_PATH = lib.concatStringsSep ":" [
        "nixpkgs=${config.sources.nixpkgs}"
        "home-manager=${config.sources.home-manager}"

        "nixos-config=/etc/nixos/configuration.nix"
        "nixpkgs/nixos=${config.sources.nixpkgs}"
      ];
    };

    file = {
      "." = {
        source = ../files;
        recursive = true;
      };
      ".aspell.conf" = {
        source = pkgs.writeText ".aspell.conf" ''
          data-dir ${config.home.homeDirectory}/.nix-profile/lib/aspell
          personal ${toString ../.aspell.en.pws} 
        '';
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

  nixpkgs.config = {
    allowUnfree = true;

    packageOverrides = pkgs: {
      stable = import config.sources.nixpkgs-darwin {};
      nur = import config.sources.NUR {
        inherit pkgs;
      };
    };
  };

  fonts.fontconfig.enable = true;

  news.display = "silent";

  programs = {
    bash = {
      enable = true;

      sessionVariables = config.home.sessionVariables;
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

      extraConfig = {
        ui.color = "always";
        github.user = "TikhonJelvis";
        core.fileMode = false;
      };
    };
  };
}
