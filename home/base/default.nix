{ config, pkgs, lib, ...}:
{
  imports = [ ./sources.nix ./ssh.nix ../utils ];

  nixpkgs = {
    config   = import ./config.nix;
    overlays = import ./overlays.nix;
  };

  xdg.configFile = {
    "nixpkgs/config.nix".source   = ./config.nix;
    "nixpkgs/overlays.nix".source = ./overlays.nix;
  };

  home = {
    packages = with pkgs;
      let
        utils =
          [ drive pandoc unzip zip _1password poppler_utils aspell ];
        development  =
          [ ghc lorri niv python3 poetry cachix stylish-haskell ];

        stylish-haskell =
          let project = (import config.sources.stylish-haskell {}).stylish-haskell;
          in project.components.exes.stylish-haskell;
        aspell = pkgs.aspellWithDicts (d: [d.en d.ru]);
      in utils ++ development;

    sessionPath = [
      "$HOME/.nix-profile/bin"
      "$HOME/local/bin"

      "/nix/var/nix/profiles/default/bin"
      "/run/wrappers/bin"
      "/run/current-system/sw/bin"
    ];

    sessionVariables = {
      EDITOR = "emacsclient --create-frame --alternate-editor emacs";
      PS1    = "λ x → \\W>";
      MANPATH = "$HOME/.nix-profile/share/man";

      NIX_PROFILES = "/nix/var/nix/profiles/default $HOME/.nix-profile";

      NIX_PATH = lib.concatStringsSep ":" [
        "nixpkgs=${config.sources.nixpkgs}"
        "nixos-config=/etc/nixos/configuration.nix"
        "nixpkgs/nixos=${config.sources.nixpkgs}"
      ];
    };

    file = let
      go = name: value: {
        source = ../files + "/${name}";
        recursive = true;
      };
      files = builtins.mapAttrs go (builtins.readDir ../files);
    in {
      ".aspell.conf" = {
        source = pkgs.writeText ".aspell.conf" ''
          data-dir ${config.home.homeDirectory}/.nix-profile/lib/aspell
          personal ${toString ../.aspell.en.pws} 
        '';
      };
    } // files;

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

      sessionVariables = config.home.sessionVariables;

      initExtra = ''
        unset __HM_SESS_VARS_SOURCED
        source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
      '';
    };

    fish.enable = false;

    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = false;

      nix-direnv.enable = true;

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
