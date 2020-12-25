{ config, pkgs, ...}:
let
  sessionVariables = {
    EDITOR = "emacsclient --create-frame --alternate-editor emacs";
    PS1    = "λ x → \W>";
  };

  aspell-with-dicts = pkgs.aspellWithDicts (d: [d.en d.ru]);

  packages = with pkgs;
    let
      development  = [ python3 ghc niv ];
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

  home = {
    inherit packages sessionVariables;
 
    file = {
      "." = {
        source = ../files;
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
