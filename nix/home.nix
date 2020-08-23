{ config, pkgs, ... }:

let
  sources = import ./nix/sources.nix;
in
{
  imports = [ ./emacs.nix ];

  home.packages = with pkgs; [ firefox git unzip niv ];

  home.sessionVariables = {
    EDITOR = "emacsclient";
    PS1 = "λ x → \W>";
  };

  nixpkgs.config = {
    allowUnfree = true;

    packageOverrides = pkgs: {
      stable = import sources."nixpkgs-20.03" {};
      nur = import sources.NUR {
        inherit pkgs;
      };
    };
  };

  programs.bash = {
    enable = true;
  };

  programs.firefox = import ./firefox.nix { inherit pkgs; };

  programs.git = {
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

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "tikhon";
  home.homeDirectory = "/home/tikhon";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";
}
