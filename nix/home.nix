{ config, pkgs, stdenv, ... }:

let
  emacs = [ pkgs.emacs ];
  basics = with pkgs; [ firefox git unzip niv ];
in
{
  home.packages = emacs ++ basics;

  home.sessionVariables = {
    EDITOR = "emacsclient";
    PS1 = "λ x → \W>";
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
