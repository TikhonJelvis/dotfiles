{ pkgs, ... }:
{
  imports = [ ./default.nix ];

  emacs = pkgs.emacs;

  services.emacs = {
    enable = true;

    # temporarily disabled; see:
    # https://github.com/nix-community/emacs-overlay/issues/58
    # client.enable = true;
  };

  # External packages only needed for my Emacs setup:
  home.packages = with pkgs;
    [ python-language-server ];
}
