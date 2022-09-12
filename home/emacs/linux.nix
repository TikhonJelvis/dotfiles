{ pkgs, lib, config, ... }:
let
  prefix-package = name: value: {
    name = ".emacs.d/packages/${name}";
    value = {
      source = value;
    };
  };
  extra-packages = lib.mapAttrs' prefix-package
    (import ./packages.nix { inherit config; });
in {
  imports = [ ./default.nix ];

  emacs = pkgs.emacsPgtkNativeComp;

  services.emacs = {
    enable = true;

    # temporarily disabled; see:
    # https://github.com/nix-community/emacs-overlay/issues/58
    # client.enable = true;
  };

  home.file = extra-packages;
  home.packages = with pkgs; [ imagemagick xclip ];
}
