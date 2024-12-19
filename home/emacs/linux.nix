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

  emacs = pkgs.emacs;

  services.emacs = {
    enable = true;
    client.enable = true;
  };

  home.file = extra-packages;
  home.packages = with pkgs; [ imagemagick xclip ];
}
