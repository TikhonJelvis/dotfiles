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

  emacs = pkgs.emacs30-gtk3;

  services.emacs = {
    enable = true;
    client.enable = true;

    # 2024-12-18: emacsclient stopped being able to open GUI
    # windows. Some searching identified the likely cause: the Emacs
    # server gets launched before X and does not have access to
    # DISPLAY environment variables[1].
    #
    # Looking through the home-manager emacs service definition, it
    # seems like this should force the emacs server service to start
    # *after* X.
    #
    # [1]: https://unix.stackexchange.com/questions/728058/systemd-starts-emacs-daemon-without-xauthority-environment-variable-after-upgrad
    startWithUserSession = "graphical";
  };

  home.file = extra-packages;
  home.packages = with pkgs; [ imagemagick xclip ];
}
