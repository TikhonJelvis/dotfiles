{ pkgs, config, ... }:
{
  imports = [ ./default.nix ];

  emacs = pkgs.emacs-pgtk.override {withNativeCompilation = false;};
}
