{ pkgs, config, ... }:
{
  imports = [ ./default.nix ];

  emacs = pkgs.emacs31-pgtk.override {withNativeCompilation = false;};
}
