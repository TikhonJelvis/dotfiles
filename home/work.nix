{ lib, ... }:
{
  options.work.enable = lib.mkEnableOption "work-specific configuration" // {
    description = ''
      Whether this is a work machine.

      This enables work-specific settings and configurations in both my Nix setup and my Emacs setup.
    '';
  };
}
