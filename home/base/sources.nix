{ config, lib, pkgs, ... } :
{
  options.sources = lib.mkOption {
    type = lib.types.attrs;
    default = import ../../nix/sources.nix;
    description = "Niv-based sources for the whole config.";
  };
}
