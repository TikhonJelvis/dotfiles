{ pkgs, ... }:
{
  programs.ssh = {
    enable = true;

    # you can add default settings back in using
    #
    # matchBlocks."*" = { ... };
    enableDefaultConfig = false;
    
    settings.haskell = {
      HostName = "www-combo-nix.haskell.org";
      User = "www";
    };
  };
}
