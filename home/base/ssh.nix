{ pkgs, ... }:
{
  programs.ssh = {
    enable = true;

    # you can add default settings back in using
    #
    # matchBlocks."*" = { ... };
    enableDefaultConfig = false;
    
    matchBlocks.haskell = {
      host = "haskell";
      hostname = "www-combo-nix.haskell.org";
      user = "www";
    };
  };
}
