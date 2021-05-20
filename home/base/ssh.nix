{ pkgs, ... }:
{
  programs.ssh.enable = true;
  
  programs.ssh.matchBlocks.haskell = {
    host = "haskell";
    hostname = "www-combo-nix.haskell.org";
    user = "www";
  };
}
