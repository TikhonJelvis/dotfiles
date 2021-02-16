{ pkgs, ... }:
{
  programs.ssh.enable = true;
  
  programs.ssh.matchBlocks.haskell = {
    host = "haskell";
    hostname = "www-combo-origin.haskell.org";
    user = "www";
  };
}
