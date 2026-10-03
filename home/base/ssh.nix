{ pkgs, ... }:
{
  programs.ssh = {
    enable = false;

    # you can add default settings back in using
    #
    # matchBlocks."*" = { ... };
    enableDefaultConfig = false;

    # brev (https://brev.dev) writes host entries here itself; it can't
    # write directly into ~/.ssh/config since home-manager manages it.
    includes = [ "~/.brev/ssh_config" ];

    settings.haskell = {
      HostName = "www-combo-nix.haskell.org";
      User = "www";
    };
  };
}
