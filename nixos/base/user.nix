# This configures some user-specific settings, with defaults that work
# for me (Tikhon).
#
# In case anybody who isn't me is trying to use this config, please
# override:
#
#  - name
#  - description
#  - github
{ config, pkgs, lib, ... }:
with lib;

let
  user-cfg = config.default-user;
in
{
  options.default-user = {
    name = mkOption {
      type = types.str;
      description = "Username for default non-root account.";
      default = "tikhon";
      example = "john";
    };

    github = mkOption {
      type = types.str;
      description = "Default GitHub username (used for dotfiles repo).";
      default = "TikhonJelvis";
      example = "JohnDoe";
    };

    description = mkOption {
      type = types.str;
      description = "Description of the user. Often the user's full name.";
      default = "Tikhon Jelvis";
      example = "John Doe";
    };

    extraGroups = mkOption {
      type = types.listOf types.str;
      description = "Extra groups for the default user.";
      example = ["wheel" "docker"];
      default = [
        "wheel" # enable ‘sudo’
        "docker"
        "scanner" # sane
        "lp" # sane
        "networkmanager"
      ];
    };
  };

  # Don't forget to set up a password file on each machine
  config.users.users.${user-cfg.name} = {
    isNormalUser = true;
    inherit (user-cfg) description extraGroups;
  };

  # Expose the GitHub username to scripts (ie nixos/bin/post-install).
  environment.etc."nixos-configs/github-user".text = cfg.github;
}
