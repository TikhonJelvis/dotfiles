{ config, pkgs, lib, ... }:
let
  home = builtins.getEnv "HOME";
  pub-key-path = home + "/.ssh/id_rsa.pub"; # change this if you have a different key
  pub-key =
    if builtins.pathExists pub-key-path
    then lib.strings.trim (builtins.readFile pub-key-path)
    else throw "No public key found at ${pub-key-path}; generate one or edit iso-config.nix";

  password-file = /home + "/${config.default-user.name}/pass";
in
{
  imports = [
    ./base/user.nix
    ./base/laptop.nix
  ];

  environment.systemPackages = with pkgs;
    [ git emacs gparted firefox xterm ];

  environment.etc."dotfiles".source = builtins.fetchGit { url = ../.; };
  environment.etc."bin/install-helper" = {
    source = ./bin/install-helper;
    mode = "0755";
  };
  environment.etc."nixos-configs/new-user".text = config.default-user.name;
  environment.etc."nixos-configs/password-hash" =
    lib.mkIf (builtins.pathExists password-file) {
      source = password-file;
    };

  # SSH
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [ pub-key ];
}
