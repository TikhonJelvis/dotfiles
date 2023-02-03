{ config, pkgs, ... }:

{
  imports =
    [ ./base/desktop.nix
      ./hardware-configuration/o11.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking = {
    hostName = "tikhon-nixos-o11";

    interfaces.enp39s0.ipv4.addresses = [
      { address = "192.168.0.42"; prefixLength = 24; }
    ];
    interfaces.enp39s0.ipv4.routes = [
      {
        address = "0.0.0.0";
        via = "192.168.0.1";
        options = { metric = "0"; };
        prefixLength = 0;
      }
    ];
  };

  # TODO: try nix-serve-ng instead?
  # Expose a Nix cache to the local network
  services.nix-serve = {
    enable = true;
    secretKeyFile = "/var/cache-priv-key.pem";
    port = 8080;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
