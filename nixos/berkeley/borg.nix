{ config, pkgs, ... }:
# For interacting with borg directly, you can use the borg-shell
# command to open a shell with the right environments set up.
#
# sudo borg-shell
# borg list
# ...etc
let
  borgbase = config.services.borgbackup.jobs.borgbase;
  borg-shell = pkgs.writeShellScriptBin "borg-shell" ''
    #!/usr/bin/env bash
    export BORG_REPO="${borgbase.repo}"
    export BORG_PASSCOMMAND="${borgbase.encryption.passCommand}"
    export BORG_RSH="${borgbase.environment.BORG_RSH}"
    bash
  '';
in
{
  services.borgbackup.jobs = {
    "borgbase" = {
      paths = [
        "/home"
      ];
      exclude = [
        # XDG cache directory
        "/home/*/.cache"

        # Bigger app data directories (> 250M)
        "/home/*/.local/share/Trash"
        "/home/*/.local/share/baloo"
        "/home/*/.local/share/Steam"

        # Build outputs
        "**/target"        # Rust, Scala... etc
        "/home/*/.cargo"   # Global Rust cache
        "**/dist"          # Python, Haskell... etc
        "**/dist-newstyle" # Haskell
        "**/_site"         # Hakyll
        "**/_cache"        # Hakyll

        # Big projects
        "**/Programming/nixpkgs"
      ];
      repo = "i2344ym0@i2344ym0.repo.borgbase.com:repo";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /root/borg-passphrase";
        # Passphrase also saved in 1Password as
        # tikhon-nixos-berkeley-passphrase under borgbase
      };
      environment.BORG_RSH = "ssh -i /root/.ssh/borgbase";
      compression = "auto,zstd";
      startAt = "daily";
    };
  };

  environment.systemPackages = [ borg-shell ];
}
