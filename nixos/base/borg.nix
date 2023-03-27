{ config, pkgs, ... }:
# To set up for a new machine:
#
#  1. import ./base/borg.nix
#  2. Set services.borgbackup.jobs.borgbase.repo
#  3. Generate repo passphrase in 1Password, save in /root/borg-passphrase
#  4. Create /root/.ssh/borgbase{.pub}
#  5. Upload key to borgbase.com + add to repo
#  6. ssh to repo locally, say <yes>
#
#
# Run systemd job manually:
#
# > systemctl start borgbackup-job-borgbase.service
#
# Look at recent logs from backup job:
#
# > journalctl -fu borgbackup-job-borgbase.service
#
# Open shell to run borg commands directly:
#
# > sudo borg-shell
# > borg list
# ...etc
#
# Mount backup to restore or test):
#
# > mkdir ~/tmp/backup
# > sudo borg-job-borgbase mount <repo> ~/tmp/backup
#
# When done, unmount:
#
# > sudo borg-job-borgbase unmount ~/tmp/backup
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
        "**/node_modules"  # node

        # Big projects
        "**/Programming/nixpkgs"
      ];
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /root/borg-passphrase";
        # Passphrase also saved in 1Password
      };
      environment.BORG_RSH = "ssh -i /root/.ssh/borgbase";
      compression = "auto,zstd";
      startAt = "daily";
    };
  };

  environment.systemPackages = [ borg-shell ];
}
