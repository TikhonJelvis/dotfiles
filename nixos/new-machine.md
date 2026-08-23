# Setting up a new machine

We can set up a new machine by `nixos-generators` to make a NixOS ISO that bakes in a configuration file for that machine. (`nixos-generators` is installed in `nixos/base/default.nix`)

## Generating ISO File

  1. `cd nixos`
  2. Set up a new `<machine>.nix` file.
  3. Generate an ISO with `bin/generate-iso <machine>.nix`

## Writing to USB

  1. Identify USB drive with `lsblk`, look for disk (`/dev/sdf`) and not partition (`/dev/sdf1`)
  2. Unmount USB if needed (`sudo umount /dev/sdf1`)
  3. Write ISO with `dd`: 

    ``` bash
sudo dd if=result/iso/nixos-*.iso of=/dev/sdf bs=4M status=progress conv=fsync
    ```
  4. Eject: `sudo eject /dev/sdf`

## Installing

On the new machine:

  1. `startx` to start the X server
  2. Connect to WiFi from the NM applet
  3. Partition the new system:  `alt+d` to run `gparted`
  4. Mount the new partitions at `/mnt`:
  
    ``` bash
    mount /dev/sda1 /mnt
    mkdir -p /mnt/boot
    mount /dev/sda2 /mnt/boot
    ```
  5. Run `install-helper`: `/etc/bin/install-helper <machine-name>`
  6. Reboot into the new system
  7. Create SSH keys + upload to GitHub
  8. `cd ~/Programming/dotfiles` then run `nixos/bin/post-install`
  9. If everything works, review and push the dotfile changes to GitHub
