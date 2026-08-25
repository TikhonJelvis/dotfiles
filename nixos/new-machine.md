# Setting up a new machine

We can set up a new machine by `nixos-generators` to make a NixOS ISO that bakes in a configuration file for that machine. (`nixos-generators` is installed in `nixos/base/default.nix`)

## Generating ISO file

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

## On the new machine

Boot into the live USB on the new machine.

### SSH

To help debugging, the generated .iso enables SSH and enables the public key of the machine that generated the .iso

As long as the generation process worked, you should be able to SSH into the Live USB machine if it's connected to a local network (ie via ethernet):

  1. Run `ip addr` on the new machine to get its IP
  2. `ssh root@<ip-addr>` from the old machine

### Installing

  1. Log in. The nixos user should login with an empty password.
  2. Connect to WiFi if needed
  3. Partition the new system by running `sudo gparted` (see "partitioning" section for notes)
    1. Create a new partition table of the `gpt` type
    2. Create boot partition
    3. Create other partitions
    4. Apply
    5. Right click "manage flags" on boot partition, check `esp` (which should also check `boot` automatically)
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

### Partitioning

The way you want to partition the system depends on the context (how many drives you have, how you want to use them, whether you are going to be dual-booting...)

For the simplest case we just need two partitions:

  - FAT32 `boot` EFI system partition (512 MiB)
  - ext4 for everything else
  
There is no need for a swap partition any more, but you can consider setting `zramSwap.enable = true` in your configuration.nix.
