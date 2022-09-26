{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/1b5a1d03-1577-4d6c-9941-7a1e9bc5b2b9";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/45BE-290F";
      fsType = "vfat";
    };

  fileSystems."/ntfs/hdd" =
    { device = "/dev/disk/by-uuid/A89683229682EFDC";
      fsType = "ntfs";
      options = [ "rw" "uid=1000" ];
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/2436dfbf-fbbf-4ae8-948c-1127b95806ef"; }
    ];

  nix.settings.max-jobs = lib.mkDefault 16;
  # High-DPI console
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
}
