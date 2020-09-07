{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/9b1695a7-f046-4905-b068-2bb271e71366";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/8AC1-64B9";
      fsType = "vfat";
    };

  fileSystems."/win/C" =
    { device = "/dev/disk/by-uuid/30C2C2FBC2C2C3F6";
      fsType = "ntfs";
    };

  fileSystems."/win/D" =
    { device = "/dev/disk/by-uuid/0E7CE7377CE717EB";
      fsType = "ntfs";
    };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
