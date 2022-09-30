# Config for desktops but *not* laptops
{ config, pkgs, ... }:
{
  imports = [ ./. ];
  
  users.mutableUsers = false;
  users.users.tikhon.passwordFile = "/home/tikhon/pass";

  # All my desktops currently use Nvidia GPUs, so let's use the
  # propietary Nvidia drivers:
  services.xserver.videoDrivers = [ "nvidia" ];
  
  # Wacom tablet
  environment.systemPackages = with pkgs; [ wacomtablet ];
  services.xserver.wacom.enable = true;

  # software monitor control (DDC)
  services.ddccontrol.enable = true;

  # TODO: are the monitor fingerprints the same for the new PC as for
  # the old one?
  services.autorandr = {
    enable = true;
    profiles = {
      "home" = {
        fingerprint = {
          DP-2 = "00ffffffffffff000472900467c68064301a0104a53c227806ee91a3544c99260f505421080001010101010101010101010101010101565e00a0a0a029503020350056502100001a000000ff002341534f6861716f39462f3364000000fd001ea522f040010a202020202020000000fc00584232373148550a202020202001f3020312412309070183010000654b040001015a8700a0a0a03b503020350056502100001a5aa000a0a0a046503020350056502100001a6fc200a0a0a055503020350056502100001a22e50050a0a0675008203a0056502100001e1c2500a0a0a011503020350056502100001a42f80050a0a0135008203a0056502100001e00a5";
          HDMI-0 = "00ffffffffffff0004721c072d3e61921a1d0103803c22782a3ad5ae4e43aa260b5054bfef80714f8140818081c081009500b300d1c04dd000a0f0703e803020350055502100001ab46600a0f0701f800820180455502100001a000000fd00283c1ea03c000a202020202020000000fc0058423237334b2047500a202020015b020350f154010304121305141f100706025d5e5f606120212223090707830100006d030c001000383c20006001020367d85dc401788003681a00000101283ce6e305e301e40f008001e606070161561c023a801871382d40582c450055502100001e8c0ad08a20e02d10103e96005550210000180000000000000000000000a9";
        };

        config = {
          DP-2 = {
            enable = true;
            mode = "2560x1440";
            rate = "59.95";
            primary = true;
          };
          HDMI-0 = {
            enable = true;
            mode = "3840x2160";
            position = "2560x0";
          };
        };
      };
    };
  };

  services.printing.enable = true;
  hardware.printers = let
    name = "Brother_HL-L3270CDW_series";
  in {
    ensurePrinters = [{
      inherit name;
      deviceUri = "dnssd://Brother%20HL-L3270CDW%20series._ipp._tcp.local/?uuid=e3248000-80ce-11db-8000-b42200424c0f";
      model = "everywhere";
      location = "1505 downstairs office";
      description = "Color laser printer in my downstairs office";
    }];
    ensureDefaultPrinter = name;
  };
  services.avahi.nssmdns = true; # Needed for CUPS to find the printer.

  # TODO: try nix-serve-ng instead?
  # Expose a Nix cache to the local network
  services.nix-serve = {
    enable = true;
    secretKeyFile = "/var/cache-priv-key.pem";
    port = 8080;
  };
}
