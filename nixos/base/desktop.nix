# Config for desktops but *not* laptops
{ config, pkgs, ... }:
{
  imports = [ ./. ];
  
  users.mutableUsers = false;
  users.users.tikhon.hashedPasswordFile = "/home/tikhon/pass";

  # let's see if *not* using the prorpietary NVidia drivers solves
  # some of the monitor config problems I'm seeing
  # services.xserver.videoDrivers = [ "nvidia" ];

  # set to "" to get an error with all available session names
  services.displayManager.defaultSession = "home-manager";

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
          DP-2 = "00ffffffffffff0004721c072d3e61921a1d0104b53c22783b3ad5ae4e43aa260b50542348008140818081c081009500b300d1c001014dd000a0f0703e803020350055502100001ab46600a0f0701f800820180455502100001a000000fd0c3078ffff6b010a202020202020000000fc0058423237334b2047500a20202002d4020343f151010304121305141f9007025d5e5f60613f2309070783010000e200c06d030c0020003878200060010203681a00000101307800e305e301e606070161561c023a801871382d40582c450055502100001e011d007251d01e206e28550055502100001e6fc200a0a0a055503020350055502100001e000000000000f97012790000030128e0f600847f0759002f801f006f081900010003009aa00104ff0ea0002f8021006f083e00030005001200168a1010007f076f080000000000414352b106000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d990";
          DP-3 = "00ffffffffffff000472900467c68064301a0104a53c227806ee91a3544c99260f505421080001010101010101010101010101010101565e00a0a0a029503020350056502100001a000000ff002341534f6861716f39462f3364000000fd001ea522f040010a202020202020000000fc00584232373148550a202020202001f3020312412309070183010000654b040001015a8700a0a0a03b503020350056502100001a5aa000a0a0a046503020350056502100001a6fc200a0a0a055503020350056502100001a22e50050a0a0675008203a0056502100001e1c2500a0a0a011503020350056502100001a42f80050a0a0135008203a0056502100001e00a5";
        };

        config = {
          DP-3 = {
            enable = true;
            mode = "2560x1440";
            primary = true;
          };
          DP-2 = {
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
  services.avahi.nssmdns4 = true; # Needed for CUPS to find the printer.
}
