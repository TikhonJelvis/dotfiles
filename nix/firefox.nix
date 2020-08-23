{ pkgs, stdenv ? pkgs.stdenv }:
let
  addons = pkgs.nur.repos.rycee.firefox-addons;
  inherit (addons) buildFirefoxXpiAddon;

  # ‘1password-...’ is not a valid Nix variable name.
  one-password = addons."1password-x-password-manager";

  noscript = buildFirefoxXpiAddon {
    pname = "noscript";
    version = "11.0.39";
    addonId = "{73a6fe31-595d-460b-a920-fcc0f8843232}";
    url = "https://addons.mozilla.org/firefox/downloads/file/3629456/noscript_security_suite-11.0.39-an+fx.xpi?src=";
    sha256 = "0pkvgw7bi1isnf4bfb6jcgjwlnidxz8c4s48ryc29shlxqmy9bfh";
    meta = with stdenv.lib; {
      homepage = "https://noscript.net/";
      description = "Noscript.";
      license = licenses.gpl2;
      platforms = platforms.all;
    };
  };
in  
{
  enable = true;
  profiles = {
    tikhon = {
      settings = {
        "browser.ctrlTab.recentlyUsedOrder" = false;
      };
    };
  };
  extensions =
    with addons; [
      https-everywhere
      noscript
      one-password
      ublock-origin
    ];
}