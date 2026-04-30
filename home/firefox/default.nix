{ pkgs, lib, ... }:
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
    meta = with lib; {
      homepage = "https://noscript.net/";
      description = "Noscript.";
      license = licenses.gpl2;
      platforms = platforms.all;
    };
  };
in  
{
  # TODO: migrate Firefox config directory XDG config home?
  #
  # Quoting from home-manager warning:
  #
  # You are currently using the legacy default (`".mozilla/firefox"`) because `home.stateVersion` is less than "26.05".
  # To silence this warning and keep legacy behavior, set:
  #   programs.firefox.configPath = ".mozilla/firefox";
  # To adopt the new default behavior, set:
  #   programs.firefox.configPath = "${config.xdg.configHome}/mozilla/firefox";
  #
  # To migrate to the XDG path, move `~/.mozilla/firefox` to
  # `$XDG_CONFIG_HOME/mozilla/firefox` and remove the old directory.
  # Native messaging hosts are not moved by this option change.
  programs.firefox = {
    enable = true;
    configPath = ".mozilla/firefox";
    profiles = {
      tikhon = {
        settings = {
          "browser.ctrlTab.recentlyUsedOrder" = false;
        };
      };
    };
  };
}
