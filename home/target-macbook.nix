{ ... }:
{
  imports = [
    ./base
    ./emacs/darwin.nix
  ];

  home = {
    username = "z0028sn";
    homeDirectory = "/Users/z0028sn";

    sessionVariables = let
      tgt-ca-bundle = "$HOME/local/etc/ssl/certs/tgt-ca-bundle.crt"
    in {
      NIX_SSL_CERT_FILE  = tgt-ca-bundle;
      SSL_CERT_FILE      = tgt-ca-bundle;
      REQUESTS_CA_BUNDLE = tgt-ca-bundle;
      TGT_CA_BUNDLE_PATH = tgt-ca-bundle;
      AWS_CA_BUNDLE      = tgt-ca-bundle;

      HOME_MANAGER_CONFIG = toString ./target-macbook.nix;
    };
  };

  programs.git = {
    ignores = [ ".DS_Store" ];

    userEmail = "tikhon.jelvis@target.com";
  };
}
