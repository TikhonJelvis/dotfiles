{ ... }:
{
  imports = [
    ./base
    ./emacs/darwin.nix
  ];

  home = {
    username = "z0028sn";
    homeDirectory = "/Users/z0028sn";
  };

  programs.git = {
    ignores = [ ".DS_Store"];

    userEmail = "tikhon.jelvis@target.com";
  };

  programs.bash = {
    initExtra = ''
      if [ -e $HOME/local/etc/ssl/certs/tgt-ca-bundle.crt ]
      then
        export NIX_SSL_CERT_FILE=$HOME/local/etc/ssl/certs/tgt-ca-bundle.crt
        export SSL_CERT_FILE=$HOME/local/etc/ssl/certs/tgt-ca-bundle.crt
        export REQUESTS_CA_BUNDLE=$HOME/local/etc/ssl/certs/tgt-ca-bundle.crt
        export TGT_CA_BUNDLE_PATH=$HOME/local/etc/ssl/certs/tgt-ca-bundle.crt
        export AWS_CA_BUNDLE=$HOME/local/etc/ssl/certs/tgt-ca-bundle.crt
      fi
    '';
  };
}
