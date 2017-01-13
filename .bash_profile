#!/usr/bin/env bash

function proxyon {
  echo "Please enter Target LANID: "
  read -s LANID
  echo "Please enter Target Password: "
  read -s PASSWORD

  CLEAN_PASSWORD=$(echo $PASSWORD | python -c "import urllib, sys ; print urllib.quote_plus(sys.stdin.read().rstrip('\r\n'))";)

  export http_proxy=http://$LANID:$CLEAN_PASSWORD@proxy-mdha.target.com:8080
  export HTTP_PROXY=http://$LANID:$CLEAN_PASSWORD@proxy-mdha.target.com:8080
  export https_proxy=http://$LANID:$CLEAN_PASSWORD@proxy-mdha.target.com:8080
  export HTTPS_PROXY=http://$LANID:$CLEAN_PASSWORD@proxy-mdha.target.com:8080
  export SSL_CERT_FILE=/Users/"$(echo ${LANID} | tr '[A-Z]' '[a-z]')"/certs/tgt-ca-bundle.crt
}

function proxyoff {
  unset http_proxy
  unset HTTP_PROXY
  unset https_proxy
  unset HTTPS_PROXY
  unset SSL_CERT_FILE
}

function kill_alwayson {
    launchctl unload /Library/LaunchAgents/net.juniper.pulsetray.plist
}

function start_alwayson {
    launchctl load /Library/LaunchAgents/net.juniper.pulsetray.plist
}

# Custom scripts and stuff:
export PATH=$PATH:~/local/bin/

if [ -e /Users/z0028sn/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/z0028sn/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

