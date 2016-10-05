#!/usr/bin/env bash

# Set variables for using the proxy on a Target network.
function proxyon {
    export http_proxy=http://proxy-mdha.target.com:8080
    export HTTP_PROXY=http://proxy-mdha.target.com:8080
    export https_proxy=http://proxy-mdha.target.com:8080
    export HTTPS_PROXY=http://proxy-mdha.target.com:8080
}

# Unset variables for use on a non-Target network.
function proxyoff {
    unset http_proxy
    unset HTTP_PROXY
    unset https_proxy
    unset HTTPS_PROXY
}

function kill_alwayson {
    launchctl unload /Library/LaunchAgents/net.juniper.pulsetray.plist
}

function start_alwayson {
    launchctl load /Library/LaunchAgents/net.juniper.pulsetray.plist
}

# load Target's SSL certificates
source ~/.target_certs.sh

# Custom scripts and stuff:
export PATH=$PATH:~/local/bin/

if [ -e /Users/z0028sn/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/z0028sn/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
