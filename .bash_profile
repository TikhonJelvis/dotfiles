#!/usr/bin/env bash

if [ -e /home/tikhon/.nix-profile/etc/profile.d/nix.sh ]; then . /home/tikhon/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)
export EDITOR=nix-aware-emacsclient

export PATH=$HOME/local:$PATH
export PATH=$HOME/local/bin:$PATH

export PS1='λ x → \W>'


