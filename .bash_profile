#!/usr/bin/env bash

if [ -e /home/tikhon/.nix-profile/etc/profile.d/nix.sh ]; then . /home/tikhon/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export PATH=/Applications/Emacs.app/Contents/MacOS/bin:$PATH
export PATH=$PATH:~/local/bin/

