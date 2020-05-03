#!/usr/bin/env bash

if [ -e /home/tikhon/.nix-profile/etc/profile.d/nix.sh ]; then . /home/tikhon/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)
export EDITOR=nix-aware-emacsclient

export PATH=$HOME/local:$PATH
export PATH=$HOME/local/bin:$PATH

export PS1='λ x → \W>'

# Hask to make Haskell executables built by Nix happy.
#
# Nix on Linux has a glibc version with a weird way of setting
# locales, which forces GHC into ASCII mode; this setting fixes that
# and lets Haskell binaries deal with UTF-8 encoded text.
export LOCALE_ARCHIVE=$(nix-instantiate -E 'with import <nixpkgs> {}; pkgs.buildPackages.glibcLocales' 2> /dev/null)/lib/locale/locale-archive
export LANG="C.UTF-8";
