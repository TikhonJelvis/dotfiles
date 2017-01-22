# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/bin:$HOME/local/bin:$HOME/local/racket/bin

export PATH

# OPAM configuration
. $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
if [ -e /home/tikhon/.nix-profile/etc/profile.d/nix.sh ]; then . /home/tikhon/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
