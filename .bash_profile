# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/bin:$HOME/local/bin:$HOME/local/racket/bin

export PATH

# OPAM configuration
. /home/tikhon/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
