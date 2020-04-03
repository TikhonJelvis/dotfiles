source ~/.bash_profile

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)
export EDITOR=nix-aware-emacsclient

test -s ~/.alias && . ~/.alias || true

export TEXINPUTS=.:$HOME/Documents/LaTeX:..:

export PATH=$HOME/local:$PATH
export PATH=$HOME/local/bin:$PATH

export TPL_PATH=$HOME/Documents/programming/haskell/TPL/src

export PS1='λ x → \W>'

export GOROOT=$HOME/local/go
export PATH=$PATH:$GOROOT/bin

