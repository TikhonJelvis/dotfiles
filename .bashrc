# Sample .bashrc for SuSE Linux
# Copyright (c) SuSE GmbH Nuernberg

# There are 3 different types of shells in bash: the login shell, normal shell
# and interactive shell. Login shells read ~/.profile and interactive shells
# read ~/.bashrc; in our setup, /etc/profile sources ~/.bashrc - thus all
# settings made here will also take effect in a login shell.
#
# NOTE: It is recommended to make language settings in ~/.profile rather than
# here, since multilingual X sessions would not work properly if LANG is over-
# ridden in every subshell.

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)
export EDITOR=emacsclient

# For some news readers it makes sense to specify the NEWSSERVER variable here
#export NEWSSERVER=your.news.server

test -s ~/.alias && . ~/.alias || true

alias enode="env NODE_NO_READLINE=1 node"

export TEXINPUTS=.:$HOME/Documents/LaTeX:..:

export PATH=$HOME/local:$PATH
export PATH=$HOME/local/bin:$PATH
export PATH=$HOME/local/spring:$PATH
export PATH=$HOME/local/node/bin:$PATH
export PATH=$HOME/local/doctorjs/bin:$PATH
export PATH=$HOME/local/racket/bin:$PATH
export PATH=$HOME/local/z3/bin:$PATH
export PATH=$HOME/local/s3cmd:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$HOME/node_modules/.bin:$PATH
export PATH=$HOME/.opam/system/bin:$PATH

export TPL_PATH=$HOME/Documents/programming/haskell/TPL/src

# export PATH=$HOME/Documents/work/ve/penv/bin:$PATH
alias empy='~/Documents/work/EatMetrics/penv/bin/python'
export VE_PATH=$HOME/Documents/work/EatMetrics/penv

export PS1='λ x → \W>'

export GOROOT=$HOME/local/go
export PATH=$PATH:$GOROOT/bin

eval $(cat ~/.fehbg)
