# A list of Emacs files pulled in from Git/etc
#
# I'm installing these in .emacs.d/packages instead of trying to use
# the Nix Emacs infrastructure, which means they should *not* have
# :ensure t set in use-package.
{ config }:
{ "screenshot.el" = config.sources.screenshot-el + "/screenshot.el"; }



