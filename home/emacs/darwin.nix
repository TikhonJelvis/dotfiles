{ pkgs, config, ... }:
{
  imports = [ ./default.nix ];

  emacs = pkgs.emacsGcc.overrideAttrs (old: {
    name = "emacs-darwin";
    version = "28.0.50";

    postInstall = let
      run-emacs = ''
        #!/usr/bin/env bash
        set -e
        exec $out/bin/emacs-28.0.50 "\$@"
      '';
    in old.postInstall or "" + ''
      ln -snf $out/lib/emacs/28.0.50/native-lisp $out/native-lisp
      ln -snf $out/lib/emacs/28.0.50/native-lisp $out/Applications/Emacs.app/Contents/native-lisp

      cat <<EOF> $out/bin/run-emacs.sh
      ${run-emacs}
      EOF
      chmod +x $out/bin/run-emacs.sh
      ln -snf $out/bin/run-emacs.sh $out/bin/emacs
    '';
  });
}
