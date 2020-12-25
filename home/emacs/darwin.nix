{ pkgs, config, ... }:
let
  emacs-src = config.sources.emacs-nativecomp;
  emacs-darwin-base = pkgs.emacsGcc.override {
    srcRepo = true;
    nativeComp = true;
  };
in
{
  imports = [ ./default.nix ];

  emacs = emacs-darwin-base.overrideAttrs (old: {
    name = "emacs-darwin";
    version = "28.0.50";
    src = pkgs.fetchFromGitHub {
      inherit (emacs-src) owner repo rev sha256;
    };

    configureFlags = old.configureFlags ++ ["--with-ns"];

    postPatch = old.postPatch + ''
      substituteInPlace lisp/loadup.el \
      --replace '(emacs-repository-get-version)' '"${emacs-src.rev}"' \
      --replace '(emacs-repository-get-branch)' '"master"'
    '';

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
