{ pkgs ? import <nixpkgs> {} }:
(import ./. {}).overrideAttrs (old:
  {
    buildInputs = [ pkgs.cabal-install ];
    returnShellEnv = true;
  }
)
