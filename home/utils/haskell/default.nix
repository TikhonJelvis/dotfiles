{ pkgs ? import <nixpkgs> {}, ... }:

pkgs.haskellPackages.developPackage {
  name = "utils";
  root = (pkgs.lib.cleanSourceWith {
    src = ./.;
    filter = path: type:
      let
        name = baseNameOf (toString path);
        ignored = ["dist" "dist-newstyle"];
      in
        builtins.all (ignored-file: name != ignored-file) ignored &&
        !pkgs.lib.hasPrefix ".ghc.environment" name &&
        pkgs.lib.cleanSourceFilter path type;
  }).outPath;

  returnShellEnv = false;
}
