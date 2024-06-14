{ pkgs, lib, epkgs }:
let
  commit = "da7b63d854d010d621e2c82a53d6ae2d94dd53b0";
in
epkgs.trivialBuild {
  pname = "lean4-mode";
  version = "1.0.0";  # TODO: does this need to be set differently?

  src = pkgs.fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    hash = "sha256-U6MJIcBZf1XrUnssipgEy0BhF4/zhKVWblUvNqKNQe0=";
    rev = commit;
  };

  buildInputs = (with epkgs; [
    dash
    flycheck
    lsp-mode
    magit-section
  ]);

  meta = {
    homepage = "https://github.com/leanprover-community/lean4-mode";
    description = "Emacs major-mode for working with the Lean 4 theorem prover.";
    maintainers = [];
    license = lib.licenses.asl20;
    inherit (pkgs.emacs.meta) platforms;
  };
}
