{ pkgs }:

let
  # NUR master on 2020-08-22
  nur-tar = builtins.fetchTarball {
    url = "https://github.com/nix-community/NUR/archive/de833d136f33f5ceaf237fa4287aa0f041ea7434.tar.gz";
    sha256 = "141j1kzmdqhskyr46r8p9fk00k4fv5cyi962h4whplnz1jglsk2w";
  };
in
{
  allowUnfree = true;

  packageOverrides = pkgs: {
    nur = import nur-tar {
      inherit pkgs;
    };
  };
}