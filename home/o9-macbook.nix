{ pkgs, ... }:
{
  imports = [
    ./base
    ./base/darwin.nix
    ./emacs/darwin.nix
  ];

  home = {
    username = "tikhon.jelvis";
    homeDirectory = "/Users/tikhon.jelvis";

    # o9Platform / Live Server development.
    packages = with pkgs; [
      # Both SDKs need to be on PATH simultaneously: LiveServer has net8.0 and
      # net10.0 projects, and LiveServer/Server/global.json requires a .NET 10
      # SDK even to build the net8 ones. Listing them separately would put only
      # whichever sorts first on PATH; combinePackages merges them properly.
      (dotnetCorePackages.combinePackages [ dotnet-sdk_8 dotnet-sdk_10 ])

      azure-cli   # az login; az acr login -n o9platform
      sqlcmd      # ad-hoc queries against o9.TenantModel on localhost,1437
      grpcurl     # ping Live Server's gRPC endpoint (GraphCube.Ping)
      _7zz        # the binary name LS expects for 7zip on non-Windows

      mongosh     # inspect o9LogDb, where LS logs land
      redis       # redis-cli against the LiveCache instance
      powershell  # o9.Platform/devscripts/*.ps1
    ];

    sessionVariables = {
      DOTNET_CLI_TELEMETRY_OPTOUT = "1";
      DOTNET_NOLOGO = "1";
    };
  };

  programs.git = {
    ignores = [ ".DS_Store" ];
    settings.user.email = "tikhon@jelv.is";
  };
}
