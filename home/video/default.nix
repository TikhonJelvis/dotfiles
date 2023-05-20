{ pkgs, lib, ... }:
let
  obs-studio = pkgs.wrapOBS {
    plugins = with pkgs.obs-studio-plugins; [
      (obs-ndi.override {
        ndi = pkgs.ndi.overrideAttrs (attrs: rec {
          src = pkgs.fetchurl {
            name = "${attrs.pname}-${attrs.version}.tar.gz";
            url = "https://go.ndi.video/e/428312/nstall-NDI-SDK-v5-Linux-tar-gz/79rns7/1238777648?h=R1GqlmeJwHONmT1otjh-L-ykzTvd_VKP4as3DhcuDRI";
            hash = "sha256-wW21e0Z9BszhRVOT33RPFSYjtIrlPXaqasSztfDjveg=";
          };

          unpackPhase = ''
            unpackFile ${src}
            echo y | ./${attrs.installerName}.sh
            sourceRoot="NDI SDK for Linux"
          '';
        });
      })
    ];
  };
in
{
  home.packages = [ obs-studio ];
}
