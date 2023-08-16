{ pkgs, lib, ... }:
let
  obs-studio = pkgs.wrapOBS {
    plugins = with pkgs.obs-studio-plugins; [
      (obs-ndi.override {
        ndi = pkgs.ndi.overrideAttrs (attrs: rec {
          src = pkgs.fetchurl {
            name = "${attrs.pname}-${attrs.version}.tar.gz";
            url = "https://go.ndi.video/e/428312/nstall-NDI-SDK-v5-Linux-tar-gz/79rns7/1238777648?h=R1GqlmeJwHONmT1otjh-L-ykzTvd_VKP4as3DhcuDRI";
            hash = "sha256-flxUaT1q7mtvHW1J9I1O/9coGr0hbZ/2Ab4tVa8S9/U=";
          };

          unpackPhase = ''
            unpackFile ${src}
            echo y | ./${attrs.installerName}.sh
            sourceRoot="NDI SDK for Linux"
          '';

          # TODO: go back to standard installPhase?
          #
          # 2023-08-15
          #
          # needed to remove 'mv logos ...' line because logos
          # directory was missing in downloaded NDI install archive
          installPhase = ''
            mkdir $out
            mv bin/x86_64-linux-gnu $out/bin
            for i in $out/bin/*; do
              patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$i"
            done
            patchelf --set-rpath "${pkgs.avahi}/lib:${pkgs.stdenv.cc.libc}/lib" $out/bin/ndi-record
            mv lib/x86_64-linux-gnu $out/lib
            for i in $out/lib/*; do
              if [ -L "$i" ]; then continue; fi
              patchelf --set-rpath "${pkgs.avahi}/lib:${pkgs.stdenv.cc.libc}/lib" "$i"
            done
            mv include examples $out/
            mkdir -p $out/share/doc/${attrs.pname}-${attrs.version}
            mv licenses $out/share/doc/${attrs.pname}-${attrs.version}/licenses
            mv documentation/* $out/share/doc/${attrs.pname}-${attrs.version}/
          '';
        });
      })
    ];
  };
in
{
  home.packages = [ obs-studio ];
}
