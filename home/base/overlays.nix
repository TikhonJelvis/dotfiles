let
  r-overlay = self: super:
    let
      snapshot = "2020-09-12";
      cran = { snapshot, name, version, sha256 }:
        self.fetchurl {
          inherit sha256;
          url = "https://cran.microsoft.com/snapshot/${snapshot}/src/contrib/${name}_${version}.tar.gz";
        };
    in
      {
        rPackages = super.rPackages.override {
          overrides = {
            arrow = super.rPackages.arrow.overrideDerivation (old: rec {
              name = "arrow-${version}";
              version = "1.0.1";

              src = cran {
                inherit snapshot version;
                name = "arrow";
                sha256 = "1z6nln5nb77p9k0mdsjd88mmlw27wcfqbq881kmklhm9jbya74pv";
              };

              buildInputs = old.buildInputs ++ [ self.pkgconfig ];
              nativeBuildInputs = old.nativeBuildInputs ++ [ self.arrow-cpp self.rPackages.cpp11 ];
              propagatedNativeBuildInputs = old.propagatedNativeBuildInputs ++ [ self.rPackages.cpp11 ];

              ARROW_USE_PKG_CONFIG = "TRUE";
              LIBARROW_BINARY = "${self.arrow-cpp}/lib";
              LIBARROW_BUILD = "FALSE";
              LIBARROW_DOWNLOAD = "FALSE";
            });

            cpp11 = self.rPackages.buildRPackage rec {
              name = "cpp11-${version}";
              version = "0.2.1";
              src = cran {
                inherit snapshot version;
                name = "cpp11";
                sha256 = "1113y61lj4cg1d2yjavdx9zih5rzb4pnxmj5v3sr4bhzlxz2scda";
              };
            };
          };
        };
      };
in
[ r-overlay ]
