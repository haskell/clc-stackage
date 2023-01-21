# Author: @googleson78
{
  description = "Build dependencies for clc-stackage";
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }:
    with nixpkgs.legacyPackages.x86_64-linux;
    {
      devShell.x86_64-linux = mkShell {
        buildInputs = [
          cabal-install
          libffi.dev
          libGL
          xorg.libX11
          zlib
          pkg-config
          xorg.libXcursor
          xorg.libXrandr
          xorg.libXxf86vm
          xorg.libXi
          bzip2
          curl
          libGLU
          xorg.libXau
          xorg.libXinerama
          blas
          pcre
          icu
          lzlib
          xz
          lapack
          expat
          file
          systemdMinimal
          elfutils
          xorg.libXext.dev
          pango
          glib
          libxml2
          numactl
          protobuf
          openal
          gmp
        ];
      };
    };
}
