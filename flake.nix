# Author: @googleson78
{
  description = "Build dependencies for clc-stackage";
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nixpkgs-old.url = "github:nixos/nixpkgs/17b62c338f2a0862a58bb6951556beecd98ccda9";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs-old, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs-old = import nixpkgs-old {
        inherit system;
      };
      pkgs = import nixpkgs {
        inherit system;
      };
    in with pkgs-old;
    {
      devShell.x86_64-linux = pkgs-old.mkShell {
        buildInputs = [
          pkgs.cabal-install
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
