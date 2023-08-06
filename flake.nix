# Author: @googleson78
{
  description = "Build dependencies for clc-stackage";
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
      compiler = pkgs.haskell.packages.ghc962;

      deps = with pkgs; [
        alsa-lib
        blas
        bzip2
        cabal-install
        clp
        curl
        elfutils
        expat
        file
        fribidi
        glib
        glpk
        gmp
        gtk2
        icu
        jack2
        lapack
        libdatrie
        libffi.dev
        libGL
        libGLU
        libselinux
        libsepol
        libsodium
        libthai
        libxml2
        lzlib
        mpfr
        nettle
        numactl
        openal
        pango
        pcre
        pcre2
        pkg-config
        protobuf
        systemdMinimal
        util-linux
        xorg.libX11
        xorg.libXau
        xorg.libXcursor
        xorg.libXdmcp
        xorg.libXext.dev
        xorg.libXinerama
        xorg.libXrandr
        xorg.libXi
        xorg.libXxf86vm
        xz
        zeromq
        zlib
      ];
    in
    {
      devShells.x86_64-linux = {
        default = pkgs.mkShell {
          buildInputs = deps;
        };

        # This shell is useless wrt the intended purpose of this repo,
        # as the whole point is building stackage w/ a custom ghc. It exists
        # primarily to test that there is nothing wrong with the package set
        # itself for nix users, as building some of the libs with a custom
        # ghc is a challenge on NixOS.
        with-ghc = pkgs.mkShell {
          buildInputs = deps ++ [ compiler.ghc ];
        };
      };
    };
}
