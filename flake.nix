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
        cabal-install
        clp
        fribidi
        glpk
        gtk2
        jack2
        libdatrie
        libffi.dev
        libGL
        libselinux
        libsepol
        libsodium
        libthai
        util-linux
        mpfr
        nettle
        pcre2
        zeromq
        xorg.libX11
        zlib
        pkg-config
        xorg.libXcursor
        xorg.libXdmcp
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
