# Author: @googleson78
{
  description = "Build dependencies for clc-stackage";
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # Follow the nixpkgs from ghc.nix. Rationale:
    #
    # The glibc that is used to build the custom GHC should be the same as the
    # one here, otherwise we will encounter glibc version mismatch errors when
    # building packages that depend on glibc (even transitively).
    #
    # Because ghc.nix is a popular tool for building GHC with nix, it is
    # sensible to use the same glibc.
    #
    # Note this means that this input should generally be upgraded whenever
    # ghc.nix is.
    ghc_nix = {
      url = "git+https://gitlab.haskell.org/ghc/ghc.nix.git";
      inputs.flake-compat.follows = "flake-compat";
    };
    nixpkgs.follows = "ghc_nix/nixpkgs";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
      compiler = pkgs.haskell.packages.ghc9101;

      # There are some packages that do not build well with nix:
      #
      # - grisette: Depends on sbv
      #
      # - sbv: Builds with the nix-provided GHC in the .#with-ghc shell, but
      #        fails with a custom built GHC (e.g. building via ghc.nix).
      #        Possibly due to the combination of Nix + TH + C.
      #
      # - simple-pango: Strangely this appears to work with a custom GHC but
      #                 not with the one provided by nixpkgs.

      # see NOTE: [LD_LIBRARY_PATH]
      ldDeps = with pkgs; [
        blas # blas-ffi
        bzip2 # bnb-staking-csvs
        expat # cairo-image
        lapack # lapack-ffi
        libGL # GLUT, etc.
        pcre # regex-pcre
        xorg.libX11 # GLFW-b
        xorg.libXcursor # GLFW-b
        xorg.libXi # GLFW-b
        xorg.libXinerama # GLFW-b
        xorg.libXrandr # GLFW-b
        xorg.libXxf86vm # GLFW-b
        zlib # too many to list
      ];

      # The comments indicate haskell packages that require the given
      # dependency. This is not exhaustive.
      deps = with pkgs; [
        cabal-install
        curl # curl
        fribidi # simple-pango
        libdatrie # simple-pango
        libGLU # GLURaw
        libselinux # simple-pango
        libsepol # simple-pango
        libsodium # libsodium-bindings
        libthai # simple-pango
        libxml2 # c14n
        nettle # nettle
        nlopt # srtree
        openal # OpenAL
        pango # simple-pango
        pcre2 # simple-cairo
        pkg-config
        postgresql_16 # postgresql-libpq
        systemdMinimal # hidapi requires udev
        util-linux # simple-pango requires mount
        xorg.libXdmcp # simple-cairo
        xz # lzma
      ] ++ ldDeps;
    in
    {
      devShells.x86_64-linux = {
        default = pkgs.mkShell {
          buildInputs = deps;

          # NOTE: [LD_LIBRARY_PATH]
          #
          # Deps that also need to be added to LD_LIBRARY_PATH. For instance,
          # if we do not add zlib here, we will receive an error like:
          #
          #     libz.so: cannot open shared object file: No such file or directory
          #
          # Note this is a different error from not including the zlib library
          # at all (i.e. forgetting pkgs.zlib).
          #
          # https://discourse.nixos.org/t/shared-libraries-error-with-cabal-repl-in-nix-shell/8921/9
          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath ldDeps}:$LD_LIBRARY_PATH";

          # The stack library requires GHC to be on the PATH at build time due
          # to TH.
          shellHook = ''
            #export PATH=/path/to/custom/ghc/stage1/bin/:$PATH
            ${throw "Remove this line from the flake.nix and add your GHC to the PATH"}
          '';
        };

        dev = pkgs.mkShell {
          buildInputs = [
            compiler.ghc
            compiler.haskell-language-server
            compiler.ormolu
            pkgs.cabal-install
            pkgs.zlib
          ];
        };

        # This shell is useless wrt the intended purpose of this repo,
        # as the whole point is building stackage w/ a custom GHC. It exists
        # primarily to test that there is nothing wrong with the package set
        # itself for nix users, as building some of the libs with a custom
        # GHC is a challenge on NixOS.
        with-ghc = pkgs.mkShell {
          buildInputs = deps ++ [ compiler.ghc ];
        };
      };
    };
}
