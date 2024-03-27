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
      url = "github:alpmestan/ghc.nix";
      inputs.flake-compat.follows = "flake-compat";
    };
    nixpkgs.follows = "ghc_nix/nixpkgs";

    # We use a newer version of nixpkgs for cabal-install, to avoid
    # the warning:
    #
    #     Warning: Unknown/unsupported 'ghc' version detected (Cabal 3.10.1.0 supports
    #     'ghc' version < 9.8)
    #
    # nixpkgs_new has cabal-install 3.10.2.1, which removes this warning.
    #
    # When ghc.nix's nixpkgs has this newer cabal, we can get rid of
    # nixpkgs_new and use nixpkgs for everything.
    nixpkgs_new.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, nixpkgs_new, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
      pkgs_new = import nixpkgs_new { inherit system; };
      compiler = pkgs_new.haskell.packages.ghc982;

      # There are some packages that do not build well with nix:
      #
      # - grisette: Depends on sbv
      #
      # - hw-json-simd:
      #   - https://github.com/haskell-works/hw-json-simd/issues/90
      #   - https://github.com/haskell/core-libraries-committee/issues/158#issuecomment-1537226472
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
        fftw # emd
        lapack # lapack-ffi
        libGL # GLUT, etc.
        openssl # core-telemetry, cql-io
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
        alsa-lib # synthesizer-alsa
        pkgs_new.cabal-install
        clp # coinor-clp
        curl # curl
        file # magic requires libmagic
        fribidi # simple-pango
        glib # alsa-seq
        glpk # comfort-glpk
        icu # text-icu
        jack2 # jack
        libdatrie # simple-pango
        libGLU # GLURaw
        libselinux # simple-pango
        libsepol # simple-pango
        libsodium # ihaskell
        libthai # simple-pango
        libxml2 # c14n
        mpfr # hmpfr
        nettle # nettle
        openal # OpenAL
        pango # simple-pango
        pcre2 # simple-cairo
        pkg-config
        primecount # primecount
        systemdMinimal # hidapi requires udev
        util-linux # simple-pango requires mount
        xorg.libXdmcp # simple-cairo
        xz # lzma
        zeromq # zeromq4-haskell
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
