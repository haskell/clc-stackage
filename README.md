# `clc-stackage`

## How to?

This is a meta-package to facilitate impact assessment for [CLC proposals](https://github.com/haskell/core-libraries-committee). The package `clc-stackage.cabal` lists almost entire Stackage as `build-depends`, so that `cabal build` transitively compiles them all.

An impact assessment is due when

1. Proposal makes a breaking change according to [PVP](https://pvp.haskell.org/).
2. Proposal exports a new entity from `Prelude` or other modules, described in [Haskell Report](https://www.haskell.org/onlinereport/haskell2010/haskellpa2.html#x20-192000II).
3. On discretion of CLC members.

The procedure is as follows:

1. Rebase changes, mandated by your proposal, atop of `ghc-9.8` branch.
2. Compile a patched GHC, say, `~/ghc/_build/stage1/bin/ghc`.
3. `git clone https://github.com/Bodigrim/clc-stackage`, then `cd clc-stackage`.
4. Run `cabal build -w ~/ghc/_build/stage1/bin/ghc --keep-going` and wait for a long time.
  * On a recent Macbook Air it takes around 12 hours, YMMV.
  * You can interrupt `cabal` at any time and rerun again later.
  * Consider setting `--jobs` to retain free CPU cores for other tasks.
  * Full build requires roughly 7 Gb of free disk space.
5. If any packages fail to compile:
  * copy them locally using `cabal unpack`,
  * patch to confirm with your proposal,
  * link them from `packages` section of `cabal.project`,
  * return to Step 4.
6. When everything finally builds, get back to CLC with a list of packages affected and patches required.

## Getting dependencies via `nix`
For Linux based systems, there's a provided `flake.nix` and `shell.nix` to get a nix shell
with an approximation of the required dependencies (cabal itself, C libs) to build `clc-stackage`.

Note that it is not actively maintained, so it may require some tweaking to get working, and conversely, it may have some redundant dependencies.

## Misc

* Your custom GHC will need to be on the PATH to build the `stack` library i.e.

  ```
  export PATH=/path/to/custom/ghc/stage1/bin/:$PATH
  ```

  Nix users can uncomment (and modify) this line in the `flake.nix`.