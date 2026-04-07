# Development

## Introduction

This project is organized into several libraries and a single executable. Roughly, the idea is:

1. Download a snapshot `s` from stackage.org.
2. Prune `s` based on packages we know we do not want (e.g. system deps).
3. Generate a custom `generated.cabal` file for the given package set, and try to build it.

Furthermore, we allow for building subsets of the entire stackage package set with the `--batch` feature. This will split the package set into disjoint groups, and build each group sequentially. The process can be interrupted at any time (e.g. `CTRL-C`), and progress will be saved in a "cache" (json file), so we can pick up where we left off.

## Components

The `clc-stackage` library is namespaced by functionality:

### utils

`CLC.Stackage.Utils` contains common utilities e.g. logging and hardcoded file paths.

### parser

`CLC.Stackage.Parser` contains the parsing functionality. In particular, `parser` is responsible for querying stackage's REST endpoint and retrieving the package set. That package set is then filtered according to [package_index.jsonc](package_index.jsonc). The primary function is:

```haskell
-- CLC.Stackage.Parser
getPackageList :: Logging.Handle -> Maybe OsPath -> IO [Package]
```

If you want to get the list of the packages to be built (i.e. stackage_snapshot - excluded_packages), run `clc-stackage --print-package-set`.

This will write the package list used for each OS to `pkgs_<os>.txt`.

### builder

`CLC.Stackage.Builder` is responsible for building a given package set. The primary functions are:

```haskell
-- CLC.Stackage.Builder
writeCabalProjectLocal :: NonEmpty Package -> IO ()

batchPackages :: BuildEnv -> NonEmpty (PackageGroup, Int)

buildProject :: BuildEnv -> Int -> PackageGroup -> IO ()
```

That is:

1. `writeCabalProjectLocal` will be called **once** at start-up, and write the entire package set to a `cabal.project.local`'s `constraints` section. This ensures the same transitive deps are used every time.

2. `batchPackages` splits the entire package set into separate groups, based on the `--batch` value. If `--batch` is not given, then we will have a single group containing every package.

3. `buildProject` will write the given package group to the generated cabal file, and attempt to build it.

### runner

`CLC.Stackage.Runner` orchestrates everything. The primary function is:

```haskell
run :: Logging.Handle -> IO ()
```

Which takes in a logging handler (we use the handler pattern for testing), and then:

1. Sets up the environment based on CLI args and previous cache data.
2. Runs the parser and builder until it either finishes or is interrupted.
3. Saves the current progress to the cache.
4. Prints out a summary.

The reason this logic is a library function and not the executable itself is for testing.

### clc-stackage

The executable that actually runs. This is a very thin wrapper over `runner`, which merely sets up the logging handler.

## Updating to a new snapshot

`clc-stackage` is based on `nightly` -- which changes automatically -- meaning we do not necessarily have to do anything when a new (minor) snapshot is released. On the other hand, *major* snapshot updates will almost certainly bring in new packages that need to be excluded, so there are some general "update steps" we will want to take:

1. Modify [package_index.jsonc](package_index.jsonc) as needed. That is, updating the snapshot major version will probably bring in some new packages that we do not want. The update process is essentially trial-and-error i.e. run `clc-stackage` as normal, and later add any failing packages to `package_index.excluded` that should be excluded.

2. Update `ghc-version` in [.github/workflows/ci.yaml](.github/workflows/ci.yaml).

3. Optional: Update nix:

    - Inputs (`nix flake update`).
    - GHC: Update the `compiler = pkgs.haskell.packages.ghc<vers>;` line.
    - Add to the `flake.nix`'s `ldDeps` and `deps` as needed to have the `nix` CI job pass. System libs available on nix can be found here: https://search.nixos.org/packages?channel=unstable.

    This job builds everything with `--dry-run`, so its success is a useful proxy for `clc-stackage`'s health. In other words, if the nix job fails, there is almost certainly a general issue (i.e. either a package should be excluded or new system dep is required), but if it succeeds, the package set is in pretty good shape (there may still be sporadic issues e.g. a package does not properly declare its system dependencies at config time).

4. Optional: Update `clc-stackage.cabal`'s dependencies (i.e. `cabal outdated`).

### Verifying snapshot

To verify the snapshot, every package should actually be built i.e. run `clc-stackage` as you normally would. However, this can be quite time-consuming when there are new solver errors that need to be resolved (e.g. new system deps need to be added). An easier method is to first get everything passing with `dry-run` -- e.g. `clc-stackage --cabal-options="--dry-run"` -- then once that is passing, run `clc-stackage` for real.

## Testing

There are two test suites, `unit` and `functional`. The latter actually runs all of the logic, though it uses the generalized runner:

```haskell
runModifyPackages :: Logging.Handle -> ([Package] -> [Package]) -> IO ()
```

This allows us to limit the number of packages to something reasonable to build on CI.

If the functional tests fail, it can be difficult to see what the actual error is, given that the error message is in the logs, which will be deleted by default. To keep the logs and generated files, run with:

```sh
$ NO_CLEANUP=1 cabal test functional
```

Note that this only saves files from the _last_ test, so if you want to examine test output for a particular test, you need to run only that test.

> [!TIP]
>
> CI has a job `build-batch` which actually builds the entire package set, hence it can be used in place of manual building / testing. Note it takes about an hour to run.
