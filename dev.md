# Development

## Introduction

This project is organized into several libraries and a single executable. Roughly, the idea is:

1. Download a snapshot `s` from stackage.org.
2. Prune `s` based on packages we know we do not want (e.g. system deps).
3. Generate a custom `generated.cabal` file for the given package set, and try to build it.

Futhermore, we allow for building subsets of the entire stackage package set with the `--batch` feature. This will split the package set into disjoint groups, and build each group sequentially. The process can be interrupted at any time (e.g. `CTRL-C`), and progress will be saved in a "cache" (json file), so we can pick up where we left off.

## Components

### utils

`utils` is a library containing common utilities e.g. logging and hardcoded file paths.

### parser

`parser` contains the parsing functionality. In particular, `parser` is responsible for querying stackage's REST endpoint and retrieving the package set. That package set is then filtered according to [excluded_pkgs.json](excluded_pkgs.json). The primary function is:

```haskell
-- CLC.Stackage.Parser
getPackageList :: IO [PackageResponse]
```

If you want to get the list of the packages to be built (i.e. stackage_snapshot - excluded_packages), load the parser into the repl with `cabal repl parser`, and run the following:

```haskell
-- CLC.Stackage.Parser
-- printPackageList :: Bool -> Maybe Os -> IO ()
λ. printPackageList True Nothing
```

This will write the package list used for each OS to `pkgs_<os>.txt`.

### builder

`builder` is responsible for building a given package set. The primary functions are:

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

`runner` orchestrates everything. The primary function is:

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

## Updating to a new shapshot

1. Update to the desired snapshot:

    ```haskell
    -- CLC.Stackage.Parser.API
    stackageSnapshot :: String
    stackageSnapshot = "nightly-yyyy-mm-dd"
    ```

2. Update the `index-state` in [cabal.project](cabal.project) and [generated/cabal.project](generated/cabal.project).

3. Modify [excluded_pkgs.json](excluded_pkgs.json) as needed. That is, updating the snapshot will probably bring in some new packages that we do not want. The update process is essentially trial-and-error i.e. run `clc-stackage` as normal, and later add any failing packages that should be excluded.

4. Update references to the current ghc e.g.

    1. `ghc-version` in [.github/workflows/ci.yaml](.github/workflows/ci.yaml).
    2. [README.md](README.md).

5. Optional: Update `clc-stackage.cabal`'s dependencies (i.e. `cabal outdated`).

6. Optional: Update nix inputs (`nix flake update`).

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
