# `clc-stackage`

[![ci](https://github.com/haskell/clc-stackage/actions/workflows/ci.yaml/badge.svg)](https://github.com/haskell/clc-stackage/actions/workflows/ci.yaml)

## How to?

This is a meta-package to facilitate impact assessment for [CLC proposals](https://github.com/haskell/core-libraries-committee).

An impact assessment is due when

1. Proposal makes a breaking change according to [PVP](https://pvp.haskell.org/).
2. Proposal exports a new entity from `Prelude` or other modules, described in [Haskell Report](https://www.haskell.org/onlinereport/haskell2010/haskellpa2.html#x20-192000II).
3. On discretion of CLC members.

The procedure is as follows:

1. Rebase changes, mandated by your proposal, atop the ghc branch (or tag) that corresponds to the current [stackage nightly](https://www.stackage.org/nightly). For example, if the latest snapshot ghc is `ghc-9.12.3`, we would want to rebase our changes on the `ghc-9.12.3-release` tag.

2. Compile a patched GHC, say, `~/ghc/_build/stage1/bin/ghc`.

3. `git clone https://github.com/haskell/clc-stackage`, then `cd clc-stackage`.

4. Build the exe: `cabal install exe:clc-stackage`.

    > :warning: **Warning:** Use a normal downloaded GHC for this step, **not** your custom built one. Why? Using the custom GHC can force a build of many dependencies you'd otherwise get for free e.g. `vector`.

5. Uncomment and modify the `with-compiler` line in [generated/cabal.project](generated/cabal.project) e.g.

    ```
    with-compiler: /home/ghc/_build/stage1/bin/ghc
    ```

6. Add your custom GHC to the PATH e.g.

    ```
    export PATH=/home/ghc/_build/stage1/bin/:$PATH
    ```

    Nix users can uncomment (and modify) this line in the `flake.nix`.

7. Run `clc-stackage` and wait for a long time. See [below](#the-clc-stackage-exe) for more details.

    * On a recent Macbook Air it takes around 12 hours, YMMV.
    * You can interrupt `cabal` at any time and rerun again later.
    * Consider setting `--jobs` to retain free CPU cores for other tasks.
    * Full build requires roughly 7 Gb of free disk space.
    * If the build fails with an error about max amount of arguments in `gcc`, run again, but with smaller batch size. 250 worked well for me.

    To get an idea of the current progress, we can run the following commands
    on the log file:

    ```sh
    # prints completed / total packages in this group
    $ grep -Eo 'Completed|^ -' output/logs/current-build/stdout.log | sort -r | uniq -c | awk '{print $1}'
    110
    182

    # combine with watch
    $ watch -n 10 "grep -Eo 'Completed|^ -' output/logs/current-build/stdout.log | sort -r | uniq -c | awk '{print \$1}'"
    ```

8. If any packages fail to compile:

    * copy them locally using `cabal unpack`,
    * patch to confirm with your proposal,
    * link them from `packages` section of `cabal.project`,
    * return to Step 6.

9. When everything finally builds, get back to CLC with a list of packages affected and patches required.

### Troubleshooting

Because we build with `nightly` and are at the mercy of cabal's constraint solver, it is possible to run into solver / build issues that have nothing to do with our custom GHC. Some of the most common problems include:

- Nightly adds a new, problematic package `p` e.g.

  - `p` requires a new system dependency (e.g. a C library).
  - `p` is an executable.
  - `p` depends on an excluded package in [./package_index.jsonc](package_index.jsonc).

- A cabal flag is set in a way that breaks the build. For example, our snapshot requires that the `bson` library does *not* have its `_old-network` flag set, as this will cause a build error with our version of `network`. This flag is automatic, so we have to force it in `generated/cabal.project` with `constraints: bson -_old-network`.

- Nightly has many packages drop out for some reason, increasing the chance for solver non-determinism.

We attempt to mitigate such issues by:

- Writing most of the snapshot's exact package versions as cabal constraints to the generated `./generated/cabal.project.local`, which ensures we (transitively) build the same package version every time. Note that boot packages like `text` are deliberately excluded so that we can build a snapshot with multiple GHCs. Otherwise even a GHC minor version difference would fail because `ghc` is in the build plan.

- Ignoring bounds in `generated/cabal.project`:

    ```
    allow-newer: *:*
    allow-older: *:*
    ```

Nevertheless, it is still possible for issues to slip through. When a package `p` fails to build for some reason, we should first:

- Verify that `p` is not in `package_index.excluded`. If it is, nightly probably pulled in some new reverse-dependency `q` that should be added to `package_index.excluded`.

- Verify that `p` does not have cabal flags that can affect dependencies / API.

- Verify that `p`'s version matches what it is in the current snapshot (e.g. `https://www.stackage.org/nightly`). If it does not, either a package needs to be excluded or constraints need to be added.

In general, user mitigations for solver / build problems include:

- Adding `p` to `package_index.excluded`. Note that `p` will still be built if it is a (transitive) dependency of some other package in the snapshot, but will not have its exact bounds written to `cabal.project.local`.

- Manually downloading a snapshot (e.g. `https://www.stackage.org/nightly/cabal.config`), changing / removing the offending package(s), and supplying the file with the `--snapshot-path` param. Like `package_index.jsonc`, take care that the problematic package is not a (transitive) dependency of something in the snapshot.

- Adding constraints to `generated/cabal.project` e.g. flags or version constraints like `constraints: filepath > 1.5`.

#### Misc

- Note that while a GHC minor version difference is usually okay, a GHC *major* difference will very likely lead to errors.

- The `flake.nix` line:

    ```nix
    compiler = pkgs.haskell.packages.ghc<vers>;
    ```

    can be a useful guide as to which GHC was last tested, as CI uses this ghc to build everything.

- If you encounter an error that you think indicates a problem with the configuration here (e.g. new package needs to be excluded, new constraint added), please open an issue. While that is being resolved, the mitigations from the [previous section](#troubleshooting) may be useful.

### The clc-stackage exe

`clc-stackage` is an executable that will:

1. Download the stackage snapshot from the stackage server.
2. Divide the snapshot into groups (determined by `--batch` argument).
3. For each group, generate a cabal file and attempt to build it.

#### Querying stackage

By default, `clc-stackage` queries https://www.stackage.org/ for snapshot information. In situations where this is not desirable (e.g. the server is not working, or we want to test a custom snapshot), the snapshot can be overridden:

```sh
$ clc-stackage --snapshot-path=path/to/snapshot
```

This snapshot should be formatted similar to the `cabal.config` endpoint on the stackage server (e.g. https://www.stackage.org/nightly/cabal.config). That is, package lines should be formatted `<pkgs> ==<vers>`:

```
abstract-deque ==0.3
abstract-deque-tests ==0.3
abstract-par ==0.3.3
AC-Angle ==1.0
acc ==0.2.0.3
...
```

The stackage config itself is valid, so trailing commas and other extraneous lines are allowed (and ignored).

#### Investigating failures

By default (`--write-logs save-failures`), the build logs are saved to the `./output/logs/` directory, with `./output/logs/current-build/` streaming the current build logs.

#### Group batching

By default, the `clc-stackage` exe tries to build all packages at once i.e. every package is written to `generated/generated.cabal`. This can cause problems e.g. we do not have enough memory to build everything simultaneously, or we receive an error that `gcc` has been given too many arguments. Hence we provide the `--batch N` option, which will split the package set into disjoint groups of size `N`. Each group is then built sequentially.

The default behavior is:

  1. `clc-stackage` will try to build everything in the same group, even if some package fails (equivalent to cabal's `--keep-going` flag.). If instead `--package-fail-fast` is enabled, the first failure will cause the entire group to immediately fail, and we will move onto the next group.

  2. `clc-stackage` will try every group, even if some prior group fails. The `--group-fail-fast` option changes this so that the first failure will cause `clc-stackage` to exit.

Each time a group finishes (success or failure), stdout/err will be updated, and then the next group will start. If the group failed to build and we have `--write-logs save-failures` (the default), then the logs and error output will be in `./output/logs/<pkg>/`, where `<pkg>` is the name of the first package in the group.

When `clc-stackage` itself finishes (either on its own or via an interrupt like `CTRL-C`), the results are saved to a cache which records all successes, failures, and untested packages. This allows us to pick up where we left off with untested packages (including failures if the `--retry-failures` flag is active).

> [!IMPORTANT]
>
> The cache operates at the *batch group* level, so only packages that have been part of a successful group will be considered successes. Conversely, a package will be considered a failure if it is part of a failing group, even if it was built successfully. Therefore, to see what packages actually failed, we will want to check the logs directory. Alternatively, we can first run `clc-stackage` initially with a large `--batch` group (for maximum performance), then run it again with, say, `--batch 1`.

See `clc-stackage --help` for more info.

##### Optimal performance

On the one hand, splitting the entire package set into `--batch` groups makes the output easier to understand and offers a nice workflow for interrupting/restarting the build. On the other hand, there is a question of what the best value of `N` is for `--batch N`, with respect to performance.

In general, the smaller `N` is, the worse the performance. There are several reasons for this:

- The smaller `N` is, the more `cabal build` processes, which adds overhead.
- More packages increase the chances for concurrency gains.

Thus for optimal performance, you want to take the largest group possible, with the upper limit being no `--batch` argument at all, as that puts all packages into the same group.

> [!TIP]
>
> Additionally, the `./output/cache.json` file can be manipulated directly. For example, if you want to try building only `foo`, ensure `foo` is the only entry in the json file's `untested` field.

## Getting dependencies via `nix`

For Linux based systems, there's a provided `flake.nix` and `shell.nix` to get a nix shell
with an approximation of the required dependencies (cabal itself, C libs) to build `clc-stackage`.

Note that it is not actively maintained, so it may require some tweaking to get working, and conversely, it may have some redundant dependencies.
