# `clc-stackage`

## Getting dependencies via `nix`
For Linux based systems, there's a provided `flake.nix` and `shell.nix` to get a nix shell
with an approximation of the required dependencies (cabal itself, C libs) to build `clc-stackage`.

Note that it is not actively maintained, so it may require some tweaking to get working, and conversely, it may have some redundant dependencies.
