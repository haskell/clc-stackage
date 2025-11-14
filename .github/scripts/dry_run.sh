set -e

echo "*** Updating cabal ***"

cabal update

echo "*** Installing clc-stackage ***"

# --overwrite-policy=always and deleting output/ are unnecessary for CI since
# this script will only be run one time, but it's helpful when we are
# testing the script locally.
cabal install exe:clc-stackage --installdir=./bin --overwrite-policy=always

if [[ -d output ]]; then
  rm -r output
fi

echo "*** Building all with --dry-run ***"

set +e
./bin/clc-stackage --batch 100 --cabal-options="--dry-run"

ec=$?

if [[ $ec != 0 ]]; then
  echo "*** clc-stackage failed ***"
  .github/scripts/print_logs.sh
  exit 1
else
  echo "*** clc-stackage succeeded ***"
fi
