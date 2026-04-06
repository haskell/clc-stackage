set -e

if [[ -f ./bin/clc-stackage ]]; then
  echo "*** ./bin/clc-stackage exists, not re-installing ***"
  chmod a+x ./bin/clc-stackage
else
  echo "*** Updating cabal ***"
  cabal update

  # --overwrite-policy=always and deleting output/ are unnecessary for CI since
  # this script will only be run one time, but it's helpful when we are
  # testing the script locally.

  echo "*** Installing clc-stackage ***"
  cabal install exe:clc-stackage --installdir=./bin --overwrite-policy=always
fi

if [[ -d output ]]; then
  rm -r output
fi

echo "*** Building all with --dry-run ***"

set +e
./bin/clc-stackage --batch 200 --cabal-options="--dry-run"

ec=$?

if [[ $ec != 0 ]]; then
  echo "*** clc-stackage failed ***"
  .github/scripts/print_logs.sh
  exit 1
else
  echo "*** clc-stackage succeeded ***"
fi
