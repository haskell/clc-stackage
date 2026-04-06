set -e

# Similar to dry_run.sh, except actually builds exactly one batch group.
# This way CI can spread the full build across multiple jobs, keeping the
# total time reasonable.
batch_index=$1

# -f not -x since downloaded exe may not have executable permissions.
if [[ -f ./bin/clc-stackage ]]; then
  echo "*** ./bin/clc-stackage exists, not re-installing ***"

  # May need to add permissions, if this exe was downloaded
  chmod a+x ./bin/clc-stackage
else
  echo "*** Updating cabal ***"
  cabal update

  echo "*** Installing clc-stackage ***"
  cabal install exe:clc-stackage --installdir=./bin --overwrite-policy=always
fi

if [[ -d output ]]; then
  rm -r output
fi

echo "*** Building with --batch-index $batch_index ***"

set +e

./bin/clc-stackage \
  --batch 200 \
  --batch-index $batch_index \
  --cabal-options="--semaphore" \
  --cleanup off

ec=$?

if [[ $ec != 0 ]]; then
  echo "*** clc-stackage failed ***"
else
  echo "*** clc-stackage succeeded ***"
fi

# Print out the logs + the packages we built, in case it is useful e.g.
# what did CI actually do.
if [[ -f generated/generated.cabal ]]; then
  echo "*** Printing generated cabal file ***"
  cat generated/generated.cabal
else
  echo "*** No generated/generated.cabal ***"
fi

if [[ -f generated/cabal.project.local ]]; then
  echo "*** Printing generated cabal.project.local file ***"
  cat generated/cabal.project.local
else
  echo "*** No generated/cabal.project.local ***"
fi

.github/scripts/print_logs.sh

exit $ec
