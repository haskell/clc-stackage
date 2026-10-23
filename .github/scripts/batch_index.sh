set -e

# Similar to dry_run.sh, except actually builds exactly one batch group.
# This way CI can spread the full build across multiple jobs, keeping the
# total time reasonable.
batch_index=$1

url=""
if [[ -n "$2" ]]; then
  url="--snapshot-url=$2"
fi

source .github/scripts/install.sh

if [[ -d output ]]; then
  rm -r output
fi

echo "*** Building with --batch-index $batch_index ***"

set +e

clc-stackage \
  --batch 200 \
  --batch-index $batch_index \
  --cabal-options="--semaphore" \
  --cleanup off \
  $url

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
