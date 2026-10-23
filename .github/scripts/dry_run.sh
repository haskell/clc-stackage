url=""
if [[ -n "$1" ]]; then
  url="--snapshot-url=$1"
fi

source .github/scripts/install.sh

if [[ -d output ]]; then
  rm -r output
fi

echo "*** Building all with --dry-run ***"

set +e
clc-stackage --batch 200 --cabal-options="--dry-run" $url

ec=$?

if [[ $ec != 0 ]]; then
  echo "*** clc-stackage failed ***"
  .github/scripts/print_logs.sh
  exit 1
else
  echo "*** clc-stackage succeeded ***"
fi
