if [[ ! -d output/logs ]]; then
  echo "*** No output ***"
else
  cd output/logs

  for dir in */; do
    echo "*** $dir stdout ***"
    cat "$dir/stdout.log"
    echo "*** $dir stderr ***"
    cat "$dir/stderr.log"
  done
fi
