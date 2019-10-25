#!/usr/bin/env bash

case "$1" in
  @*)
    filename="${1:1}"
    sed -i '' 's@^--odir=.*@--odir='"$TRAVIS_BUILD_DIR"'/html/@;s@^--package-version=.*@--package-version=master@;s@^--title=streamly-0.6.1@--title=streamly-master@' $filename
    haddock $1 ;;
  *) haddock $1 ;;
esac
