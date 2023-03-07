#!/usr/bin/env bash

GHC_VERSION=8.10.4
STREAMLY_VERSION=0.9.0

case `uname` in
  Darwin) SYSTEM=x86_64-osx;;
  Linux) SYSTEM=x86_64-linux;;
  *) echo "Unsupported system"; exit 1 ;;
esac

pushd $(git rev-parse --show-toplevel)
git ls-files \
  | grep '\.l\?hs$' \
  | xargs hscope -b \
    -I src/ \
    -I test \
    -I dist-newstyle/build/$SYSTEM/ghc-$GHC_VERSION/streamly-$STREAMLY_VERSION/build/src/ \
    -X BangPatterns \
    -X CApiFFI \
    -X CPP \
    -X ConstraintKinds \
    -X DeriveDataTypeable \
    -X DeriveGeneric \
    -X DeriveTraversable \
    -X ExistentialQuantification \
    -X FlexibleContexts \
    -X FlexibleInstances \
    -X GeneralizedNewtypeDeriving \
    -X InstanceSigs \
    -X KindSignatures \
    -X LambdaCase \
    -X MagicHash \
    -X MultiParamTypeClasses \
    -X PatternSynonyms \
    -X RankNTypes \
    -X RecordWildCards \
    -X ScopedTypeVariables \
    -X TupleSections \
    -X TypeFamilies \
    -X ViewPatterns \
    -X NoMonoLocalBinds \
    -X TemplateHaskell \
    -X UnboxedTuples \
    -X UndecidableInstances
popd
