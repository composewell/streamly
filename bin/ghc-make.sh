#!/bin/bash

curdir=`pwd`
SRC_ROOT=${curdir}/core
BLD_ROOT=${curdir}/build
RSP_FILE=${BLD_ROOT}/ghc-streamly.rsp
CONFIG_FILE=${BLD_ROOT}/src/config.h

mkdir -p ${BLD_ROOT}
CABAL_MACROS=${BLD_ROOT}/cabal_macros.h

if test ! -f "$CABAL_MACROS"
then
  echo "$CABAL_MACROS: not found"
  echo "Use cabal to generate cabal_macros.h and place it in build dir."
  exit 1
fi

# configure creates config.h in a directory reflecting the relative path of
# config.h.in in the source tree. We keep that src path in the include path and
# then all source include paths are translated to include paths relative to the
# build dir. So that all such paths get included.
#

# -I${BLD_ROOT}/src/doctest
# -I${BLD_ROOT}/src/Streamly/Internal/Data
# -I${BLD_ROOT}/src/Streamly/Internal/Data/Array
# -I${BLD_ROOT}/src/Streamly/Internal/Data/Stream

if test ! -f "$CONFIG_FILE"
then
  cd ${BLD_ROOT}
  ${SRC_ROOT}/configure
  cd $curdir
fi

RTS_INCLUDE_DIRS=$(ghc-pkg field rts include-dirs --simple-output)
RTS_INCLUDE_FLAGS=""
for dir in $RTS_INCLUDE_DIRS; do
    RTS_INCLUDE_FLAGS="$RTS_INCLUDE_FLAGS -I$dir"
done

CPP_FLAGS="\
-I${SRC_ROOT}/src \
-I${SRC_ROOT}/src/doctest \
-I${SRC_ROOT}/src/Streamly/Internal/Data \
-I${SRC_ROOT}/src/Streamly/Internal/Data/Array \
-I${SRC_ROOT}/src/Streamly/Internal/Data/Stream \
-I${BLD_ROOT}/src \
$RTS_INCLUDE_FLAGS"

# Use cabal build with -v option. Copy paste compiler flags from the
# response file arguments shown in the output.

if test ! -f "$RSP_FILE"
then
cat << EOF > ${RSP_FILE}
--make
-static -dynamic-too -dynosuf dyn_o -dynhisuf dyn_hi
'-package-env=-'
-this-unit-id streamly-core-0.4.0-temp
-outputdir ${BLD_ROOT} -odir ${BLD_ROOT} -hidir ${BLD_ROOT} -hiedir ${BLD_ROOT} -stubdir ${BLD_ROOT}
-i -i${SRC_ROOT}/src -i${BLD_ROOT}/src
-optP-include -optP${CABAL_MACROS}
$CPP_FLAGS
-XHaskell2010
-XBangPatterns
-XConstraintKinds
-XDeriveDataTypeable
-XDeriveGeneric
-XDeriveTraversable
-XExistentialQuantification
-XFlexibleContexts
-XFlexibleInstances
-XGeneralizedNewtypeDeriving
-XInstanceSigs
-XKindSignatures
-XLambdaCase
-XMultiParamTypeClasses
-XRankNTypes
-XScopedTypeVariables
-XStandaloneDeriving
-XTupleSections
-XTypeApplications
-XTypeOperators
-XCApiFFI
-XCPP
-XDefaultSignatures
-XMagicHash
-XRecordWildCards
-XStandaloneKindSignatures
-XQuantifiedConstraints
-Weverything
-Wno-implicit-prelude
-Wno-missing-deriving-strategies
-Wno-missing-exported-signatures
-Wno-missing-import-lists
-Wno-missing-local-signatures
-Wno-missing-safe-haskell-mode
-Wno-missed-specialisations
-Wno-all-missed-specialisations
-Wno-monomorphism-restriction
-Wno-prepositive-qualified-module
-Wno-unsafe
-Wno-missing-kind-signatures
-Wno-redundant-bang-patterns
-Wno-operator-whitespace
-Wno-missing-role-annotations
-Wno-missing-poly-kind-signatures
-O2
-fdicts-strict
'-fspec-constr-recursive=16'
'-fmax-worker-args=16'
-Rghc-timing
EOF
fi

HSC2HS_FLAGS="$CPP_FLAGS -D__GLASGOW_HASKELL__=910 -i${CABAL_MACROS}"

HSC_FILES=$(find "$SRC_ROOT" -name '*.hsc' -printf '%P\n')
for f in $HSC_FILES; do
    src="$SRC_ROOT/$f"
    dst="$BLD_ROOT/${f%.hsc}.hs"

    mkdir -p "$(dirname "$dst")"
    #echo "src=$src, dst=$dst"
    echo "hsc2hs $HSC2HS_FLAGS $src -o $dst"
    hsc2hs $HSC2HS_FLAGS "$src" -o "$dst"
done

#    -fplugin Fusion.Plugin           \
#    -ddump-to-file                   \
#    -ddump-simpl                     \

ghc @${RSP_FILE}                     \
    +RTS -M400M -RTS                 \
    $*
