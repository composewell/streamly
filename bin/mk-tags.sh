#!/bin/sh

HSCOPE_DIR=$HOME/.hscope
CODEX_CONFIG=$HOME/.codex

if test -e $CODEX_CONFIG
then
  echo "Please move or remove $CODEX_CONFIG and then run again"
  exit 1
fi

# XXX it does not seem to include the current project anyway
cat << EOF > $CODEX_CONFIG
currentProjectIncluded: true
hackagePath: $HSCOPE_DIR/packages/hackage.haskell.org/
tagsFileHeader: true
tagsFileName: tags
tagsFileSorted: true
tagsCmd: hasktags --ctags --follow-symlinks --output="\$TAGS" "\$SOURCES"
EOF

# XXX depends on cabal configure, does not seem to be supporting new cabal
# XXX it depends in the hackage index tar file in ~/.cabal
codex update --force
rm -f $CODEX_CONFIG
