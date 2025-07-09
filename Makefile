# Makefile to generate .cabal files from .cabal.m4 templates
#
# Usage:
# make                     # Build both cabal files
# make diff                # See staged/unstaged changes
# make M4FLAGS=-DDEV_MODE  # Regenerate with development flags

# Optional m4 flags passed in (e.g., M4FLAGS="-DDEV_MODE")
M4FLAGS ?=

# Targets
CABAL_FILES := streamly.cabal core/streamly-core.cabal

all: $(CABAL_FILES)

%.cabal: %.cabal.m4
	m4 $(M4FLAGS) $< > $@

diff: all
	git diff -- $(CABAL_FILES)

.PHONY: all diff
