#
# Export org files.
#
# Inspired by https://github.com/leanprover/presentations/blob/master/org-reveal.mk#L9
#
.PHONY: all clean install-cask %.pdf %.tex
.PRECIOUS: ${TEX_DIR}/%.tex

CASK_DIR ?= ${HOME}/.cask
CASK_BIN ?= ${CASK_DIR}/bin/cask
EMACS_BIN ?= emacs

SRC_DIR = $(realpath $(dir $<))
DEST_DIR = $(abspath ../../output)
FIG_DIR = $(abspath ../../figures)
TEX_DIR = $(abspath ../tex)

ORG_FILES = $(wildcard *.org)
TEX_FILES = $(addsuffix .tex, ${TEX_DIR}/$(basename ${ORG_FILES}))

all: $(patsubst %.org, ${DEST_DIR}/%.pdf, ${ORG_FILES})

install-cask:
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python

${TEX_DIR}/%.tex: %.org .cask org-export-init.el
	@if [ ! -f ${CASK_BIN} ]; then echo "Cask not found; do 'make install-cask' first."; exit 1; fi
	CASK_DIR=${CASK_DIR} $(EMACS_BIN) --quick --batch -l org-export-init.el --visit $< -f org-latex-export-to-latex
	mv -f $(@F) $@

${DEST_DIR}/%.pdf: ${TEX_DIR}/%.tex
	make -C ${TEX_DIR} -f Makefile $@

.cask: Cask
	@EMACS=$(EMACS_BIN) $(CASK_BIN)
	@touch .cask

clean:
	-make -C ${TEX_DIR} -f Makefile clean

# Just some convenience targets...
%.pdf: ${DEST_DIR}/%.pdf ;

%.tex: ${TEX_DIR}/%.tex ;
