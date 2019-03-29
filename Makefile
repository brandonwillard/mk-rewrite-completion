.PHONY: all clean

all:
	make -C src/org -f Makefile
	make -C src/python -f Makefile
	make -C src/R -f Makefile
	make -C src/tex -f Makefile

clean:
	for MAKEFILE_DIR in $$(egrep -l '^clean:' src/**/Makefile) ; do \
		make -C $$(dirname $${MAKEFILE_DIR}) -f Makefile clean; \
	done
