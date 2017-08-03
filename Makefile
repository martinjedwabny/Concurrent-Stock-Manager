
NAME = FuSe
VERSION = 0.7
BASE = $(NAME)-$(VERSION)

NULL =

DIRS = src decoder stock-manager-src docs

ARCHIVE = \
  Makefile \
  $(wildcard src/*.ml) \
  $(wildcard src/*.mli) \
  src/Makefile \
  $(wildcard decoder/*.ml) \
  $(wildcard decoder/*.mli) \
  $(wildcard decoder/*.mly) \
  $(wildcard decoder/*.mll) \
  decoder/Makefile \
  $(wildcard stock-manager-src/*.ml) \
  stock-manager-src/Makefile \
  docs/Makefile \
  docs/FuSe.pdf \
  LICENSE \
  LICENSE.ml \
  README.md \
  CONTRIBUTORS \
  $(NULL)

SYNC_HOME_FILES = \
  FuSe.css \
  $(NULL)

SYNC_FILES = \
  $(SYNC_HOME_FILES) \
  $(BASE).tar.gz \
  docs/FuSe.pdf \
  $(NULL)

all:
	for i in $(DIRS); do make -C $$i; done

dist: $(BASE).tar.gz

$(BASE).tar.gz: $(ARCHIVE)
	rm -rf $(BASE)
	mkdir $(BASE)
	cp --parents $(ARCHIVE) $(BASE)
	tar cvfz $@ $(BASE)
	rm -rf $(BASE)

docs/FuSe.pdf:
	make -C docs FuSe.pdf
	
clean:
	for i in $(DIRS); do make -C $$i clean; done
	rm -rf $(BASE) $(BASE).tar.gz
