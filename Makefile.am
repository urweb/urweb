ACLOCAL_AMFLAGS = -I m4

BIN      := @BIN@
LIB      := @LIB@
INCLUDE  := @INCLUDE@
SITELISP := @SITELISP@

LIB_UR   := $(LIB)/ur
LIB_C    := $(LIB)/c
LIB_JS   := $(LIB)/js

all-local: smlnj mlton

SUBDIRS = src/c

.PHONY: smlnj mlton package reauto

smlnj: src/urweb.cm
mlton: bin/urweb

clean-local:
	rm -f src/*.mlton.grm.* src/*.mlton.lex.* \
		src/urweb.cm src/urweb.mlb
	rm -rf .cm src/.cm

src/urweb.cm: src/prefix.cm src/sources
	cat src/prefix.cm src/sources \
	>src/urweb.cm

src/urweb.mlb: src/prefix.mlb src/sources src/suffix.mlb
	cat src/prefix.mlb src/sources src/suffix.mlb \
	| sed -e 's/^\(.*\).grm$$/\1.mlton.grm.sig:\1.mlton.grm.sml/' -e 'y/:/\n/' \
		-e 's/^\(.*\).lex$$/\1.mlton.lex.sml/' \
	>$@

src/urweb.mlton.lex: src/urweb.lex
	cp $< $@
src/urweb.mlton.grm: src/urweb.grm
	cp $< $@

src/urweb.mlton.lex.sml: src/urweb.mlton.lex
	mllex $<

src/urweb.mlton.grm.sig src/urweb.mlton.grm.sml: src/urweb.mlton.grm
	mlyacc $<

MLTON := mlton

#ifdef DEBUG
#	MLTON += -const 'Exn.keepHistory true'
#endif

#ifdef PROFILE
#	MLTON += -profile $(PROFILE)
#endif

bin/urweb: src/compiler.mlb src/urweb.mlb src/*.sig src/*.sml \
		src/urweb.mlton.lex.sml \
		src/urweb.mlton.grm.sig src/urweb.mlton.grm.sml
	$(MLTON) -output $@ src/compiler.mlb

install-exec-local:
	mkdir -p $(BIN)
	cp bin/urweb $(BIN)/
	mkdir -p $(LIB_UR)
	cp lib/ur/*.urs $(LIB_UR)/
	cp lib/ur/*.ur $(LIB_UR)/
	mkdir -p $(LIB_JS)
	cp lib/js/*.js $(LIB_JS)/
	mkdir -p $(INCLUDE)
	cp config.h $(INCLUDE)/
	cp include/*.h $(INCLUDE)/
	mkdir -p $(SITELISP)
	cp src/elisp/*.el $(SITELISP)/
	-ldconfig

uninstall-local:
	rm -f $(BIN)/urweb \
		$(LIB_UR)/basis.urs $(LIB_UR)/char.urs $(LIB_UR)/listPair.urs $(LIB_UR)/list.urs \
		$(LIB_UR)/monad.urs $(LIB_UR)/option.urs $(LIB_UR)/string.urs $(LIB_UR)/top.urs \
		$(LIB_UR)/char.ur $(LIB_UR)/listPair.ur $(LIB_UR)/list.ur \
		$(LIB_UR)/monad.ur $(LIB_UR)/option.ur $(LIB_UR)/string.ur $(LIB_UR)/top.ur \
		$(LIB_JS)/urweb.js \
		$(INCLUDE)/config.h $(INCLUDE)/queue.h $(INCLUDE)/request.h $(INCLUDE)/types.h \
		$(INCLUDE)/urweb.h \
		$(SITELISP)/urweb-compat.el $(SITELISP)/urweb-mode.el $(SITELISP)/urweb-move.el \
		$(SITELISP)/urweb-defs.el $(SITELISP)/urweb-mode-startup.el $(SITELISP)/urweb-util.el

package:
	hg archive -t tgz -X tests /tmp/urweb.tgz

reauto:
	-autoreconf
	libtoolize -cf
	automake --add-missing --force-missing --copy
	autoreconf
