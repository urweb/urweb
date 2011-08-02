ACLOCAL_AMFLAGS = -I m4

BIN      := @BIN@
LIB      := @LIB@
INCLUDE  := @INCLUDE@
SITELISP := @SITELISP@
VERSION  := @VERSION@

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

install-exec-emacs:
if USE_EMACS
	mkdir -p $(DESTDIR)$(SITELISP)
	cp src/elisp/*.el $(DESTDIR)$(SITELISP)/
endif

install-exec-local-main:
	mkdir -p $(DESTDIR)$(BIN)
	cp bin/urweb $(DESTDIR)$(BIN)/
	mkdir -p $(DESTDIR)$(LIB_UR)
	cp lib/ur/*.urs $(DESTDIR)$(LIB_UR)/
	cp lib/ur/*.ur $(DESTDIR)$(LIB_UR)/
	mkdir -p $(DESTDIR)$(LIB_JS)
	cp lib/js/*.js $(DESTDIR)$(LIB_JS)/
	mkdir -p $(DESTDIR)$(INCLUDE)
	cp config.h $(DESTDIR)$(INCLUDE)/
	cp include/*.h $(DESTDIR)$(INCLUDE)/

install-exec-local: install-exec-local-main install-exec-emacs
	-ldconfig

uninstall-emacs:
if USE_EMACS
	rm -f $(DESTDIR)$(SITELISP)/urweb-compat.el $(DESTDIR)$(SITELISP)/urweb-mode.el $(DESTDIR)$(SITELISP)/urweb-move.el \
		$(DESTDIR)$(SITELISP)/urweb-defs.el $(DESTDIR)$(SITELISP)/urweb-mode-startup.el $(DESTDIR)$(SITELISP)/urweb-util.el
endif

uninstall-local-main:
	rm -f $(DESTDIR)$(BIN)/urweb \
		$(DESTDIR)$(LIB_UR)/basis.urs $(DESTDIR)$(LIB_UR)/char.urs $(DESTDIR)$(LIB_UR)/listPair.urs $(DESTDIR)$(LIB_UR)/list.urs \
		$(DESTDIR)$(LIB_UR)/monad.urs $(DESTDIR)$(LIB_UR)/option.urs $(DESTDIR)$(LIB_UR)/string.urs $(DESTDIR)$(LIB_UR)/top.urs \
		$(DESTDIR)$(LIB_UR)/char.ur $(DESTDIR)$(LIB_UR)/listPair.ur $(DESTDIR)$(LIB_UR)/list.ur \
		$(DESTDIR)$(LIB_UR)/monad.ur $(DESTDIR)$(LIB_UR)/option.ur $(DESTDIR)$(LIB_UR)/string.ur $(DESTDIR)$(LIB_UR)/top.ur \
		$(DESTDIR)$(LIB_JS)/urweb.js \
		$(DESTDIR)$(INCLUDE)/config.h $(DESTDIR)$(INCLUDE)/queue.h $(DESTDIR)$(INCLUDE)/request.h $(DESTDIR)$(INCLUDE)/types.h \
		$(DESTDIR)$(INCLUDE)/urweb.h

uninstall-local: uninstall-local-main uninstall-emacs

package:
	hg archive -t tgz -X tests "/tmp/urweb-$(VERSION).tgz"

reauto:
	-autoreconf
	libtoolize -cf
	automake --add-missing --force-missing --copy
	autoreconf
