BIN      := @BIN@
LIB      := @LIB@
INCLUDE  := @INCLUDE@
SITELISP := @SITELISP@

LIB_UR   := $(LIB)/ur
LIB_C    := $(LIB)/c
LIB_JS   := $(LIB)/js

all-local: smlnj mlton

SUBDIRS = src/c

.PHONY: smlnj mlton package

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
	| sed 's/^\(.*\).grm$$/\1.mlton.grm.sig:\1.mlton.grm.sml/; y/:/\n/' \
	| sed 's/^\(.*\).lex$$/\1.mlton.lex.sml/' \
	>$@

%.mlton.lex: %.lex
	cp $< $@
%.mlton.grm: %.grm
	cp $< $@

%.mlton.lex.sml: %.mlton.lex
	mllex $<

%.mlton.grm.sig %.mlton.grm.sml: %.mlton.grm
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
	cp include/*.h $(INCLUDE)/
	mkdir -p $(SITELISP)
	cp src/elisp/*.el $(SITELISP)/
	ldconfig

package:
	hg archive -t tgz -X tests /tmp/urweb.tgz

