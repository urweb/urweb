all: smlnj mlton

.PHONY: all smlnj mlton clean

smlnj: src/lacweb.cm
mlton: bin/lacweb

clean:
	rm -f src/*.mlton.grm.* src/*.mlton.lex.* \
		src/lacweb.cm src/lacweb.mlb
	rm -rf .cm src/.cm

src/lacweb.cm: src/prefix.cm src/sources
	cat src/prefix.cm src/sources \
	>src/lacweb.cm

src/lacweb.mlb: src/prefix.mlb src/sources src/suffix.mlb
	cat src/prefix.mlb src/sources src/suffix.mlb \
	| sed 's/^\(.*\).grm$$/\1.mlton.grm.sig\n\1.mlton.grm.sml/' \
	| sed 's/^\(.*\).lex$$/\1.mlton.lex.sml/' \
	>src/lacweb.mlb

%.mlton.lex: %.lex
	cp $< $@
%.mlton.grm: %.grm
	cp $< $@

%.mlton.lex.sml: %.mlton.lex
	mllex $<

%.mlton.grm.sig %.mlton.grm.sml: %.mlton.grm
	mlyacc $<

MLTON := mlton

ifdef DEBUG
	MLTON += -const 'Exn.keepHistory true'
endif

bin/lacweb: src/lacweb.mlb src/*.sig src/*.sml
	$(MLTON) -output $@ src/lacweb.mlb
