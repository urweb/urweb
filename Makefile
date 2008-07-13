all: smlnj mlton c

.PHONY: all smlnj mlton c clean

smlnj: src/lacweb.cm
mlton: bin/lacweb
c: clib/lacweb.o clib/driver.o

clean:
	rm -f src/*.mlton.grm.* src/*.mlton.lex.* \
		src/lacweb.cm src/lacweb.mlb \
		clib/*.o
	rm -rf .cm src/.cm

clib/lacweb.o: src/c/lacweb.c
	gcc -O3 -I include -c src/c/lacweb.c -o clib/lacweb.o

clib/driver.o: src/c/driver.c
	gcc -O3 -I include -c src/c/driver.c -o clib/driver.o

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

bin/lacweb: src/lacweb.mlb src/*.sig src/*.sml \
		src/lacweb.mlton.lex.sml \
		src/lacweb.mlton.grm.sig src/lacweb.mlton.grm.sml
	$(MLTON) -output $@ src/lacweb.mlb
