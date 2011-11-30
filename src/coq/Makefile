MODULES := Axioms Name Syntax Semantics
VS      := $(MODULES:%=%.v)

.PHONY: coq clean

coq: Makefile.coq
	make -f Makefile.coq

Makefile.coq: Makefile $(VS)
	coq_makefile -impredicative-set $(VS) -o Makefile.coq

clean:: Makefile.coq
	make -f Makefile.coq clean
	rm -f Makefile.coq
