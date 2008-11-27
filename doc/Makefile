PAPERS=manual

FIGURES=

all: $(PAPERS:%=%.dvi) $(PAPERS:%=%.ps) $(PAPERS:%=%.pdf) 

%.dvi: %.tex $(FIGURES:%=%.eps)
	latex $<
	latex $<

%.ps: %.dvi
	dvips $< -o $@

%.pdf: %.dvi $(FIGURES:%=%.pdf)
	pdflatex $(<:%.dvi=%)

%.pdf: %.eps
	epstopdf $<

clean:
	rm -f *.aux *.bbl *.blg *.dvi *.log *.pdf *.ps

.PHONY: all clean
