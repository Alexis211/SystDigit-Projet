
all: rapport_final.pdf

%.pdf: %.tex
	pdflatex $<

%.tex: %.lytex
	lilypond-book --pdf $<
