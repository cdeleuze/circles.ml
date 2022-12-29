# where to find the graphics library
GDIR = -I ~/.opam/default/lib/graphics/

clean:
	rm -f *.tex *.dvi *.ps *.aux *.log *.bbl *.blg *.cmi *.cmo

purge: clean
	rm -f *.pdf exemple*.eps

# executable
circles: circles.ml picture.cmo
	ocamlc $(GDIR) graphics.cma picture.cmo circles.ml -o $@

%.cmo: %.ml
	ocamlc $(GDIR) -c -o $@ $<

# pictures included in the litterate program

EXEMPLE_EPS = exemple.eps exempleaf.eps exemplef.eps exemplenb.eps

exemple.eps: circles
	./circles -e 100 75 50 2 > $@

exempleaf.eps: circles
	./circles -e -f 100 75 50 2 > $@

exemplef.eps: circles
	./circles -e -F 100 75 50 3 > $@

exemplenb.eps: circles
	./circles -E -s 1000 150 75 50 5 > $@

# pdf of the litterate program
circles.pdf: circles.tex circles.bbl $(EXEMPLE_EPS)
	latex circles.tex
	dvips circles
	ps2pdf circles.ps

circles.bbl: circles.tex $(EXEMPLE_EPS)
	latex circles
	bibtex circles
	latex circles

circles.tex: circles.ml
	ocamlweb --encoding latin1 --latex-option novisiblespaces \
                 --noindex --header -s $< -o $@
        # we use sed to fix ocamlweb output...
	sed -i -e 's/\*\./\\times\\negthickspace.\\;/g' \
	    -e 's/+\./+\\negthickspace.\\;/g'  \
	    -e 's/-\./-\\negthickspace.\\;/g'  \
	    -e 's/\/\./\/\\negmedspace.\\:/g'  \
	    -e 's/<(-\\negthickspace/<(-/g'      \
	    $@
