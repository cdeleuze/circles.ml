circles.tex: circles.ml
	ocamlweb --latex-option novisiblespaces --noindex --header -s  circles.ml -o circles.tex
	cat circles.tex | \
	sed -e 's/\*\./\\times\\negthickspace.\\;/g' \
	    -e 's/+\./+\\negthickspace.\\;/g'  \
	    -e 's/-\./-\\negthickspace.\\;/g'  \
	    -e 's/\/\./\/\\negmedspace.\\:/g'  \
	    -e 's/<(-\\negthickspace/<(-/g'      \
	    > /tmp/c.tex
	mv /tmp/c.tex circles.tex

circles.pdf: circles.tex
	latex circles.tex
	dvips circles
	ps2pdf circles.ps
