PYTHON	 = /usr/bin/python
PRESENT	 = /Applications/Misc/Présentation.app/Contents/MacOS/presentation.py
PDF	 = putting-lenses-to-work.pdf
EMACS    = emacs

all: $(PDF)

open: $(PDF)
	open $<

present: all
	$(PYTHON) $(PRESENT) $(PDF)

# Ensure all examples work before building the slide deck
%.tex: %.org test/Main.hs putting-lenses-to-work.cabal Makefile
	cabal build
	./dist/build/putting-lenses-to-work/putting-lenses-to-work
	$(EMACS) -batch -L . -L ~/.emacs.d -l lenses \
	    -l init -l settings -l org-settings -l ox-beamer \
	    --eval="(progn (find-file \"$<\") (extract-code-blocks) (setq org-export-latex-minted-options '((\"fontsize\" \"\\\\small\") (\"linenos\" \"true\"))) (org-beamer-export-to-latex))"

%.pdf: %.tex
	pdflatex -shell-escape -interaction nonstopmode $<
	pdflatex -shell-escape -interaction nonstopmode $<
	pdflatex -shell-escape -interaction nonstopmode $<

clean:
	rm -fr html
	rm -f *.tex *.pdf *.vrb *.aux *.log *.nav *.out *.snm *.toc *.upa
	rm -f src/*.d src/*.vo src/*.glob
