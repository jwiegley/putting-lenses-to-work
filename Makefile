PYTHON	 = /usr/bin/python
PRESENT	 = /Applications/Misc/Présentation.app/Contents/MacOS/presentation.py
PDF	 = putting-lenses-to-work.pdf
COQFLAGS = "-I $(HOME)/src/category-theory/Endo"
EMACS    = emacs

all: $(PDF)

open: $(PDF)
	open $<

present: all
	$(PYTHON) $(PRESENT) $(PDF)

%.tex: %.org
	$(EMACS) -batch -L ~/.emacs.d \
	    -l init -l settings -l org-settings -l ox-beamer \
	    --eval="(progn (find-file \"$<\") (org-beamer-export-to-latex))"

%.pdf: %.tex
	pdflatex $< && pdflatex $< && pdflatex $<

clean:
	rm -fr html
	rm -f *.tex *.pdf *.vrb *.aux *.log *.nav *.out *.snm *.toc *.upa
	rm -f src/*.d src/*.vo src/*.glob Makefile.coq
