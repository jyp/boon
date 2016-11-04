emacs ?= emacs

cheat.pdf: cheat-sheet.hs
	ghc --make cheat-sheet.hs -main-is CC
	./cheat-sheet
	xelatex cheat-sheet.tex
	xelatex qwerty.tex
	./cheat-sheet
	xelatex qwerty.tex

test:
	$(emacs) -batch --script boon-test.el

clean:
	rm -f *.elc

.PHONY: all clean test

