cheat.pdf: cheat-sheet.hs
	ghc cheat-sheet.hs
	./cheat-sheet
	xelatex cheat-sheet.tex
	./cheat-sheet
	xelatex cheat-sheet.tex
