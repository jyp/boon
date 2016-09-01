cheat.pdf: cheat-sheet.hs
	ghc --make cheat-sheet.hs -main-is CC
	./cheat-sheet
	xelatex cheat-sheet.tex
	./cheat-sheet
	xelatex cheat-sheet.tex
