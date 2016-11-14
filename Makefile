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

Colemak.hs:
	$(emacs) -batch \
          --eval "(add-to-list 'load-path (expand-file-name \".\"))" \
          --eval "(package-initialize)" \
          -l boon-tutorial.el \
          --eval '(boon-dump-cheatsheet "colemak")'

clean:
	rm -f *.elc

.PHONY: all clean test

