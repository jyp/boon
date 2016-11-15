emacs ?= emacs

cheat.pdf: cheat-sheet.hs Colemak.hs
	nix-shell --run "cabal build"
	nix-shell --run dist/build/boonCS/boonCS
	nix-shell latex.nix --run "xelatex cheat-sheet.tex"
	nix-shell --run dist/build/boonCS/boonCS
	nix-shell latex.nix --run "xelatex cheat-sheet.tex"

test:
	$(emacs) -batch --script boon-test.el

Colemak.hs: boon-tutorial.el boon-colemak.el boon-keys.el
	$(emacs) -batch \
          --eval "(add-to-list 'load-path (expand-file-name \".\"))" \
          --eval "(package-initialize)" \
          -l boon-tutorial.el \
          --eval '(boon-dump-cheatsheet "colemak")'

clean:
	rm -f *.elc

.PHONY: all clean test

