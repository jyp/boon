emacs ?= emacs

sheets: colemak.pdf qwerty.pdf

%.pdf: cheat-sheet.hs %.hs
	cp $*.hs Layout.hs
	nix-shell --run "cabal build"
	nix-shell --run "dist/build/boonCS/boonCS $*" # generate the size of boxes
	nix-shell latex.nix --run "xelatex $*.tex"
	nix-shell --run "dist/build/boonCS/boonCS $*" # generate the diagram according to the above sizes
	nix-shell latex.nix --run "xelatex $*.tex"

test:
	$(emacs) -batch --script boon-test.el

BOON_ENV = -batch \
          --eval "(add-to-list 'load-path (expand-file-name \".\"))" \
          --eval "(package-initialize)" \

%.hs:  boon-tutorial.el boon-%.el boon-keys.el
	$(emacs) $(BOON_ENV) -l boon-tutorial.el \
          --eval "(boon-dump-cheatsheet \"$*\")"

clean:
	rm -f *.elc

.PHONY: all clean test

