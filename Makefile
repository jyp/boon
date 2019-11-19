all: sheets

emacs ?= emacs

sheets: colemak.pdf qwerty.pdf qwertz.pdf workman.pdf

%.pdf: cheat-sheet.hs %.hs
	cp $*.hs Layout.hs
	cabal new-build
	nix-shell --run "cabal new-exec boonCS $*" # generate the size of boxes
	xelatex $*.tex
	nix-shell --run "cabal new-exec boonCS $*" # generate the diagrams according to the above sizes
	xelatex $*.tex

test:
	$(emacs) -batch --script boon-test.el

BOON_ENV = -batch \
          --eval "(add-to-list 'load-path (expand-file-name \".\"))" \
          --eval "(package-initialize)" \

%.hs:  boon-tutorial.el boon-%.el boon-keys.el
	$(emacs) -q $(BOON_ENV) -l boon-tutorial.el \
          --eval "(boon-dump-cheatsheet \"$*\")"

clean:
	rm -f *.elc

.PHONY: all clean test

