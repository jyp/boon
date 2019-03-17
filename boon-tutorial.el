;;; boon-tutorial.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains the tutorial for boon.

;;; Code:

(require 'boon-core)
(require 'boon-keys)
(require 'dash)

(defun boon-dump-map (map)
  "Dump the MAP in a format usable to generate a cheat sheet."
  (concat
   "["
   (-reduce (lambda (x y) (concat x "," y))
            (--map (let* ((b (lookup-key map (make-vector 1 it)))
                          (mn (boon-mnemonic-noformat b map)))
                     (format "(%d,(%S,\"%S\"))" it mn
                             (cond ((symbolp b) b))))
                   (-concat
                    (-iterate '1+ ?A 26)
                    (-iterate '1+ ?a 26)
                    '(?\; ?: ?- ?' ?, ?. ?< ?> ?/ ?? 32 ?\")
                    )))
   "]"))

(defun boon-dump-cheatsheet (flavour)
  "Dump cheatcheat info for FLAVOUR."
  (let ((el (concat "boon-" flavour ".el")))
    (require 'boon)
    (load el)
    (with-temp-buffer
      (insert "module Layout where\n")
      (insert (format "nil = \"\"\n"))
      (insert (format "commandMap = %s\n" (boon-dump-map boon-command-map)))
      (insert (format "movesMap   = %s\n" (boon-dump-map boon-moves-map)))
      (insert (format "selectMap  = %s\n" (boon-dump-map boon-select-map)))
      (write-region nil nil (concat flavour ".hs")))))

(defun boon-keymap-rev-look (sub map)
  "Return an event yielding SUB from the keymap MAP."
  (let (res)
    (map-keymap (lambda (event b)
                  (when (and (consp b) (stringp (car b))) (setq b (cdr b)))
                  (when (eq b sub) (setq res event)))
                map)
    (key-description (vector res))))

(defun boon-mnemonic-noformat (sub &optional map)
  "Return the mnemonic for SUB from the keymap MAP."
  (let (res)
    (map-keymap (lambda (_event b) (when (and (consp b)
                                              (stringp (car b))
                                              (eq (cdr b) sub))
                                     (setq res (car b))))
                (or map boon-command-map))
    res))


(defun boon-mnemonic (sub &optional map)
  "Return the formatted mnemonic for SUB from the keymap MAP."
  (format "(mnemonic: %s)" (boon-mnemonic-noformat sub map)))


;; utilities to create the tutorial
;;;###autoload
(defun boon-gen-key (key)
  "Generate a suitable tutorial string to refer to command KEY."
  (interactive (list (read-key-sequence-vector "key?")))
  (insert "\\\\")
  (insert "[")
  (insert (symbol-name (lookup-key boon-command-map key)))
  (insert "]"))

;; (global-set-key (kbd "C-'") 'boon-gen-key)

;;;###autoload
(defun boon-gen-sel-key (key)
  "Generate a suitable tutorial string to refer to selection KEY."
  (interactive (list (read-key "key?")))
  (insert (concat "\" (selector '" (symbol-name (lookup-key boon-select-map (vconcat (list key)))) ") \"")))

;; (global-set-key (kbd "C-'") 'boon-gen-sel-key)

;;;###autoload
(defun boon-tutorial ()
  "Open a buffer with boon tutorial."
  (interactive)
  (switch-to-buffer (generate-new-buffer "BOON-TUTORIAL"))
  (turn-on-boon-mode)
  (let ((x-key (boon-keymap-rev-look boon-x-map boon-command-map)))
    (cl-flet ((selector (sel) (boon-keymap-rev-look sel boon-select-map)))
  (insert (substitute-command-keys (concat

"Boon tutorial.  See end for copying conditions.

This tutorial assumes that you know Emacs already.

* INTRODUCTION
--------------

Sometimes the tutorial will mention a mnemonic for a key.  A mnemonic
is a story linking the key to type, to the action that it does.  This
story is often small, maybe just a word.  You may use the mnemonics
provided by the tutorial, but it is best to invent your own.  If the
tutorial says 'mnemonic: nil', this means that the frontend that you
have activated has not defined a mnemonic for that command.

Make sure that Boon is active in this buffer.  Call
'\\[turn-on-boon-mode]' if necessary.

Boon mode can be turned off with: '\\[turn-off-boon-mode]'
it restores the default Emacs experience.

Lines that start with the characters \">>\", indicate directions for
trying a command.  For example:

>> Type 'C-k' and 'C-l' to scroll this text.

There is a cheat sheet that can be used as reference while going
through this tutorial.  It's linked in the readme usage section
https://github.com/jyp/boon#usage

Most Emacs key-chord commands (M-... and C-...) will work as expected.


* SWITCHING STATES
------------------

Boon has two states:

- Command state

- Insert state

Boon indicates the difference between command state and insert state
in several ways:

- The modeline says Boon:<STATE> (where <STATE> can be INS or CMD)

- The cursor is a box in command state, and a bar in insert state.

- If the powerline package is installed and '(boon-powerline-theme)'
  is invoked, then the state text changes color.  This makes it
  possible to see the current state without having to read any text.
  (There is also support for spaceline, but it requires a patch to
  said package at the time of writing)

You can switch from command state to insert state by typing:
'\\[boon-set-insert-like-state]' " (boon-mnemonic
'boon-set-insert-like-state)".

To switch from insert state to command state, type: '<ESC>'

Try switching between the states now:

If you're in command state:
>> Type: '\\[boon-set-insert-like-state]' to switch to insert state.

If you're in insert state:
>> Type: '<ESC>' to switch to command state.


* BASIC CURSOR CONTROL
----------------------

How do you move to a specific place within the text on the screen?

The whole right-hand side of the keyboard is dedicated to this
purpose.

There are several ways you can do this.  You can still use the
arrow keys, but it's more efficient to keep your right hand
around the home row and use the commands:

  \\[backward-char] \\[forward-char] \\[previous-line] \\[next-line]

These characters are equivalent to the four arrow keys, like
this:

			  Previous line '\\[previous-line]'
				  :
				  :
     Backward '\\[backward-char]' .... Current cursor position .... Forward '\\[forward-char]'
				  :
				  :
			    Next line '\\[next-line]'

>> Practice by moving the cursor along the text in the diagram above.

You'll find it easy to remember these letters by their location
on the keyboard.  Note that, when you navigate within a line your
hand stays on the home row.  Navigating between lines happens on
the top row.  Very soon you will forget the letters that your hand
is typing when moving the cursor, you'll know intuitively what
you're doing, as when using arrow keys.

You will be using these basic cursor positioning commands a lot,
but there are even faster ways to go about moving the cursor.

If moving on character at a time is too slow, you can move by
words.  The '\\[boon-smarter-forward]' key moves forward a word and '\\[boon-smarter-backward]' moves back a word.

>> Type '\\[boon-smarter-forward]' and '\\[boon-smarter-backward]' a couple of times.

As in regular Emacs, when you are in the middle of a word, '\\[boon-smarter-forward]' moves
to the end of the word.  When you are in whitespace between words, '\\[boon-smarter-forward]'
moves to the end of the following word.  The '\\[boon-smarter-backward]' key works likewise in
the opposite direction.  In fact, '\\[boon-smarter-forward]' and '\\[boon-smarter-backward]' move by whole syntactic
units, they will skip over parentheses when it makes sense.

>> Move the cursor to the \"*\" in the following expression.
   Using: '\\[boon-smarter-forward]', '\\[boon-smarter-backward]', '\\[backward-char]' and '\\[forward-char]' to get to the place you want.

54 / ((8 + y) * (x - 3))

Notice that you can quickly move in the expression while staying
comfortably on the home row.

You can also move to the beginning or end of the current line:

>> Type '\\[boon-beginning-of-line]' to move to the beginning of a line.

>> Type '\\[boon-end-of-line]' to move to the end of a line.

Like '\\[previous-line]' and '\\[next-line]', the keys on the top row are line-based.

When moving up and down, Emacs tries to manage the cursor position
inside a line intelligently.  This often works, but sometimes one need
to move quickly to the beginning or end of a line after moving up or
down.  You can do all that by staying on the top row.

If you want to moving up and down faster, use the shift key:

>> Type '\\[backward-paragraph]' (shift i) to move to the previous paragraph.

>> Type '\\[forward-paragraph]' (shift letter o) to move to the next paragraph.

You also can jump several steps at once by typing a number before
a command.

>> Try '5' before a movement command.  For example:
   '5 i' moves up 5 lines, and `5 o' moves back down 5 lines.

>> Try all of the above commands now a few times for practice.
   These are the commands that are used most often.

Before trying the next two commands, remember that 'I' and 'O' moves
by paragraph, use them to return to the next section after trying the
following commands.

Two less used cursor motion commands, moves you to the top or bottom of
the whole text.

>> Type '\\[beginning-of-buffer]' to move to the beginning of the whole text.

>> Type '\\[end-of-buffer]' to move to the end of the whole text.


* OTHER MOVEMENT COMMANDS
-------------------------

- '\\[boon-switch-mark]' " (boon-mnemonic 'boon-switch-mark) " pops a mark and jumps to it.
  If a region is active, exchange point and mark.

- '\\[xref-find-definitions]' is bound to `xref-find-definitions'.

If the Avy package is installed:
- '\\[avy-goto-word-1]' " (boon-mnemonic 'avy-goto-word-1) " activates `avy-goto-word-1'

* IF EMACS STOPS RESPONDING
---------------------------

If Emacs stops responding to your commands, you can stop it safely
by typing 'C-g'.  You can also use 'C-g' to stop a command which is
taking too long to execute.

And 'C-g' can discard an argument or cancel a command that you do not
want to finish.

'<ESC>' is also an alternative which works in many contexts.


* 'C-x' prefix
-------------

Instead of the 'C-x' prefix; you may just type " x-key "

>> Type '\\[split-window-below]' to split this window.

>> Type '\\[delete-other-windows]' to close the other windows.

Additionally, the `execute-extended-command' command is bound to
'\\[execute-extended-command]'.  It is a good idea to bind your
own favourite commands in `boon-x-map', so you can access them
via " x-key ".  (Standard commands are always available under 'C-x')


* 'C-c' prefix
------------

Mode-specific commands often have the form 'C-c C-<letter>'.  These
are accessible by typing simply '\\[boon-c-god] <letter>' from command state.
Unfortunately there is no such binding in text mode by default
--- so you cannot test this right away.


* UNDO
------

Undo is bound to '\\[undo]'.


* INSERTING
-----------

When you want to insert text, enter insert state by typing:
'\\[boon-set-insert-like-state]' (can you remember the mnemonic?).

Ordinary characters, like A, 7, *, etc., are then inserted as you type
them.  To insert a Newline character, simply type '<Return>'.

In insert state, regular Emacs editing commands also work.

>> Type '\\[boon-set-insert-like-state]' to enter insert state and type some text

>> Type '<ESC>' to go back to command state.

A single character can also be inserted in command state by first
typing the quote command: '\\[boon-quote-character]' (mnemonic: quote)

>> Type '\\[boon-quote-character] *' to insert a character in command state.

And just like you can move several steps at once, you can also insert
a character several times from command state, by pressing a number key
before pressing '\\[boon-quote-character]’.

>> Try that now -- type ’8q*’ to insert ********.


* TAKING (DELETING)
----------

Taking (deleting) text is mostly done with the:
'\\[boon-take-region]' key " (boon-mnemonic 'boon-take-region) ".

The take command expects an argument.  This argument can be any
right-hand move command (in `boon-moves-map'), such as '\\[backward-char]'.

>> Type '\\[boon-take-region] \\[backward-char]' to delete the character before the cursor

In the above instructions, '\\[backward-char]' is the argument to the '\\[boon-take-region]' command.

>> Type '\\[boon-take-region] \\[boon-smarter-backward]' to delete backwards, up to the beginning of a word

You can also use a left-hand _region specifier_ as an argument to
'\\[boon-take-region]'.  One such arguments is '" (selector 'boon-select-wim) "', which refers to the symbol or (sexp)
at point.

>> Type '\\[boon-take-region] " (selector 'boon-select-wim) "' to delete the symbol where the cursor is
   (even if it's in the middle of the symbol)

One of the most useful region specifiers is '\\<boon-select-map>\\[boon-select-line]\\<boon-command-map>', which specifies the
current line.

>> Type '\\[boon-take-region] \\<boon-select-map>\\[boon-select-line]\\<boon-command-map>' to delete the current line.

The region specifiers are defined in the `boon-select-map' keymap.
(Type '\\[describe-variable] boon-select-map' '<Return>' to inspect it)


Region arguments can be given a repeat count.

>> Type '\\[boon-take-region] 7 \\[forward-char]' to delete seven characters forward.

You can also kill a segment of text by selecting it first, and then
use the kill command.

>> Move the cursor to the \"a\" in \"also\" in the previous paragraph.

>> Type '\\[boon-drop-mark]'.  Emacs should display a message \"mark 0\" at the bottom
   of the screen.

>> Move the cursor to the \"c\" in \"command\", on the second line of the
   same paragraph.

>> Type '\\[boon-take-region]'.  This will kill the text starting from the \"a\", and
   ending just before the \"c\".

Selecting text with '\\[boon-drop-mark]' can take a region argument, including
left-hand once.  When this argument is a move command, then '\\[boon-drop-mark]'
behaves like putting a mark at the the current point.

>> Type '\\[boon-drop-mark] \\<boon-select-map>\\[boon-select-paragraph]\\<boon-command-map>' to select a paragraph.

>> Type '\\[boon-drop-mark]' with an active selection to cancel the selection.

The region specifier `boon-select-line' '" (selector 'boon-select-line) "' can be
given to the marking command '\\[boon-drop-mark]'.

>> Type '\\[boon-drop-mark] " (selector 'boon-select-line) "' to select the current line.

>> Type '\\[next-line]' a few times to select some lines.

>> Type '\\[boon-take-region]' to delete all selected lines.

You can kill and switch to insert state with a single command:
'\\[boon-substitute-region]' " (boon-mnemonic 'boon-substitute-region) ".

>> Type '\\[boon-substitute-region] " (selector 'boon-select-wim) "' to replace the word/symbol at point.

To delete the word under the cursor.

>> Move the cursor to a word you wish to kill (Cut).

>> Type '\\[boon-take-region] \\<boon-select-map>\\[boon-select-with-spaces] \\[boon-select-word]\\<boon-command-map>' to remove the word and it's surrounding spaces.


* TREASURE (Copy, `kill-ring-save')
------------

The command for coping is: '\\[boon-treasure-region]'.

It adds the copied text to the `kill-ring' but doesn't remove it from
the buffer.

If a region is active, then the region contents gets copied.
Otherwise it expects a motion command.

>> Type '\\[boon-treasure-region] " (selector 'boon-select-wim) "' to copy the symbol under the cursor.

>> Type '\\[boon-treasure-region] \\[boon-drop-mark]' to copy the current line.


* FETCH (YANK, PASTE)
---------------------

The command for fetching (yanking, pasting) is: '\\[boon-splice]' (mnemonic: fetch).

>> Try it, type '\\[boon-splice]' to yank the text back from the `kill-ring'.

A numeric argument to '\\[boon-splice]' is the number of times that you want to
yank the text.

'\\[yank-pop]' (shift f) calls `yank-pop', which replaces the just yanked text
with the previous entry in the `kill-ring'


* REPEATE
---------

The last complex command can be repeated by typing: '\\[boon-repeat-command]'.

In Emacs, complex commands are defined as those which demand
minibuffer input.  This definition includes commands which take region
arguments, such as `boon-take-region' and `boon-substitute-region'.
We extend this definition with the insert command.

>> Substitute a word by typing: '\\[boon-substitute-region] " (selector 'boon-select-wim) "'
   (it removes the word and enters insert state).

>> Type another word as a replacement.

>> Return to command state with: '<ESC>'

>> Move the cursor to another word and type: '\\[boon-repeat-command]'
   (the word was substituted with the same replacement text).

All complex commands, including inserts, are saved in the
‘command-history’.  You can can conjure up (and fix) any complex
command of your choice using '\\[repeat-complex-command]'.


* WHITESPACE
------------

While using standard Emacs commands, it sometimes gets annoying to
manage whitespace.  Yanking a word may leave extra space here, and a
lack of space there.  In Boon, repeating the '\\[boon-splice]' command automatically
fixes spaces, using a heuristic based on the syntax-table.

>> Move the cursor to the \"v\" inside the following quotes: \" very\"

>> Type: '\\[boon-treasure-region] \\<boon-select-map>\\[boon-select-with-spaces] \\[boon-select-word]\\<boon-command-map>' to copy the word with the leading space.

>> Move the cursor to the first letter of the word \"annoying\" in the
   previous paragraph.

>> Type: '\\[boon-splice]' to yank (paste) and notice how the spacing is wrong.

>> Repeat '\\[boon-splice]' and see how the spacing gets fixed for you.

In Emacs, killing extra spaces between words can be done in hindsight.
That is, you can cut a word with or without spaces before or after it.

But Emacs automatically \"glues\" adjacent cuts in the `kill-ring'.

With Boon, cutting spaces can also be done in foresight, even in the
middle of a word, using the region selector transformer: '\\<boon-select-map>\\[boon-select-with-spaces]'.  It
includes the space following a word or the leading space if it's the
last word (ending with a period).


* ENCLOSURES (PARENS, ETC.)
---------------------------

Boon provides help with manipulating enclosures.

The command '\\<boon-select-map>\\[boon-select-borders]\\<boon-command-map>' " (boon-mnemonic 'boon-enclose) " adds enclosures around a region.

It takes two arguments:

1. The kind of enclosure to use ('p' for regular parentheses)

2. The region to surround.  This region is specified by the same
   language as the arguments to '\\[boon-take-region]' or '\\[boon-drop-mark]' commands.

>> Type '\\<boon-select-map>\\[boon-select-borders]\\<boon-command-map> p \\<boon-select-map>\\[boon-select-wim]\\<boon-command-map>' inside a symbol, to enclose it in parens.

It is often useful to put enclosures around what has just been pasted.
The just pasted region is accessible with: '\\[boon-toggle-mark]' (single quote).

>> Delete a word with '\\[boon-take-region] \\<boon-select-map>\\[boon-select-word]\\<boon-command-map>', then paste it with '\\[boon-splice]'.

>> Put parens around the just pasted word with: '\\[boon-enclose] p \\[boon-toggle-mark]'.

>> Inspect the kind of enclosures available with:
   '\\[customize-variable] boon-enclosures' '<Return>'

It is also possible to remove enclosures with the '\\<boon-select-map>\\[boon-select-borders]\\<boon-command-map>' operator, which
transforms a region argument to its enclosure (the first and last characters).

Just delete the parentheses:

>> Move the cursor inside this (sexp).

>> Type '\\[boon-take-region] \\<boon-select-map>\\[boon-select-borders] \\[boon-select-outside-pairs]\\<boon-command-map>'.

Or delete the whole expression:

>> Move the cursor inside this (sexp).

>> Type '\\[boon-take-region] \\<boon-select-map>\\[boon-select-outside-pairs]\\<boon-command-map>'.

The '\\<boon-select-map>\\[boon-select-borders]\\<boon-command-map>' operator is especially useful when applied to '\\<boon-select-map>\\[boon-select-outside-pairs]\\<boon-command-map>', '\\[boon-smarter-backward]'
and '\\[boon-smarter-forward]' --- but it works on any region.


* SEARCHING
-----------

Search forward:

\\[isearch-forward] --> `isearch-forward'
\\[next-error] --> `next-error'
\\[boon-qsearch-next-at-point] --> `boon-qsearch-next-at-point'
\\[boon-qsearch-next] --> `boon-qsearch-next'

Search backward:

\\[isearch-backward] --> `isearch-backward'
\\[previous-error] --> `previous-error'
\\[boon-qsearch-previous-at-point] --> `boon-qsearch-previous-at-point'
\\[boon-qsearch-previous] --> `boon-qsearch-previous'

Lists all lines that match a regexp:

\\[occur] --> `occur'


* MULTIPLE REGIONS
------------------

Boon provides a region operator that works on all occurrences of a
string in a region.  The operator takes two arguments:
1. A string.
2. And a region that contains the matching strings.
You can remove all occurrences of Boon in this paragraph:

>> Move the cursor inside the word \"Boon\" in the previous paragraph.

>> Type: '\\[boon-take-region] \\<boon-select-map>\\[boon-select-occurences] \\[boon-select-wim] \\[boon-select-paragraph]\\<boon-command-map>'


* MULTIPLE CURSORS
------------------

If you have the \"multiple-cursors\" package installed, then you can
replace all occurrences.

>> Undo the previous command.

>> Move the cursor inside the word \"Boon\" again.

>> Type '\\[boon-substitute-region] \\<boon-select-map>\\[boon-select-occurences] \\[boon-select-wim] \\[boon-select-paragraph]\\<boon-command-map>'

>> Type the replacement text.

>> Exit \"multiple-cursors\" by typing '<ESC>' repeatedly.

Boon has special support for multiple cursors, which means that all
commands described in this tutorial should work out of the box with
multiple cursors.


* SPECIAL BUFFERS
-----------------

Certain Emacs modes already offer their own command system.  Such
modes include \"dired\", \"magit\", and others.  In such buffers, it
makes little sense to have an insert state, and most edit commands
don't make sense either.

In such modes Boon switches to a \"special state\", where the modeline
shows: Boon:SPC, and only overrides the 'x' key, which is bound to
`boon-x-map'.

You can get the original behavior of x keys by prefixing it
with (\\<boon-special-map>\\[boon-quote-character]).


* COPYING
---------

This tutorial descends from a long line of Emacs tutorials starting
with the one written by Stuart Cracraft for the original Emacs.

This version of the tutorial is not a part of GNU Emacs, but derived
from the standard Emacs tutorial,


  Copyright (C) 1985, 1996, 1998, 2001-2013 Free Software Foundation,
  Inc.
")))))
(goto-char 1))

;;; boon-tutorial.el ends here
