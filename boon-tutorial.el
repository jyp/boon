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

"Boon tutorial.   See end for copying conditions.
\\<boon-command-map>
This tutorial assumes that you know Emacs already.

Note on the tutorial: sometimes the tutorial will mention a
mnemonic for a key. A mnemonic is a story linking the key to type
to the action that it does. This story is often small, maybe just
a word. You may use the mnemonics provided by the tutorial, but
it is best to invent your own. If the tutorial says 'mnemonic:
nil', this means that the frontend that you have activated has
not defined a mnemonic for that command.

Make sure that Boon is active in this buffer. Call
\\[turn-on-boon-mode] if necessary.

Boon has two states: command state and insert state. Boon
indicates the difference between command state and insert state
in several ways:

- The modeline says Boon:<STATE> (where <STATE> can be INS or CMD)

- The cursor is a box in command state, and a bar in insert state.

- If you invoke `(boon-powerline-theme)' and have powerline
  installed then the state text will be shown using various
  colors. This is useful to find out what state you're in without
  having to read any text. (There is also support for spaceline,
  but it requires a patch to said package at the time of writing)

You can switch from command to insert mode by typing
\\[boon-set-insert-like-state] " (boon-mnemonic
'boon-set-insert-like-state)". Go back to command mode by typing
<ESC>.

>> Switch to command mode now (type <ESC>)

Within Boon, most Emacs key-chord commands (M-... and C-...) are
accessible as normal.  For example, \\[turn-off-boon-mode]
reverts to your regular experience.

In this tutorial, the characters \">>\" at the left margin
indicate directions for you to try using a command.  For
instance:

>> Type \\[scroll-down-line] and \\[scroll-up-line] to scroll this text


* BASIC CURSOR CONTROL
----------------------


How do you move to a specific place within the text on the screen?

The whole right-hand side of the keyboard is dedicated to this
purpose.

There are several ways you can do this.  You can still use the
arrow keys, but it's more efficient to keep your right hand
around the home row and use the commands:

  \\[backward-char] \\[forward-char] \\[previous-line] \\[next-line].

These characters are equivalent to the four arrow keys, like
this:

			  Previous line, \\[previous-line]
				  :
				  :
   Backward, \\[backward-char]  .... Current cursor position .... Forward, \\[forward-char]
				  :
				  :
			    Next line, \\[next-line]

>> Move the cursor to the line in the middle of that diagram.

You'll find it easy to remember these letters by their location
on the keyboard.  Note that, when you navigate within a line your
hand stays on the home row.  Navigating between lines happens on
the top row. Very soon you will forget the letters that your hand
is typing when moving the cursor, you'll know intuitively what
you're doing, as when using arrow keys.

You will be using these basic cursor positioning commands a lot,
but there are even faster ways to go about moving the cursor.

If moving on character at a time is too slow, you can move by
words. The '\\[boon-smarter-forward]' key moves forward a word and
'\\[boon-smarter-backward]' moves back a word.

>> Type a few \\[boon-smarter-forward]'s and \\[boon-smarter-backward]'s.

As in regular Emacs, when you are in the middle of a word, \\[boon-smarter-forward] moves to
the end of the word.  When you are in white space between words, \\[boon-smarter-forward]
moves to the end of the following word.  The \\[boon-smarter-backward] key works likewise in the
opposite direction. In fact, \\[boon-smarter-forward] and \\[boon-smarter-backward] move by whole syntactic units:
they will skip over parentheses when it makes sense.

>> Move the cursor to the '*' in the following expression. Move to the
   relevant line, then type \\[boon-smarter-forward] and \\[boon-smarter-backward] a few times. Combine these keys
   with \\[backward-char] and \\[forward-char] to get to the place you want.

54 / ((8 + y) * (x - 3))

Notice that you can quickly move in the expression while staying
comfortably on the home row.

You can move to the beginning or end of a line by typing
\\[boon-beginning-of-line] or \\[boon-end-of-line].  As
\\[previous-line] and \\[next-line], these line-based commands
are on the top row.  When moving up and down, Emacs tries to
manage the cursor position inside a line intelligently.  This
often works, but one sometimes need to quickly move to the
beginning or end of line after moving up or down. You can do all
this by staying on the top row.

If you want to speed up moving up and down, use the shift key: \\[backward-paragraph] and
\\[forward-paragraph] move by whole paragraphs.

>> Try moving around using the above commands.

Checkout the cheat sheet (linked from README) for a summary of
movement operations.

>> Try all of the above commands now a few times for practice.
   These are the most often used commands.

Two other (less important) cursor motion commands are
'\\[beginning-of-buffer]' , which moves to the beginning of the
whole text, and '\\[end-of-buffer]', which moves to the end of the whole text.

>> Try \\[beginning-of-buffer] now, to move to the beginning of the tutorial.
   Then use \\[forward-paragraph] repeatedly to move back here.

You can type a prefix argument by typing a number before the
command.

>> Move forward by seven words.

To insert a character several times, you can use the escaping
command '\\[boon-quote-character]' " (boon-mnemonic 'boon-quote-character) "

>> Try that now -- type '8\\[boon-quote-character]*' to insert ********.


* OTHER MOVEMENT COMMANDS
-------------------------

There are two other movement commands, bound to \\[avy-goto-word-1] and \\[boon-switch-mark].

- \\[boon-switch-mark] " (boon-mnemonic 'boon-switch-mark) " jumps pops a mark and jumps to it. (If a region is active, exchange point and mark)

- \\[avy-goto-word-1] " (boon-mnemonic 'avy-goto-word-1) " activates avy-goto-word-1 (if installed)

- Additionally, \\[xref-find-definitions] is bound to `xref-find-definitions'.

* IF EMACS STOPS RESPONDING
---------------------------

If Emacs stops responding to your commands, you can stop it safely by
typing C-g.  You can use C-g to stop a command which is taking too
long to execute.

You can also use C-g to discard an argument or the beginning of a
command that you do not want to finish.

<ESC> is an alternative which works in many contexts.


* C-x prefix
-------------

Instead of the C-x prefix; you may just type " x-key "

>> Type \\[split-window-below] to split this window
>> Type \\[delete-other-windows] to close the other windows

Additionally, the `execute-extended-command' command is bound to
'\\[execute-extended-command]'. It is a good idea to bind your
own favourite commands in `boon-x-map', so you can access them
via " x-key ". (Standard commands are always available under C-x)

* C-c prefix
------------

Mode-specific commands have often the form 'C-c
C-<letter>'. These are accessible by typing simply
'\\[boon-c-god]<letter>' from command mode. Unfortunately there
is no such binding in text mode by default --- so you cannot test
this right away.


* INSERTING AND DELETING
------------------------

If you want to insert text, type '\\[boon-set-insert-like-state]' (can you remember the mnemonic?).
Ordinary characters, like A, 7, *, etc., are then inserted as you type
them.  To insert a Newline character, simply type <Return>.

In insert mode, regular Emacs editing commands can be used.

>> Type \\[boon-set-insert-like-state] to insert some text; then <ESC> to go back to command mode.

Deleting text is mostly done with the '\\[boon-take-region]' key " (boon-mnemonic 'boon-take-region) ".
The take command expects an argument. This argument can be any
right-hand move command (in `boon-moves-map'), such as '\\[backward-char]'.

>> Type '\\[boon-take-region] \\[backward-char]' to delete the character before the cursor

In the above, \\[backward-char] is the argument to the \\[boon-take-region] command.


>> Type '\\[boon-take-region] \\[boon-smarter-backward]' to delete backwards, up to the beginning of a word

You can also use a left-hand _region specifier_ as an argument to
`boon-take-region'. One of such arguments is '" (selector 'boon-select-wim) "', which refers to the symbol
(or sexp) at point.

>> Type \\[boon-take-region] " (selector 'boon-select-wim) " to delete the symbol where the cursor is (even if in the
   middle of the symbol)

One of the most useful region specifier is \\<boon-select-map>\\[boon-select-line]\\<boon-command-map>, which specifies the
current line.

>> Type \\[boon-take-region] \\<boon-select-map>\\[boon-select-line]\\<boon-command-map> to delete the current line.

The region specifiers are defined in the `boon-select-map' keymap. (Type
'\\[describe-variable] boon-select-map <return>' to inspect it)


Region arguments can be given a repeat count.

>> Type '\\[boon-take-region] 7 \\[forward-char]' to delete seven characters forward.


You can also kill a segment of text by selecting it first, then
use the kill command. Move to one end of the region you want to kill, and type
\\[boon-drop-mark].  Next, move the cursor to the other end of
the text you intend to kill.  As you do this, Emacs highlights
the text between the cursor and the position where you typed
\\[boon-drop-mark].  Finally, type \\[boon-take-region].  This kills all the text between the
two positions.


>> Move the cursor to the Y at the start of the previous paragraph.
>> Type \\[boon-drop-mark].  Emacs should display a message \"mark 0\"
   at the bottom of the screen.
>> Move the cursor to the n in \"end\", on the second line of the
   paragraph.
>> Type \\[boon-take-region].  This will kill the text starting from the Y,
   and ending just before the n.

Selecting text with \\[boon-drop-mark] actually takes a region argument. When this
argument is a move command, \\[boon-drop-mark] behaves like putting a mark at the
current point. But, \\[boon-drop-mark] can take any region argument, including
left-hand ones.

>> Type \\[boon-drop-mark] \\<boon-select-map>\\[boon-select-paragraph]\\<boon-command-map> to select a paragraph

>> Type \\[boon-drop-mark] again to undo the selection

In particular, the region specifier `boon-select-line' (" (selector 'boon-select-line) ") can be
given to the marking command (\\[boon-drop-mark]).

>> Type \\[boon-drop-mark] " (selector 'boon-select-line) " to select the current line
>> Type \\[next-line] a few times to select some lines
>> Type \\[boon-take-region] to delete all these lines

You can kill and switch to insert mode in a single command, bound to
'\\[boon-substitute-region]' " (boon-mnemonic 'boon-substitute-region) ".

>> Try typing \\[boon-substitute-region] " (selector 'boon-select-wim) " to replace the symbol at point.

The command for yanking is '\\[boon-splice]'. " (boon-mnemonic `boon-splice) "

>> Try it; type \\[boon-splice] to yank the text back.

The universal argument to \\[boon-splice] is the number of times
that you want to yank the text.

(Shifted \\[yank-pop] does `yank-pop'.)

* REPEATING
-----------

The last complex command can be repeated by typing
\\[boon-repeat-command].  In emacs, complex commands have the
same are defined as those which demand minibuffer input. This
definition includes commands which take region arguments, such as
`boon-take-region' and `boon-substitute-region'. We extend this
definition with the insert command.

>> Substitute a word by typing \\[boon-substitute-region] " (selector 'boon-select-wim) " replacement <ESC>
>> Move the cursor to another word
>> Substitute that other word by the same replacement by typing \\[boon-repeat-command]

All complex commands, including inserts, are saved in the
`command-history'. You can can conjure up (and fix) any complex
command of your choice using \\[repeat-complex-command]


* WHITESPACE
------------

Using standard Emacs commands, it is sometimes annoying to manage
whitespace. Indeed, yanking a word may leave extra space here and
lack a space there. In Boon, repeating the \\[boon-splice] command
automatically fixes spaces, using a heuristic based on the
syntax-table.

>> Copy the word \" very\" in this sentence to the kill ring, including
   a space before and no space after

>> Move the cursor to the first letter of the word 'annoying' in the previous
   paragraph.

>> Splice the word \" very\", and notice how the spacing is wrong.

>> Repeat the \\[boon-splice] command and see how the spacing gets fixed for you.

In emacs, killing extra spaces between words can be done in
hindsight. That is, you can cut a word, and then the space
before (or after) such word. Emacs will automatically 'glue'
adjacent cuts in the killring.

With boon, cutting space can also be done in foresight even in
the middle of a word, by using the region selector tranformer
`boon-select-with-spaces', which adds the spaces following the
region (if such do not exist, it adds the spaces before).

>> Move the cursor to a word you wish to cut
>> type '\\[boon-take-region] \\<boon-select-map>\\[boon-select-with-spaces] \\[boon-select-word]\\<boon-command-map>' to remove the word and convenient surrounding spaces.


* PARENS
--------

Boon provides help to manipulate parentheses.

The command '\\[boon-enclose]' " (boon-mnemonic 'boon-enclose) " adds parentheses around a region.
It takes two arguments:


1. the kind of parentheses to use (by defaut 'p' for regular parentheses)

2. the region to surround. This region is specified by the same
   language as the arguments to \\[boon-take-region] or \\[boon-drop-mark] commands.

>> Move the cursor inside a word and type '\\[boon-enclose] p \\<boon-select-map>\\[boon-select-wim]\\<boon-command-map>' to
   enclose it in parens.


It is often useful to put parens around what has been just
pasted. The just pasted region is accessible using (\\[boon-toggle-mark]).

>> kill a word, then paste it.
>> Put parens around the just pasted word. (\\[boon-enclose] p \\[boon-toggle-mark])

>> Inspect the kind of parentheses available by typing '\\[customize-variable]
   boon-enclosures <RET>'

It is possible to remove parentheses by using the '\\<boon-select-map>\\[boon-select-borders]\\<boon-command-map>' operator, which
transforms a region argument to its enclosure (its first and last characters).

>> Move the cursor inside a (sexp), and type '\\[boon-take-region] \\<boon-select-map>\\[boon-select-outside-pairs]\\<boon-command-map>' to delete the
   expression.

>> Move the cursor inside another (sexp), and type '\\[boon-take-region] \\<boon-select-map>\\[boon-select-borders] \\[boon-select-outside-pairs]\\<boon-command-map>' to delete the
   parens.

The '\\<boon-select-map>\\[boon-select-borders]\\<boon-command-map>' operator is especially useful when applied to '\\<boon-select-map>\\[boon-select-outside-pairs]\\<boon-command-map>' (sexp), '\\[boon-smarter-backward]'
and '\\[boon-smarter-forward]' --- but it works on any region.

* UNDO
------

Undo is bound to '\\[undo]'.

* SEARCHING
-----------

Various searching functions are bound to the same prefix. Examples:
\\<boon-moves-map>

\\[isearch-forward] --> `isearch-forward'
\\[next-error] --> `next-error'
\\[boon-qsearch-next-at-point] --> `boon-qsearch-next-at-point'
\\[boon-qsearch-next] --> `boon-qsearch-next'

Backward searches use another prefix:

\\[isearch-backward] --> `isearch-backward'
\\[previous-error] --> `previous-error'
\\[boon-qsearch-previous-at-point] --> `boon-qsearch-previous-at-point'
\\[boon-qsearch-previous] --> `boon-qsearch-previous'

\\<boon-command-map>


Check `boon-moves-map` for a complete list, or type the prefix
followed by C-h.

* MULTIPLE REGIONS, MULTIPLE CURSORS
------------------------------------

Boon provides a region operator to work on all occurrences of a string
in a region. This operator takes two arguments: 1. the string 2. the
region where to match the string. The string itself is specified by a
region containing it. For example, you can remove all occurrences of
boon in this paragraph.

>> Move the cursor inside the word 'boon' in the above paragraph
>> Type '\\[boon-take-region] \\<boon-select-map>\\[boon-select-occurences] \\[boon-select-wim] \\[boon-select-paragraph]\\<boon-command-map>'

If you have the `multiple-cursors' package installed, you can also
replace such occurrences.

>> Undo the previous command
>> Move the cursor inside the word 'boon' in the above paragraph
>> Type '\\[boon-substitute-region] \\<boon-select-map>\\[boon-select-occurences] \\[boon-select-wim] \\[boon-select-paragraph]\\<boon-command-map>'
>> Type replacement text
>> Exit multiple-cursors by typing <esc> repeatedly

Boon has special support for multiple cursors, which means that all
commands described in this tutorial should work out of the box with multiple
cursors.


* SPECIAL BUFFERS
-----------------

Certain Emacs mode already offer their \"own\" command system. Such
modes include 'dired', 'magit', and others. In such buffers, it makes
little sense to have an 'insert mode', and most edit commands do not
make sense either. Thus, boon switches to \"special mode\". In \"special
mode\", Boon overrides just the x key, which is bound to `boon-x-map'.

You can get the original behavior of x keys by prefixing it
with (\\<boon-special-map>\\[boon-quote-character]).


* COPYING
---------

This tutorial descends from a long line of Emacs tutorials
starting with the one written by Stuart Cracraft for the original Emacs.

This version of the tutorial is not a part of GNU Emacs, but derived
from the standard Emacs tutorial,


  Copyright (C) 1985, 1996, 1998, 2001-2013 Free Software Foundation,
  Inc.
")))))
(goto-char 1))

;;; boon-tutorial.el ends here
