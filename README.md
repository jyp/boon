[![Join the chat at https://gitter.im/boon-mode/Lobby](https://badges.gitter.im/boon-mode/Lobby.svg)](https://gitter.im/boon-mode/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Travis](https://travis-ci.org/jyp/boon.svg?branch=master)](https://travis-ci.org/jyp/boon)
[![MELPA](https://melpa.org/packages/boon-badge.svg)](https://melpa.org/#/boon)
[![MELPA-Stable](http://stable.melpa.org/packages/boon-badge.svg)](http://stable.melpa.org/#/boon)

Boon: An Ergonomic Command Mode for Emacs
==========================================


Boon is a complete package for modal editing, which is not Evil.

Selling points:
- Ergonomic: common commands are easy to type. (See below)
- Lightweight: ~300 loc for its core.
- Good Emacs integration: integrates well with existing Emacs
  infrastructure and takes advantage of it.


Ergonomic Design
----------------

It is largely accepted that modal edition is more ergonomic than using
key-chords.  Boon attempts to make modal editing as comfortable as
possible, by adhering to the following design principles:

- Spacial allocation first, mnemonics second: the allocation of keys
  to commands is primarily based on the locations of keys on the
  keyboard. Whatever is printed on the key cap is a secondary concern.

- Easy finger rolls: common key combinations should either be
  left/right hand alternation or easy one-hand rolls.

- Use of home row and strong fingers for the most used commands

- Easy navigation: many commands are bound to navigation. This
  allocation of keys facilitates moving around, which is the most
  common task when editting text. Because movements double up as
  region-definitions, this design also makes manipulation commands
  more powerful.

- Prefer an easy two-keystroke combination to a single hard-to-reach
  key. Hard-to-reach keys are free for the user to bind to rarely used
  commands (often user and mode-dependenent).

In command mode, movement keys are bound to the right hand, while text
manipulation is bound to the left hand. This division of tasks is
reminiscent of game-console controllers.


Right-hand.

The leftwards (and upwards) movements are bound to the leftmost
fingers (index and middle finger), while rightwards (and downwards)
movements are bound to the rightmost fingers (ring finger and pinky.)
Additional unpaired, movements are bound to the middle column
(extended reach with index).

Left-hand.

The most common edition commands (cut, paste, parenthesis
manipulation) are bound to the home row. The top row is (mainly) for
searching. The bottom row gives access to regular Emacs stuff (C-x
...) (C-c ...), insert mode, and registers.


Emacs Integration: Reusable Modules
-----------------------------------

Boon is designed as set of modules, largely independent of each
other. Each module is customizable and provides reusable components,
in full agreement with the Emacs spirit. This means that even if you
disagree with the design choices explained above, you may still want
to use some parts of Boon. The structure of Boon is as follows:

1. boon-moves.el and boon-search.el provide a set of move and search
   commands. These work the same way as standard Emacs commands ---
   they are merely more powerful (or just have different
   emphasis). Frontends typically bind these commands (in addition to
   standard ones) in the boon keymaps.
2. boon-arguments.el provides a set of selectors to define
   regions. (These selectors are the equivalent of vim 'text
   objects'). Selectors include plain regions (words, lines,
   paragraphs, ...), but also region transformers (think: exclude
   borders, just borders, including spaces, each, etc.). Additionally
   every move command (in the boon-moves-map keymap) can be used as a
   selector which means that they are easily customized. On top of it
   all, the system supports multiple-cursors (multiple regions will be
   returned when multiple cursors are active).
3. boon-core.el provides an infrastructure for modal editing. The
   implementation is very much inspired from evil-core, but heavily
   simplified.
4. boon-main.el provides a set of commands. These are similar to
   standard Emacs commands, but they use the system of selectors
   described above. (For good measure, some random extra commands are
   thrown in.) These commands may be used in combination with a modal
   system or not. A few commands also switch to insert mode.
5. boon-keys.el, boon-colemak.el, boon-qwerty.el are frontends. Those
   require all the above and provide a mapping of moves, selectors and
   commands onto keys.

Installation
------------

REQUIREMENTS
- Emacs version >= 25.1
- Qwerty or Colemak layout (workman version partially implemented).

Install Boon (perhaps using
[![MELPA](http://stable.melpa.org/packages/boon-badge.svg)](http://stable.melpa.org/#/boon)),
and add the following to your configuration:

    (require 'boon-colemak)
    ;; (require 'boon-qwerty) ;; for qwerty port

Then

    (boon-mode) ;; to enable boon everywhere

If you just eval'ed the above (or just did not want to enable boon
everywhere), Boon may not be active in the current buffer. If it is
not activated and you want to try it locally, activate it by

    M-x turn-on-boon-mode

Usage
-----

You can jump-start by reading the cheat sheet
([colemak](https://github.com/jyp/boon/blob/master/colemak.pdf),
[qwerty](https://github.com/jyp/boon/blob/master/qwerty.pdf))
directly, but reading through the tutorial is highly recommended:

    M-x boon-tutorial

(You'll get the version of the tutorial adapted to the frontend you
have activated, qwerty or colemak.)

Configuration
-------------

The main variables to configure are:

- boon-select-map, boon-moves-map, boon-command-map. (Those are keymaps.)
- boon-enclosures (can be `custom`ized.)

If you use powerline (or *mutatis mutandis* spaceline), you may want
to:

    (require 'boon-powerline)
    (boon-powerline-theme) ;; if you want use powerline with Boon



Comparison with other modal layers for Emacs
---------------------------------------------

- Evil

  Evil is a (quite) complete vi emulation layer for Emacs.

  In contrast, in Boon, much of Emacs structure is leveraged and user
  experience is retained. Examples: the `x` key gives the `C-x` prefix
  map.  The usual Emacs (interactive) arguments are used for text
  objects. Thus most of Boon remains usable even if one does not wish
  to use modal editing.

  Besides, Evil use vi bindings (by default at least), which do not
  provide the best ergonomics.

- Xah Fly Keys http://ergoemacs.org/misc/ergoemacs_vi_mode.html

  Like boon, Xah Fly Keys aims at providing a layout whose design is
  ergonomic. As far as I understand it follows the spirit of Xah's
  ErgoEmacs package. As I understand ErgoEmacs makes most design
  decisions differently from boon. I have not made an in-depth
  comparison of ergonomics yet.

- Fingers https://github.com/fgeller/fingers.el

  Fingers borrows a few ideas from Boon, including the division of
  work between left and right hand. fgeller gives a detailed account
  of the particular differences with Boon. My opinion is that Fingers
  is compatible with Boon concepts and could (and probably should) be
  implemented as a Boon 'frontend'.

- Modalka https://github.com/mrkkrp/modalka

  Modalka is an engine to "introduce native modal editing of your own
  design". Thus its purpose is similar to `boon-core.el`. It could be
  possible in the future to replace parts of boon-core with a
  dependency on Modalka. However at the moment it does not seem
  suitable. The main issue is that modalka does not support several
  states; it can only be either activated or not.

- RYO modal mode https://github.com/Kungsgeten/ryo-modal

  RYO modal has the same purpose as Modalka and boon-core. Compared to
  Modalka, it provides support for repeating a command. However RYO
  modal's method is incompatible with Emacs' repeat (and consequently
  also with boon complex commands). Additionally it suffers from the
  same drawback as Modalka: it has a single non-insertion state.

- God-mode https://github.com/chrisdone/god-mode

  God-mode is similar to "sticky modifier keys" in principle. Its
  simplicity allows to quickly get up to speed with it. However, it
  lacks the main benefit of a true modal layer: text operators. (what
  vi people call a "language for text edition"). Boon integrates basic
  god-mode functionality (bound to the C key).

- Modal Mode http://retroj.net/modal-mode (Last updated in 2014)

  Another modal layer for Emacs, which is also lightweight and aims to
  integrate with Emacs. However, as far as I can see, there is no
  special attention paid to ergonomics.
