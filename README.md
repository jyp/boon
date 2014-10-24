boon: Ergonomic Command Mode for Emacs
======================================

What is Boon?
-------------

Boon brings modal editing capabilities to emacs and...

- It tries to be as ergonomic as possible. (See below)
- It remains lightweight (~300 loc for its core.)
- It attempts to integrate with emacs as smoothly as possible


Design
------

It is largely accepted that modal edition is more ergonomic than using
keychoord.  Boon attempts to make modal editing as comfortable as
possible, by adhering to the following design principles:

- Spacial allocation first, mnemonics second: the allocation of keys
  to commands is based in priority on the locations of keys on the
  keyboard. Whatever is printed on the keycap is a secondary concern.

- Easy finger rolls: common combination should either be left/right
  hand alternation or easy one-hand rolls.

- Use of home row and strong fingers for the most used commands

- Easy navigation: many commands are bound to navigation. This
  facilitates moving around. Because movements double up as
  region-definitions, it makes manipulation commands (operators) more
  powerful.

In command mode, movement keys are bound to the right hand, while text
manipulation is bound to the left hand.


Right-hand.

The leftwards (and upwards) movements are bound to the leftmost
fingers (index and middle finger), while rightwards (and downwards)
movements are bound to the rightmost fingers (ring finger and pinky.)
Additional movements are bound to the middle column.

Left-hand.

The most common edition commands (cut, paste, parenthesis
manipulation) are bound to the home row. The top row is (mainly) for
searching. The bottom row gives access to regular emacs stuff (C-x
...) (C-c ...) and registers.

Installation/Configuration
--------------------------

REQUIREMENTS
- Emacs version >= 24.3
- Colemak layout

Install Boon (prehaps using
[![MELPA](http://melpa.org/packages/boon-badge.svg)](http://melpa.org/#/boon)),
and add the following to your configuration:

    (require 'boon-colemak) ;; qwerty mode not implemented (contributions welcome)
    (require 'boon-extras) ;; optional
    (boon-mode)

You can jump-start by reading the cheat sheet directly:

[![Boon cheat sheet](https://raw.githubusercontent.com/jyp/boon/master/cheat-sheet.svg)]

But reading through this modified (and shortened) version of the emacs tutorial is recommended:

[TUTORIAL](blob/master/TUTORIAL.txt)

Comparison with other modal layers for emacs
---------------------------------------------

As far as I know, none of the other modal mode care about ergonomics
(beside being modal).

- Evil

In boon, quite a bit of emacs structure and user experience is
retained. Examples: the x key gives the C-x prefix map.
Interactive arguments are used for text objects.

Besides, Emacs is already customizable enough as it is: the core of
Boon is just 200 lines or so. Figuring out all the ins and outs of
Evil to do what I want would probably require more effort.

- God-mode https://github.com/chrisdone/god-mode

Allows to retain most of emacs. It is similar to "sticky modifier
keys" in principle.

- Modal Mode http://retroj.net/modal-mode

Perhaps the work which is the closest to Boon.

- Modal Emacs https://github.com/joelmccracken/modal-emacs

Modal Emacs does not appear to be complete.
