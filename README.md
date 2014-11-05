Boon: An Ergonomic Command Mode for Emacs
==========================================

Boon is another package for modal editing.

Selling points:
- Ergonomic: common commands are easy to type. (See below)
- Lightweight: ~300 loc for its core.
- Good emacs integration: takes advantage of, and leverages existing
  emacs infrastructure instead of re-inventing the wheel.


Design
------

It is largely accepted that modal edition is more ergonomic than using
keychord.  Boon attempts to make modal editing as comfortable as
possible, by adhering to the following design principles:

- Spacial allocation first, mnemonics second: the allocation of keys
  to commands is based in priority on the locations of keys on the
  keyboard. Whatever is printed on the key cap is a secondary concern.

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
searching. The bottom row gives access to regular Emacs stuff (C-x
...) (C-c ...) and registers.

Installation/Configuration
--------------------------

REQUIREMENTS
- Emacs version >= 24.3
- Colemak layout

Install Boon (perhaps using
[![MELPA](http://stable.melpa.org/packages/boon-badge.svg)](http://stable.melpa.org/#/boon)),
and add the following to your configuration:

    (require 'boon-colemak) ;; qwerty mode not implemented (contributions welcome)
    (require 'boon-extras) ;; optional
    (boon-mode) ;; enable boon everywhere (use turn-on-boon-mode) to try locally

You can jump-start by reading the
[cheat sheet](https://pdf.yt/d/hSKUThNNSxrNFXkQ) directly, but reading
through this modified (and shortened) version of the Emacs tutorial is
recommended:

[TUTORIAL.txt](TUTORIAL.txt)

Comparison with other modal layers for Emacs
---------------------------------------------

As far as I know, none of the other modal mode care about ergonomics
(beside being modal).

- Evil

  Evil is a (quite) complete vi emulation layer for Emacs.

  In boon, quite a bit of Emacs structure and user experience is
  retained. Examples: the x key gives the C-x prefix map.
  Interactive arguments are used for text objects.

  Besides, Emacs is already customizable enough as it is: the core of
  Boon is just 200 lines or so. Figuring out all the ins and outs of
  Evil to do what I want would probably require more effort.

- God-mode https://github.com/chrisdone/god-mode

  God-mode is similar to "sticky modifier keys" in principle. Its
  simplicity allows to quickly get up to speed with it. However, it
  lacks the main benefit of a true modal layer: text operators. (what
  vi fans call a "language for text edition").

- Modal Mode http://retroj.net/modal-mode

  Perhaps the work which is the closest to Boon in principle
  (lightweight and integration with Emacs). However, as far as I can
  see, there is no special attention to ergonomics.

- Modal Emacs https://github.com/joelmccracken/modal-emacs

  Modal Emacs does not appear to be complete.
