[![Join the chat at https://gitter.im/boon-mode/Lobby](https://badges.gitter.im/boon-mode/Lobby.svg)](https://gitter.im/boon-mode/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![MELPA](https://melpa.org/packages/boon-badge.svg)](https://melpa.org/#/boon)
[![MELPA-Stable](http://stable.melpa.org/packages/boon-badge.svg)](http://stable.melpa.org/#/boon)

Boon: An Ergonomic Command Mode for Emacs
==========================================

Boon is a complete package for modal editing, which is not Evil.

Strong points:

- Ergonomic: common commands are easy to type.
- Emacs-friendly: Emacs conventions are respected as much as
  compatible with design goals. This means that Boon integrates well
  with existing Emacs infrastructure, and leverages it. Existing user
  configuration can often be re-used.
- Modular: No need to buy into the whole system: mix-and-match the
  parts you want.

Ergonomic Design
----------------

It is largely accepted that modal editing is more ergonomic than using
key-chords.  Boon attempts to take this advantage to its conclusion,
making modal editing as comfortable as possible, by adhering to the
following design principles:

- Spacial allocation first, mnemonics second: the allocation of keys
  to commands is primarily based on the locations of keys on the
  keyboard. Whatever is printed on the key cap is a secondary concern.

- Easy finger rolls: common key combinations should either be
  left/right hand alternation or easy one-hand rolls.

- Use of home row and strong fingers for the most used commands.

- Easy navigation: many keys are allocated to navigation. This
  strategy facilitates moving around, which is in fact the most
  common task when editing text. Because movements double up as
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
Additional unpaired movements are bound to the middle column, which is
reached with an extension of the index finger.

Left-hand.

The most common editing commands (cut, paste, parenthesis
manipulation) are bound to the home row. The top row is (mainly) for
searching. The bottom row gives access to user-defined (C-x) and
mode-specific shortcuts (C-c), insert mode, and registers.

Emacs integration
-----------------

1. C-x and C-c shortcuts. Any 'C-x <sequence>' command is available
   via 'x <sequence>'. (drop the 1st "Control"). Thus 'x s' will save
   all buffers, etc. C-c C-<letter> shortcuts are now typed simply as
   "c <letter>". This means that no special configuration is required
   when you start using a new major or minor mode.

2. 'Special' modes. Emacs already has several modes which have a modal
   interface. (Dired, Magit, etc.) Instead of re-inventing the wheel,
   Boon leaves these modes alone (mostly). Only the 'x' key is stolen
   by Boon (so splitting windows, switching buffers, etc. keep their
   usual shortcut.)

3. Command repeats. Boon does not use its own mechanism to repeat
   commands: it simply uses the Emacs standard way. Commands which
   contain an insertion (eg. replace current word by something else)
   are properly recorded in Emacs command history. Emacs command
   history remains fully usable with Boon.

4. Parsing commands. Boon does not have a special way to parse
   commands. Everything is done using Emacs keymaps and interactive
   arguments.

5. Customize-friendly. Quick customization is easily done using M-x
   customize-group boon.

6. Multiple-cursors support. System of selectors supports
   multiple-cursors: (multiple regions will be returned when multiple
   cursors are active).

Modular design
--------------

Boon is designed as set of modules, largely independent of each
other. Each module is customizable and provides reusable components,
in full agreement with the Emacs spirit. This means that even if you
disagree with the design choices explained above, you may still want
to use some parts of Boon. The structure of Boon is as follows:

1. boon-moves.el and boon-search.el provide a set of move and search
   commands. These work the same way as standard Emacs commands ---
   they are merely more powerful (or just have different
   emphasis). Layout-frontends typically bind these commands (in
   addition to standard ones) in the boon keymaps.
2. boon-arguments.el provides a set of selectors to define
   regions. (These selectors are the equivalent of vim 'text
   objects'). Selectors include plain regions (words, lines,
   paragraphs, ...), but also region transformers (think: exclude
   borders, just borders, including spaces, "foreach",
   etc.). Additionally every move command (in the `boon-moves-map`
   keymap) can be used as a selector which means that they are easily
   customized.
3. boon-core.el provides an infrastructure for modal editing. The
   implementation draws much inspiration from evil-core, but is heavily
   simplified.
4. boon-main.el provides a set of commands. These are similar to
   standard Emacs commands, but they use the system of selectors
   described above. (For good measure, some random extra commands are
   thrown in.) These commands may be used in combination with a modal
   system or not. A few commands also switch to insert mode.
5. boon-keys.el, boon-colemak.el, boon-qwerty.el, etc. are
   (layout-specific) frontends. Those require all the above and
   provide a mapping of moves, selectors and commands onto keys.


Installation
------------

REQUIREMENTS
- Emacs version >= 25.1
- Qwerty, Qwertz or Colemak layout (Workman version partially
  implemented -- contributions welcome).

Install Boon (perhaps using
[![MELPA](http://stable.melpa.org/packages/boon-badge.svg)](http://stable.melpa.org/#/boon)),
and add the following to your configuration:

    (require 'boon-colemak)
    ;; (require 'boon-qwerty) ;; for qwerty variant

Then

    (boon-mode) ;; to enable boon everywhere

If you just eval'ed the above (or just did not want to enable boon
everywhere), Boon may not be active in the current buffer. If it is
not activated and you want to try it locally, activate it by

    M-x turn-on-boon-mode

Usage
-----

You can jump-start by reading any of the cheat sheets
([colemak](https://github.com/jyp/boon/blob/master/colemak.pdf),
[qwerty](https://github.com/jyp/boon/blob/master/qwerty.pdf))
directly, but reading through the tutorial is highly recommended:

    (require 'boon-tutorial)
    M-x boon-tutorial

(You'll get the version of the tutorial adapted to the layout-frontend
you have activated, qwerty, colemak, etc.)

Configuration
-------------

The main variables to configure are:

- boon-select-map, boon-moves-map, boon-command-map, boon-insert-map,
  boon-special-map. (Those are keymaps.)

- boon-enclosures (can be `custom`ized.)

If you use powerline (or *mutatis mutandis* spaceline), you may want
to:

    (require 'boon-powerline)
    (boon-powerline-theme) ;; if you want use powerline with Boon

- Per-mode keybinding can be implemented by setting the boon-map
  property of the mode symbol. Example:


     (let ((map (make-sparse-keymap)))
       (set-keymap-parent map boon-command-map)
       (define-key map "j" 'org-open-at-point-global)
       (put 'org-mode 'boon-map map))



Comparison with other modal layers for Emacs
---------------------------------------------

If possible I would have liked not to develop Boon (or parts of it)
and use an existing package. Hence I have looked into other options
and have documented my research below. In summary, even though I would
like to share code and/or replace parts of Boon with similar package,
at the moment no package offers enough that I feel the urge to try and
use them. This is partly due to the fact that Boon maintenance burden
is fairly low. (And at the time of inception, the only conceivable
alternative was Evil.)


- Evil

  Evil is a (quite) complete vi emulation layer for Emacs.

  In contrast, in Boon, much of Emacs structure is leveraged and user
  experience is retained. Examples: the `x` key gives the `C-x` prefix
  map.  The usual Emacs (interactive) arguments are used for text
  objects. Thus most of Boon remains usable even if one does not wish
  to use modal editing.

  Besides, the vi keybindings do not provide the best ergonomics
  (IMO).

  Making a boon-like layer on top of Evil appears theoretically
  possible, but I judge that the amount of work to gain an
  understanding of the code such that this would be practical would
  require more work than using Boon separately. Additionally, Emacs
  integration would still be poorer.

- Xah Fly Keys http://ergoemacs.org/misc/ergoemacs_vi_mode.html

  Like boon, Xah Fly Keys (hereafter abbreviated to XFK) aims at
  providing a layout whose design is ergonomic. (According to the
  author it constitutes "the most efficient editing system in the
  universe".)  Regardless, there follows a comparison based on my
  understanding.

  1. As far as I can tell XFK has no notion of selectors (vim's ``text
     objects''). Instead some keys are specially purposed to delete
     specific chunks of texts. Boon has a smaller set of useful
     commands and modifiers which can be combined in useful ways. It
     leaves keys for more "clever" navigation commands (eg. browsing
     errors) and editing shortcuts (eg. replace and insert in one
     keystroke).

  2. XFK binds digits to actions, boon leaves them for prefixes. (I
      may be misunderstandig here --- perhaps they are bound to
      special characters.)

  3. XFK seem to have less of a systematic assignment of keys to
     actions, even though the movements are roughly bound to the right
     hand. I believe that a more systematic binding startegy is easier
     to learn.

  4. Boon provides 1-key access to `C-x` and `C-c` prefixes. Instead,
     XFK puts everything under a single "leader key" (space),
     presumably without preserving emacs convention.

  5. The set of supported layouts is different. (Even though I'd
     expect ports to be easy.) As far as I can see, XFK has an
     automatic way to construct maps for a new keyboard layout. This
     is a worthy idea, but unfortunately boon already uses ad-hoc
     mapping depending on the layout, so adopting this strategy is not
     backwards compatible.

- Fingers https://github.com/fgeller/fingers.el

  *Fingers* borrows a few ideas from Boon, including the division of
  work between left and right hand. The author (fgeller) gives a
  detailed account of the particular differences with Boon. My opinion
  is that Fingers is compatible with Boon concepts and could (and
  probably should) be implemented as a Boon layout-frontend.

- Modalka https://github.com/mrkkrp/modalka

  Modalka is an engine to "introduce native modal editing of your own
  design". Thus its purpose is similar to `boon-core.el`. It could be
  possible and beneficial in the future to replace parts of boon-core
  with a dependency on Modalka. However at the moment it does not seem
  suitable. The main issue is that modalka does not support several
  states; it can only be either activated or not. Also, this part of
  Boon is sufficently simple that adding a dependency may be more
  troublesome than helpful.

- RYO modal mode https://github.com/Kungsgeten/ryo-modal

  RYO modal has the same purpose as Modalka and boon-core; it is also
  a candidate to replace `boon-core.el`, but still falls
  short. Compared to Modalka, it provides support for repeating a
  command, but . However boon can repeat insertion commands, while RYO
  modal cannot. Additionally it also suffers from having a single
  non-insertion state.

- God-mode https://github.com/chrisdone/god-mode

  God-mode is similar to "sticky modifier keys" in principle. Its
  simplicity allows to quickly get up to speed with it. However, it
  lacks the main benefit of a true modal layer: text operators. (what
  vi people call a "language for text editing"). Boon integrates
  god-mode functionality for the `C-c` prefix map specifically (bound to
  the `c` key).

- Modal Mode http://retroj.net/modal-mode (Last updated in 2014)

  Another modal layer for Emacs, which is also lightweight and aims to
  integrate with Emacs. However, as far as I can see, there is no
  special attention paid to ergonomics.


