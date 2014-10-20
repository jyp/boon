boon: Ergonomic Command Mode for Emacs
======================================

What is Boon?
-------------

Boon is an attempt to add a modal layer to emacs, using a fresh
design.  While experience is drawn from vi (and its incarnations as
emacs extensions), Boon attempt to provide better ergonomics.


Boon design
-----------

Boon attempt to make text edition as comfortable as possible.v
This means to build a command mode.


Configuration
-------------

Install Boon (prehaps using MELPA), and add the following to your configuration:

(require 'boon-colemak)
(require 'boon-extras) ;; optional
(boon-mode)


FAQ
---

- Why the name Boon?

This is a pun over Evil. At first I chose the name "ergonomic mode and
command system", but it was bound to generate even more confusion.

- Why not customize Evil?

I want something which is more native to Emacs.

Quite a bit of emacs structure and user experience is
retained. Example: the x key gives the C-x prefix map.

Besides, Emacs is
already customizable enough as it is: the core of Boon is just 200
lines or so. Figuring out all the ins and outs of Evil to do what I
want would probably require more effort.

