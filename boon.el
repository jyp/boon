;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; fix swap-region
;; change helm-command-map to call stuff from the (overridden) helm-map 
;; implement helm command mode as a translation map
;;  h (help) C-c ?
;; 
;;
;; When 24.4 rolls out:
;;    use string-blank-p
;;    bind rectangle-mark-mode
;;    checkout electric-pair-mode options (http://www.masteringemacs.org/articles/2013/12/29/whats-new-in-emacs-24-4/)
;;
;; Enclosure " "textRET for arbitrary enclosures
;; Region specifier ' (former selected region)
;; Bind c to mode-specific things (instead of x c)
;; Command d should work on the selection if any
;; When inserting a newline; remove spaces before and after the point.
;; Support Fundamental buffers (see evil code)
;; Repeat "last command" (bind on return?)

;;; Code:

(require 'boon-keys)
(require 'boon-colemak)



