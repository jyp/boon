;; boon-emacs.el --- Emacs idiomatic key bindings for boon.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Bernd Rellermeyer

;; Author: Bernd Rellermeyer <bernd.rellermeyer@t-online.de>
;; Keywords: convenience
;; Version: 1.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file defines Emacs idiomatic or mnemonic key bindings for
;; boon.

;;; Code:

(require 'boon)

(define-key boon-select-map "q" 'boon-select-outside-quotes)
(define-key boon-select-map "w" 'boon-select-word)
(define-key boon-select-map "p" 'boon-select-paragraph)
(define-key boon-select-map "g" 'boon-select-org-table-cell)
(define-key boon-select-map "a" 'boon-select-borders)
(define-key boon-select-map "r" 'boon-select-justline)
(define-key boon-select-map "s" 'boon-select-wim)
(define-key boon-select-map "S" 'boon-select-sentence)
(define-key boon-select-map "v" 'boon-select-with-spaces)
(define-key boon-select-map "T" 'boon-select-org-tree)
(define-key boon-select-map "d" 'boon-select-document)
(define-key boon-select-map "z" 'boon-select-content)
(define-key boon-select-map "x" 'boon-select-outside-pairs)
(define-key boon-select-map "C" 'boon-select-comment)
(define-key boon-select-map "c" 'boon-select-inside-pairs)
(define-key boon-select-map "b" 'boon-select-block)
(define-key boon-select-map "k" 'boon-select-blanks)

(define-key boon-moves-map "@" 'boon-switch-mark)
(define-key boon-moves-map "," 'xref-pop-marker-stack)
(define-key boon-moves-map "." 'xref-find-definitions)
(define-key boon-moves-map "?" 'xref-find-references)
(define-key boon-moves-map "p" '("previous" . previous-line))
(define-key boon-moves-map "n" '("next" . next-line))
(define-key boon-moves-map "{" 'backward-paragraph)
(define-key boon-moves-map "}" 'forward-paragraph)
(define-key boon-moves-map "A" 'backward-sentence)
(define-key boon-moves-map "E" 'forward-sentence)
(define-key boon-moves-map "a" 'boon-beginning-of-line)
(define-key boon-moves-map "e" 'boon-end-of-line)
(define-key boon-moves-map "B" 'boon-smarter-backward)
(define-key boon-moves-map "F" 'boon-smarter-forward)
(define-key boon-moves-map "P" 'boon-smarter-upward)
(define-key boon-moves-map "N" 'boon-smarter-downward)
(define-key boon-moves-map "(" 'boon-beginning-of-expression)
(define-key boon-moves-map ")" 'boon-end-of-expression)
(define-key boon-moves-map "b" '("backward" . backward-char))
(define-key boon-moves-map "f" '("forward" . forward-char))
(define-key boon-moves-map "<" 'beginning-of-buffer)
(define-key boon-moves-map ">" 'end-of-buffer)
(define-key boon-moves-map "h" 'avy-goto-word-1)
(define-key boon-moves-map "H" 'avy-goto-char)
(define-key boon-moves-map "r" 'boon-backward-search-map)
(define-key boon-moves-map "s" 'boon-forward-search-map)
(define-key boon-moves-map "R" 'boon-qsearch-previous)
(define-key boon-moves-map "S" 'boon-qsearch-next)

(define-key boon-command-map "\\" 'boon-quote-character)
(define-key boon-command-map "/" 'occur)
(define-key boon-command-map "M" 'kmacro-start-macro)
(define-key boon-command-map "m" '("macro" . kmacro-end-or-call-macro))
(define-key boon-command-map "X" 'boon-highlight-regexp)
(define-key boon-command-map "t" '("transform" . boon-replace-by-character))
(define-key boon-command-map "T" 'boon-enclose)
(define-key boon-command-map "I" 'boon-substitute-region)
(define-key boon-command-map "w" 'boon-take-region)
(define-key boon-command-map "W" 'boon-treasure-region)
(define-key boon-command-map "y" '("yank" . boon-splice))
(define-key boon-command-map "Y" 'yank-pop)
(define-key boon-command-map "g" '("goto" . boon-goto-map))
(define-key boon-command-map "z" 'boon-repeat-command)
(define-key boon-command-map "x" 'boon-x-map)
(define-key boon-command-map "c" 'boon-c-god)
(define-key boon-command-map "O" 'boon-open-line-and-insert)
(define-key boon-command-map "o" '("open" . boon-open-next-line-and-insert))
(define-key boon-command-map "i" '("insert" . boon-set-insert-like-state))
(define-key boon-command-map "C" 'boon-copy-to-register)
(define-key boon-command-map "G" 'insert-register)
(define-key boon-command-map "v" 'scroll-up-command)
(define-key boon-command-map "V" 'scroll-down-command)
(define-key boon-command-map "j" 'scroll-up-line)
(define-key boon-command-map "k" 'scroll-down-line)
(define-key boon-command-map "q" '("quit" . quit-window))

(define-key indent-rigidly-map "k" 'indent-rigidly-right)
(define-key indent-rigidly-map "l" 'indent-rigidly-left)

(provide 'boon-emacs)

;;; boon-emacs.el ends here
