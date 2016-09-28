;;; boon-colemak.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(require 'boon)

(define-key boon-select-map "q"  'boon-select-outside-quotes)
(define-key boon-select-map "w"  'boon-select-word)
(define-key boon-select-map "f"  'boon-select-word) ;; 'rf' is easier to type than 'rw'
(define-key boon-select-map "p"  'boon-select-paragraph)
(define-key boon-select-map "g"  'boon-select-org-table-cell)

(define-key boon-select-map "a"  'boon-select-borders) ;; Around
(define-key boon-select-map "r"  'boon-select-justline) ;; Row
(define-key boon-select-map "s"  'boon-select-wim) ;; symbol
(define-key boon-select-map "t"  'boon-select-with-spaces)
(define-key boon-select-map "d"  'boon-select-document)
(define-key boon-select-map "b"  'boon-select-block)

(define-key boon-select-map "C"  'boon-select-comment)
(define-key boon-select-map "x"  'boon-select-outside-pairs) ;; eXpression
(define-key boon-select-map "c"  'boon-select-inside-pairs) ;; Contents

(define-key boon-select-map "z"  'boon-select-content) ;; inZide

(define-key boon-select-map "k"  'boon-select-blanks) ;; blanKs

(define-key boon-moves-map "k" 'boon-switch-mark) ; bacK to marK
(define-key boon-moves-map "K" 'xref-pop-marker-stack)

(define-key boon-moves-map "j"  'xref-find-definitions)
(define-key boon-moves-map "J"  'xref-find-references)
(define-key boon-moves-map "u"  'previous-line)
(define-key boon-moves-map "y"  'next-line)
(define-key boon-moves-map "U"  'backward-paragraph)
(define-key boon-moves-map "Y"  'forward-paragraph)
(define-key boon-moves-map "l"  'boon-beginning-of-line)
(define-key boon-moves-map ";"  'boon-end-of-line)
(define-key boon-moves-map "n"  'boon-smarter-backward)
(define-key boon-moves-map "o"  'boon-smarter-forward)
(define-key boon-moves-map "E"  'boon-smarter-upward)
(define-key boon-moves-map "I"  'boon-smarter-downward)
(define-key boon-moves-map ","  'boon-beginning-of-expression)
(define-key boon-moves-map "."  'boon-end-of-expression)
(define-key boon-moves-map "e"  'backward-char)
(define-key boon-moves-map "i"  'forward-char)
(define-key boon-moves-map "<"  'beginning-of-buffer)
(define-key boon-moves-map ">"  'end-of-buffer)
(define-key boon-moves-map "m"  'boon-qsearch-previous-at-point)
(define-key boon-moves-map "/"  'boon-qsearch-next-at-point)
(define-key boon-moves-map "h"  'avy-goto-word-1)
(define-key boon-moves-map "H"  'avy-goto-char)

;; Special keys

;; LEFT HAND

;; Top row
;; q
(define-key boon-command-map "q" 'boon-quote-character)

;; w,f
;; where is? find?
(define-key boon-moves-map "w "  'isearch-backward)
(define-key boon-moves-map "f "  'isearch-forward)

(define-key boon-moves-map "wt"  'boon-qsearch-previous-at-point)
(define-key boon-moves-map "ft"  'boon-qsearch-next-at-point)
(define-key boon-moves-map "ws"  'boon-qsearch-previous-at-point)
(define-key boon-moves-map "fs"  'boon-qsearch-next-at-point)

(define-key boon-moves-map "ww"  'boon-qsearch-previous)
(define-key boon-moves-map "ff"  'boon-qsearch-next)

(define-key boon-moves-map "W"  'boon-qsearch-previous)
(define-key boon-moves-map "F"  'boon-qsearch-next)
(define-key boon-moves-map "wp"  'boon-qsearch-previous)
(define-key boon-moves-map "fp"  'boon-qsearch-next)
(define-key boon-moves-map "we"  'previous-error)
(define-key boon-moves-map "fe"  'next-error)
(define-key boon-moves-map "wk"  'flycheck-previous-error)
(define-key boon-moves-map "fk"  'flycheck-next-error)
(define-key boon-moves-map "wb"  'previous-buffer)
(define-key boon-moves-map "fb"  'next-buffer)
(define-key boon-moves-map "fu"  'mc/cycle-forward)
(define-key boon-moves-map "wu"  'mc/cycle-backward)

;; p
;; Pinpoint Place
(define-key boon-command-map "p" 'occur)

;; Misc crap
(define-key boon-command-map "P" 'kmacro-end-or-call-macro) ; Play
(define-key boon-command-map "X" 'boon-highlight-regexp)

;; g Gather/Go To
(define-key boon-command-map "g" boon-goto-map)

;; home row
;; a
(define-key boon-command-map "a" 'boon-enclose) ; around

;; r
(define-key boon-command-map "r" 'boon-substitute-region) ; replace
(define-key boon-command-map "R" 'kmacro-start-macro) ; Record

;; s
(define-key boon-command-map "s" 'boon-splice) ; splice
(define-key boon-command-map "S" 'yank-pop)

;; t
(define-key boon-command-map "t" 'boon-take-region) ; "take"
(define-key boon-command-map "T" 'boon-treasure-region) ; "treasure"

;; d
(define-key boon-command-map "d" 'boon-replace-by-character) ; "displace"

;; Bottom row
;; z
;; reserved (repeat?)
;; x
(define-key boon-command-map "x" boon-x-map)
;; c
(define-key boon-command-map "c" 'boon-c-god)
;; v
(define-key boon-command-map (kbd "C-v") 'boon-open-line-and-insert)
(define-key boon-command-map "V" 'boon-open-next-line-and-insert)
(define-key boon-command-map "v" 'boon-set-insert-like-state) ; 'v' looks like an insertion mark
;; b
(define-key boon-command-map "B" 'boon-copy-to-register)
(define-key boon-command-map "b" 'insert-register)

;; RIGHT HAND: movement and marking commands.
;; Most of these are actually in the boon-moves-map; however some don't quite work there; so they end up here.
(define-key boon-command-map (kbd "C-u") 'scroll-down-line)
(define-key boon-command-map (kbd "C-y") 'scroll-up-line)

(define-key indent-rigidly-map "i" 'indent-rigidly-right)
(define-key indent-rigidly-map "e" 'indent-rigidly-left)

(provide 'boon-colemak)
;;; boon-colemak.el ends here


