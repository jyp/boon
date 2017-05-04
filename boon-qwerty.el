;;; boon-qwerty.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(require 'boon)


(define-key boon-select-map "q"  'boon-select-outside-quotes)
(define-key boon-select-map "w"  'boon-select-word)
(define-key boon-select-map "g"  'boon-select-paragraph)

(define-key boon-select-map "a"  'boon-select-borders) ;; Around
(define-key boon-select-map "s"  'boon-select-wim) ;; symbol
(define-key boon-select-map "v"  'boon-select-with-spaces)
(define-key boon-select-map "d"  'boon-select-document)

(define-key boon-select-map "C"  'boon-select-comment)
(define-key boon-select-map "x"  'boon-select-outside-pairs) ;; eXpression
(define-key boon-select-map "c"  'boon-select-inside-pairs) ;; Contents

(define-key boon-select-map "z"  'boon-select-content) ;; inZide

(define-key boon-select-map "b"  'boon-select-blanks) ;; blanKs


(define-key boon-moves-map "n" '("noon walk" . boon-switch-mark))
(define-key boon-moves-map "N" 'xref-pop-marker-stack)

(define-key boon-moves-map "y"  '("fYnd" . xref-find-definitions))
(define-key boon-moves-map "Y"  'xref-find-references)
(define-key boon-moves-map "i"  'previous-line)
(define-key boon-moves-map "o"  'next-line)
(define-key boon-moves-map "I"  'backward-paragraph)
(define-key boon-moves-map "O"  'forward-paragraph)
(define-key boon-moves-map "u"  'boon-beginning-of-line)
(define-key boon-moves-map "p"  'boon-end-of-line)
(define-key boon-moves-map "j"  'boon-smarter-backward)
(define-key boon-moves-map ";"  'boon-smarter-forward)
(define-key boon-moves-map "K"  'boon-smarter-upward)
(define-key boon-moves-map "L"  'boon-smarter-downward)
(define-key boon-moves-map ","  'boon-beginning-of-expression)
(define-key boon-moves-map "."  'boon-end-of-expression)
(define-key boon-moves-map "k"  'backward-char)
(define-key boon-moves-map "l"  'forward-char)
(define-key boon-moves-map "<"  'beginning-of-buffer)
(define-key boon-moves-map ">"  'end-of-buffer)
(define-key boon-moves-map "h"  '("hop" . avy-goto-word-1))
(define-key boon-moves-map "H"  'avy-goto-char)



;; Special keys

;; LEFT HAND

;; Top row
;; q
(define-key boon-command-map "q" '("quote" . boon-quote-character))

;; w,e
;; where is? elsewhere?
(define-key boon-moves-map "w" '("where was?" . boon-backward-search-map))
(define-key boon-moves-map "e" '("elsewhere?" . boon-forward-search-map))

(define-key boon-moves-map "ww"  'boon-qsearch-previous)
(define-key boon-moves-map "ee"  'boon-qsearch-next)

(define-key boon-moves-map "W"  'boon-qsearch-previous)
(define-key boon-moves-map "E"  'boon-qsearch-next)

;; r
(define-key boon-command-map "r" '("occuR" . occur))
(define-key boon-command-map "R" 'kmacro-start-macro) ; Record

;; Misc crap
(define-key boon-command-map "P" 'kmacro-end-or-call-macro) ; Play
(define-key boon-command-map "X" 'boon-highlight-regexp)

;; t
(define-key boon-command-map "t" '("transform" . boon-replace-by-character))


;; home row
;; a
(define-key boon-command-map "a" '("around" . boon-enclose))

;; s
(define-key boon-command-map "s" '("substitute" . boon-substitute-region))

;; d
(define-key boon-command-map "d" '("delete" . boon-take-region)) ; "delete"
(define-key boon-command-map "D" 'boon-treasure-region) ; "duplicate"

;; f
(define-key boon-command-map "f" '("fetch" . boon-splice))
(define-key boon-command-map "F" 'yank-pop)

;; g
(define-key boon-command-map "g" '("goto" . boon-goto-map))

;; Bottom row
;; z
(define-key boon-command-map "z" '("repeat" . boon-repeat-command))
;; x
(define-key boon-command-map "x" 'boon-x-map)
;; c
(define-key boon-command-map "c" 'boon-c-god)
;; v
(define-key boon-command-map (kbd "C-v") 'boon-open-line-and-insert)
(define-key boon-command-map "V" 'boon-open-next-line-and-insert)
(define-key boon-command-map "v" '("v looks like an insert mark" . boon-set-insert-like-state))
;; b
(define-key boon-command-map "B" 'boon-copy-to-register) ; bank
(define-key boon-command-map "b" 'insert-register)

;; RIGHT HAND: movement and marking commands.
;; Most of these are actually in the boon-moves-map; however some don't quite work there; so they end up here.
(define-key boon-command-map (kbd "C-k") 'scroll-down-line)
(define-key boon-command-map (kbd "C-l") 'scroll-up-line)

(define-key indent-rigidly-map "k" 'indent-rigidly-right)
(define-key indent-rigidly-map "l" 'indent-rigidly-left)


(provide 'boon-qwerty)
;;; boon-qwerty.el ends here


