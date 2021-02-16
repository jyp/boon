;;; boon-dvorak.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(require 'boon)

(define-key boon-select-map ";"  'boon-select-outside-quotes)
(define-key boon-select-map ","  'boon-select-word)
(define-key boon-select-map "i"  'boon-select-paragraph)

(define-key boon-select-map "a"  '("around" . boon-select-borders))
(define-key boon-select-map "o"  'boon-select-wim)
(define-key boon-select-map "k"  'boon-select-with-spaces)
(define-key boon-select-map "e"  '("everything" . boon-select-document))

(define-key boon-select-map "J"  'boon-select-comment)
(define-key boon-select-map "q"  'boon-select-outside-pairs)
(define-key boon-select-map "j"  'boon-select-inside-pairs)

(define-key boon-select-map "'"  'boon-select-content)

(define-key boon-select-map "x"  'boon-select-blanks)


(define-key boon-moves-map "b" 'boon-switch-mark)
(define-key boon-moves-map "B" 'xref-pop-marker-stack)

(define-key boon-moves-map "f"  '("find" . xref-find-definitions))
(define-key boon-moves-map "F"  'xref-find-references)
(define-key boon-moves-map "c"  'previous-line)
(define-key boon-moves-map "r"  'next-line)
(define-key boon-moves-map "C"  'backward-paragraph)
(define-key boon-moves-map "R"  'forward-paragraph)
(define-key boon-moves-map "g"  'boon-beginning-of-line)
(define-key boon-moves-map "l"  '("last" . boon-end-of-line))
(define-key boon-moves-map "h"  'boon-smarter-backward)
(define-key boon-moves-map "s"  'boon-smarter-forward)
(define-key boon-moves-map "T"  'boon-smarter-upward)
(define-key boon-moves-map "N"  'boon-smarter-downward)
(define-key boon-moves-map "w"  'boon-beginning-of-expression)
(define-key boon-moves-map "v"  'boon-end-of-expression)
(define-key boon-moves-map "t"  'backward-char)
(define-key boon-moves-map "n"  '("next" . forward-char))
(define-key boon-moves-map "W"  'beginning-of-buffer)
(define-key boon-moves-map "V"  'end-of-buffer)
(define-key boon-moves-map "d"  'avy-goto-word-1)
(define-key boon-moves-map "D"  'avy-goto-char)



;; Special keys

;; LEFT HAND

;; Top row
;; q
(define-key boon-command-map ";" 'boon-quote-character)

;; w,e
;; where is? elsewhere?
(define-key boon-moves-map "," 'boon-backward-search-map)
(define-key boon-moves-map "." 'boon-forward-search-map)

(define-key boon-moves-map "<"  'boon-qsearch-previous)
(define-key boon-moves-map ">"  'boon-qsearch-next)

;; r
(define-key boon-command-map "p" '("print" . occur)) ; print?
(define-key boon-command-map "P" 'kmacro-start-macro)

;; Misc crap
(define-key boon-command-map "L" '("pLay" . kmacro-end-or-call-macro))
(define-key boon-command-map "Q" 'boon-highlight-regexp)

;; t
(define-key boon-command-map "y" 'boon-replace-by-character)


;; home row
;; a
(define-key boon-command-map "a" '("around" . boon-enclose))

;; s
(define-key boon-command-map "o" 'boon-substitute-region)

;; d
(define-key boon-command-map "e" '("erase" . boon-take-region))
(define-key boon-command-map "E" 'boon-treasure-region)

;; f
(define-key boon-command-map "u" 'boon-splice)
(define-key boon-command-map "U" 'yank-pop)

;; g
(define-key boon-command-map "i" 'boon-goto-map)

;; Bottom row
;; z
(define-key boon-command-map "'" 'boon-repeat-command)
;; x
(define-key boon-command-map "q" 'boon-x-map)
;; c
(define-key boon-command-map "j" 'boon-c-god)
;; v
(define-key boon-command-map (kbd "C-k") 'boon-open-line-and-insert)
(define-key boon-command-map "K" 'boon-open-next-line-and-insert)
(define-key boon-command-map "k" 'boon-set-insert-like-state)
;; b
(define-key boon-command-map "X" 'boon-copy-to-register)
(define-key boon-command-map "x" 'insert-register)

;; RIGHT HAND: movement and marking commands.
;; Most of these are actually in the boon-moves-map; however some don't quite work there; so they end up here.
(define-key boon-command-map (kbd "C-t") 'scroll-down-line)
(define-key boon-command-map (kbd "C-n") 'scroll-up-line)

(define-key indent-rigidly-map "t" 'indent-rigidly-right)
(define-key indent-rigidly-map "n" 'indent-rigidly-left)

(provide 'boon-dvorak)
;;; boon-dvorak.el ends here
