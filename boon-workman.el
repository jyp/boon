;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:
;; fgeller's workman port (WIP, contributions welcome)
;;; Code:
(require 'boon)

(define-key boon-select-map (kbd "g")  'boon-select-document)
(define-key boon-select-map (kbd "w")  'boon-select-paragraph)
(define-key boon-select-map (kbd "d")  'boon-select-word)
(define-key boon-select-map (kbd "r")  'boon-select-word) ;; 'rf' is easier to type than 'rw'
(define-key boon-select-map (kbd "x")  'boon-select-outside-pairs) ;; eXpression
(define-key boon-select-map (kbd "m")  'boon-select-inside-pairs) ;; Contents
(define-key boon-select-map (kbd "h")  'boon-select-wim) ;; symbol
(define-key boon-select-map (kbd "q")  'boon-select-outside-quotes)
(define-key boon-select-map (kbd "SPC")  'boon-select-line)
(define-key boon-select-map (kbd "s")  'boon-select-justline) ;; Row
(define-key boon-select-map (kbd "a")  'boon-select-borders) ;; Around
(define-key boon-select-map (kbd "z")  'boon-select-content) ;; inZide

(define-key boon-moves-map (kbd "k") 'boon-switch-mark) ; bacK to marK
(define-key boon-moves-map (kbd "K") 'boon-switch-mark-quick) ; quicK bacK to marK

(define-key boon-moves-map (kbd "j")  'boon-find-char-backward)
(define-key boon-moves-map (kbd "J")  'boon-find-char-forward)
(define-key boon-moves-map (kbd "u")  'previous-line)
(define-key boon-moves-map (kbd "p")  'next-line)
(define-key boon-moves-map (kbd "U")  'backward-paragraph)
(define-key boon-moves-map (kbd "P")  'forward-paragraph)
(define-key boon-moves-map (kbd "f")  'boon-beginning-of-line)
(define-key boon-moves-map (kbd ";")  'boon-end-of-line)
(define-key boon-moves-map (kbd "n")  'boon-smarter-backward)
(define-key boon-moves-map (kbd "i")  'boon-smarter-forward)
(define-key boon-moves-map (kbd "N")  'boon-smarter-upward)
(define-key boon-moves-map (kbd "I")  'boon-smarter-downward)
(define-key boon-moves-map (kbd ",")  'boon-beginning-of-expression)
(define-key boon-moves-map (kbd ".")  'boon-end-of-expression)
(define-key boon-moves-map (kbd "e")  'backward-char)
(define-key boon-moves-map (kbd "o")  'forward-char)
(define-key boon-moves-map (kbd "<")  'beginning-of-buffer)
(define-key boon-moves-map (kbd ">")  'end-of-buffer)
(define-key boon-moves-map (kbd "C-,")      'beginning-of-buffer)
(define-key boon-moves-map (kbd "C-.")      'end-of-buffer)


;; Special keys

;; LEFT HAND

;; Top row
;; q
(define-key boon-command-map (kbd "q") 'boon-quote-character)

;; w,f
;; where is? find?
(define-key boon-moves-map "d "  'isearch-backward)
(define-key boon-moves-map "r "  'isearch-forward)

(define-key boon-moves-map (kbd "dt")  'boon-qsearch-previous-at-point)
(define-key boon-moves-map (kbd "rt")  'boon-qsearch-next-at-point)
(define-key boon-moves-map (kbd "ds")  'boon-qsearch-previous-at-point)
(define-key boon-moves-map (kbd "rs")  'boon-qsearch-next-at-point)

(define-key boon-moves-map (kbd "dd")  'boon-qsearch-previous)
(define-key boon-moves-map (kbd "rr")  'boon-qsearch-next)

(define-key boon-moves-map (kbd "D")  'boon-qsearch-previous)
(define-key boon-moves-map (kbd "R")  'boon-qsearch-next)
(define-key boon-moves-map (kbd "dw")  'boon-qsearch-previous)
(define-key boon-moves-map (kbd "rw")  'boon-qsearch-next)
(define-key boon-moves-map (kbd "de")  'previous-error)
(define-key boon-moves-map (kbd "re")  'next-error)
(define-key boon-moves-map (kbd "dk")  'flycheck-previous-error)
(define-key boon-moves-map (kbd "rk")  'flycheck-next-error)
(define-key boon-moves-map (kbd "db")  'previous-buffer)
(define-key boon-moves-map (kbd "rb")  'next-buffer)

;; p
;; Pinpoint Place
(define-key boon-command-map (kbd "w") 'occur)

;; Misc crap
(define-key boon-command-map (kbd "W") 'kmacro-end-or-call-macro) ; Play
(define-key boon-command-map (kbd "X") 'boon-highlight-regexp)

;; g Gather/Go To
(define-key boon-command-map (kbd "b") boon-goto-map)

;; home row
;; a
(define-key boon-command-map (kbd "a") 'boon-enclose) ; around
(define-key boon-command-map (kbd "A") 'boon-swap-region) ; swap

;; r
(define-key boon-command-map (kbd "s") 'boon-substitute-region) ; replace
(define-key boon-command-map (kbd "S") 'kmacro-start-macro) ; Record

;; s
(define-key boon-command-map (kbd "h") 'boon-splice) ; splice
(define-key boon-command-map (kbd "h") 'yank-pop)

;; t
(define-key boon-command-map (kbd "t") 'boon-take-region) ; "take"
(define-key boon-command-map (kbd "T") 'boon-treasure-region) ; "treasure"

;; d
(define-key boon-command-map (kbd "g") 'boon-replace-by-character) ; "displace"

;; Bottom row
;; z
;; reserved (repeat?)
(define-key boon-command-map (kbd "z") 'undefined)
;; x
(define-key boon-command-map (kbd "x") boon-x-map)
;; c
(define-key boon-command-map (kbd "m") 'boon-c-god)
;; v
(define-key boon-command-map (kbd "C-c") 'boon-open-line-and-insert)
(define-key boon-command-map (kbd "C") 'boon-open-next-line-and-insert)
(define-key boon-command-map (kbd "c") 'boon-set-insert-like-state) ; 'v' looks like an insertion mark
;; b
(define-key boon-command-map (kbd "V") 'boon-copy-to-register)
(define-key boon-command-map (kbd "v") 'insert-register)

;; RIGHT HAND: movement and marking commands.
;; Most of these are actually in the boon-moves-map; however some don't quite work there; so they end up here.
(define-key boon-command-map (kbd "C-u") 'scroll-up-line)
(define-key boon-command-map (kbd "C-p") 'scroll-down-line)
(define-key boon-command-map (kbd "y") 'ace-jump-word-mode) ; hop
(define-key boon-command-map (kbd "Y") 'ace-jump-char-mode) ; Hop
(define-key boon-command-map (kbd "'") 'boon-toggle-mark)

(provide 'boon-workman)
;;; boon-workman.el ends here
