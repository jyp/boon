;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(require 'boon-core)
(require 'boon-main)
(require 'boon-search)
(require 'boon-keys)

(define-key boon-select-map "c"  'boon-select-comment)
(define-key boon-select-map "d"  'boon-select-document)
(define-key boon-select-map "p"  'boon-select-paragraph)
(define-key boon-select-map "w"  'boon-select-word)
(define-key boon-select-map "f"  'boon-select-word) ;; 'rf' is easier to type than 'rw'
(define-key boon-select-map "x"  'boon-select-outside-pairs) ;; eXpression
(define-key boon-select-map "c"  'boon-select-inside-pairs) ;; Contents
(define-key boon-select-map "s"  'boon-select-wim) ;; symbol
(define-key boon-select-map "q"  'boon-select-outside-quotes)
(define-key boon-select-map " "  'boon-select-line)
(define-key boon-select-map "r"  'boon-select-justline) ;; Row
(define-key boon-select-map "a"  'boon-select-borders) ;; Around
(define-key boon-select-map "z"  'boon-select-content) ;; inZide

(define-key boon-select-map "'"  'boon-switch-mark)
(define-key boon-select-map "k"  'boon-select-blanks) ;; blanKs


(define-key boon-moves-map "k" 'boon-switch-mark) ; bacK to marK
(define-key boon-moves-map "K" 'boon-switch-mark-quick) ; quicK bacK to marK

(define-key boon-moves-map "j"  'boon-find-char-backward)
(define-key boon-moves-map "J"  'boon-find-char-forward)
(define-key boon-moves-map "u"  'previous-line)
(define-key boon-moves-map "y"  'next-line)
(define-key boon-moves-map "U"  'backward-paragraph)
(define-key boon-moves-map "Y"  'forward-paragraph)
(define-key boon-moves-map "l"  'boon-beginning-of-line)
(define-key boon-moves-map ";"  'boon-end-of-line)
(define-key boon-moves-map "n"  'boon-smarter-backward)
(define-key boon-moves-map "o"  'boon-smarter-forward)
(define-key boon-moves-map "N"  'boon-smarter-upward)
(define-key boon-moves-map "O"  'boon-smarter-downward)
(define-key boon-moves-map ","  'boon-beginning-of-expression)
(define-key boon-moves-map "."  'boon-end-of-expression)
(define-key boon-moves-map "e"  'backward-char)
(define-key boon-moves-map "i"  'forward-char)
(define-key boon-moves-map "<"  'boon-beginning-of-region)
(define-key boon-moves-map ">"  'boon-end-of-region)
(define-key boon-moves-map "m"  'boon-qsearch-previous-at-point)
(define-key boon-moves-map "/"  'boon-qsearch-next-at-point)
(define-key boon-moves-map "h"  'avy-goto-word-1)
(define-key boon-moves-map "H"  'avy-goto-word-1)
(define-key boon-moves-map (kbd "C-,")      'beginning-of-buffer)
(define-key boon-moves-map (kbd "C-.")      'end-of-buffer)

(define-key boon-helm-command-map (kbd "q")    'helm-keyboard-quit)
(define-key boon-helm-command-map (kbd "f")    'helm-follow-mode)
(define-key boon-helm-command-map (kbd "p")    'previous-history-element)

;; home row: yanking/killing
(define-key boon-helm-command-map (kbd "r")    'helm-yank-selection)
(define-key boon-helm-command-map (kbd "s")        'next-history-element) ;; has the effect of getting the whole symbol at point
(define-key boon-helm-command-map (kbd "t")        'helm-yank-text-at-point)
(define-key boon-helm-command-map (kbd "d")        'helm-delete-minibuffer-contents)

;; bottom row: actions
(define-key boon-helm-command-map (kbd "z")        'helm-select-3rd-action)
(define-key boon-helm-command-map (kbd "x")        'helm-select-2nd-action)
(define-key boon-helm-command-map (kbd "c")        'boon-c-map)
(define-key boon-helm-command-map (kbd "v")        'boon-helm-set-insert-state) ;; for consistency
(define-key boon-helm-command-map (kbd "b")        'helm-execute-persistent-action)

(define-key boon-helm-command-map (kbd "'")        'helm-toggle-all-marks)
(define-key boon-helm-command-map (kbd "K")        'helm-mark-all)
(define-key boon-helm-command-map (kbd "C-k")        'helm-unmark-all) ;; use K ' for this
(define-key boon-helm-command-map (kbd "k")      'helm-toggle-visible-mark)

(define-key boon-helm-command-map (kbd "l")        'previous-history-element)
(define-key boon-helm-command-map (kbd ";")        'next-history-element)
(define-key boon-helm-command-map (kbd "y")     'helm-next-line)
(define-key boon-helm-command-map (kbd "u")       'helm-previous-line)
(define-key boon-helm-command-map (kbd "U")        'helm-previous-source)
(define-key boon-helm-command-map (kbd "Y")        'helm-next-source)
(define-key boon-helm-command-map (kbd ",")        'helm-previous-page)
(define-key boon-helm-command-map (kbd ".")        'helm-next-page)
(define-key boon-helm-command-map (kbd "C-y")      'helm-scroll-other-window)
(define-key boon-helm-command-map (kbd "C-u")      'helm-scroll-other-window-down)
(define-key boon-helm-command-map (kbd ",")        'helm-prev-visible-mark)
(define-key boon-helm-command-map (kbd ".")        'helm-next-visible-mark)
(define-key boon-helm-command-map (kbd ">")        'helm-goto-next-file)
(define-key boon-helm-command-map (kbd "<")        'helm-goto-precedent-file)


(define-key isearch-mode-map [(control p)] 'helm-occur-from-isearch)
(define-key isearch-mode-map [(control w)] 'isearch-repeat-backward)
(define-key isearch-mode-map [(control f)] 'isearch-repeat-forward)

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

;; p
;; Pinpoint Place
(define-key boon-command-map "p" 'helm-occur)

;; Misc crap
(define-key boon-command-map "P" 'kmacro-end-or-call-macro) ; Play
(define-key boon-command-map "X" 'boon-highlight-regexp)

;; g Gather/Go To
(define-key boon-command-map "g" boon-goto-map)

;; home row
;; a
(define-key boon-command-map "a" 'boon-enclose) ; around
(define-key boon-command-map "A" 'boon-swap-region) ; swap

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
(define-key boon-command-map "z" 'boon-drop-cursor)
(define-key boon-command-map "Z" 'boon-move-cursor)
;; x
(define-key boon-command-map "x" boon-x-map)
;; c
(define-key boon-command-map "c" boon-c-map)
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
(define-key boon-command-map "h" 'avy-goto-word-1) ; hop
(define-key boon-command-map "H" 'avy-goto-char) ; Hop
(define-key boon-command-map "'" 'boon-toggle-mark)

(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "C-l")        'previous-history-element)
     (define-key helm-map (kbd "C-;")        'next-history-element)
     (define-key helm-map (kbd "C-u")        'helm-previous-line)
     (define-key helm-map (kbd "C-y")        'helm-next-line)
     (define-key helm-map (kbd "C-,")        'helm-previous-page)
     (define-key helm-map (kbd "C-.")        'helm-next-page)
     ))
(provide 'boon-colemak)
;;; boon-colemak.el ends here


(define-key indent-rigidly-map "i" 'indent-rigidly-right)
(define-key indent-rigidly-map "e" 'indent-rigidly-left)
