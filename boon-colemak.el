;;; boon-colemak.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(require 'boon-core)
(require 'boon-main)
(require 'boon-search)
(require 'boon-keys)

(define-key boon-select-map "q"  'boon-select-outside-quotes)
;; (define-key boon-select-map "w"  'boon-select-word)
(define-key boon-select-map "f"  'boon-select-word) ;; 'rf' is easier to type than 'rw'
(define-key boon-select-map "p"  'boon-select-paragraph)
(define-key boon-select-map "g"  'boon-select-org-table-cell)

(define-key boon-select-map "a"  'boon-select-borders) ;; Around
(define-key boon-select-map "r"  'boon-select-justline) ;; Row
(define-key boon-select-map "s"  'boon-select-wim) ;; symbol
(define-key boon-select-map "t"  'boon-select-with-spaces)
(define-key boon-select-map "d"  'boon-select-document)

(define-key boon-select-map "C"  'boon-select-comment)
(define-key boon-select-map "x"  'boon-select-outside-pairs) ;; eXpression
(define-key boon-select-map "c"  'boon-select-inside-pairs) ;; Contents
(define-key boon-select-map "v"  'boon-select-block)

(define-key boon-select-map "z"  'boon-select-content) ;; inZide

(define-key boon-select-map "'"  'boon-switch-mark)
(define-key boon-select-map "k"  'boon-select-blanks) ;; blanKs


(define-key boon-moves-map "k" 'boon-switch-mark) ; bacK to marK
(define-key boon-moves-map "K" 'pop-global-mark)

(define-key boon-moves-map "j"  'boon-find-definition)
(define-key boon-moves-map "J"  'find-function)
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
(define-key boon-moves-map "<"  'beginning-of-buffer)
(define-key boon-moves-map ">"  'end-of-buffer)
(define-key boon-moves-map "m"  'boon-qsearch-previous-at-point)
(define-key boon-moves-map "/"  'boon-qsearch-next-at-point)
(define-key boon-moves-map "h"  'avy-goto-word-1)
(define-key boon-moves-map "H"  'avy-goto-char)
;; (define-key boon-moves-map (kbd "C-,")      'beginning-of-buffer)
;; (define-key boon-moves-map (kbd "C-.")      'end-of-buffer)



(eval-after-load 'ivy
  '(progn
         (define-key ivy-minibuffer-map (kbd "RET") 'ivy-done)
         (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-call)
         ;; (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-alt-done)
         ;; (define-key ivy-minibuffer-map (kbd "C-M-j") 'ivy-immediate-done)
         (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-partial)
         (define-key ivy-minibuffer-map (kbd "C-y") 'ivy-next-line)
         (define-key ivy-minibuffer-map (kbd "C-u") 'ivy-previous-line)
         (define-key ivy-minibuffer-map (kbd "<down>") 'ivy-next-line)
         (define-key ivy-minibuffer-map (kbd "<up>") 'ivy-previous-line)
         ;; (define-key ivy-minibuffer-map (kbd "C-s") 'ivy-next-line-or-history)
         ;; (define-key ivy-minibuffer-map (kbd "C-r") 'ivy-reverse-i-search)
         (define-key ivy-minibuffer-map (kbd "SPC") 'boon-completer-space)
         (define-key ivy-minibuffer-map (kbd "DEL") 'ivy-backward-delete-char)
         (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-backward-kill-word)
         (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-delete-char)
         (define-key ivy-minibuffer-map (kbd "C-f") 'ivy-forward-char)
         ;; (define-key ivy-minibuffer-map (kbd "M-<") 'ivy-beginning-of-buffer)
         ;; (define-key ivy-minibuffer-map (kbd "M->") 'ivy-end-of-buffer)
         (define-key ivy-minibuffer-map (kbd "C-.") 'ivy-next-history-element)
         (define-key ivy-minibuffer-map (kbd "C-,") 'ivy-previous-history-element)
         (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
         ;; (define-key ivy-minibuffer-map (kbd "C-v") 'ivy-scroll-up-command)
         ;; (define-key ivy-minibuffer-map (kbd "M-v") 'ivy-scroll-down-command)
         ;; (define-key ivy-minibuffer-map (kbd "C-M-n") 'ivy-next-line-and-call)
         ;; (define-key ivy-minibuffer-map (kbd "C-M-p") 'ivy-previous-line-and-call)
         (define-key ivy-minibuffer-map (kbd "C-q") 'ivy-toggle-regexp-quote)
         ;; (define-key ivy-minibuffer-map (kbd "M-j") 'ivy-yank-word)
         (define-key ivy-minibuffer-map (kbd "C-v") 'ivy-insert-current)
         (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial) ;; for counsel-find-file
         (define-key ivy-minibuffer-map (kbd "C-TAB") 'ivy-dispatching-call)
         (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-kill-line)
         (define-key ivy-minibuffer-map (kbd "S-SPC") 'ivy-restrict-to-matches)
         (define-key ivy-minibuffer-map (kbd "C-t") 'ivy-kill-ring-save)
         ;; (define-key ivy-minibuffer-map (kbd "C-h") 'ivy-avy)
         ;; (define-key ivy-minibuffer-map (kbd "C-M-a") 'ivy-read-action)
         (define-key ivy-minibuffer-map (kbd "C-o") 'ivy-occur)
     )
)

(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "C-u")        (lambda () (interactive) (boon-helm-browse 'helm-previous-line)))
     (define-key helm-map (kbd "C-y")        (lambda () (interactive) (boon-helm-browse 'helm-next-line)))
     (define-key helm-map (kbd "C-,")        (lambda () (interactive) (boon-helm-browse 'helm-previous-page)))
     (define-key helm-map (kbd "C-.")        (lambda () (interactive) (boon-helm-browse 'helm-next-page)))
     ))

;; actions
(define-key boon-helm-command-map (kbd "j")   'helm-execute-persistent-action)
(define-key boon-helm-command-map (kbd "h")   'helm-select-2nd-action)
(define-key boon-helm-command-map (kbd "k")   'helm-select-3rd-action)

;; top row
(define-key boon-helm-command-map (kbd "q")   'helm-keyboard-quit)
(define-key boon-helm-command-map (kbd "f")   'helm-follow-mode)

;; home row: yanking/killing
;; (define-key boon-helm-command-map (kbd "r")   'helm-yank-selection)
;; (define-key boon-helm-command-map (kbd "s")   'next-history-element) ;; has the effect of getting the whole symbol at point
(define-key boon-helm-command-map (kbd "s")   'helm-yank-text-at-point)
(define-key boon-helm-command-map (kbd "d")   'helm-delete-minibuffer-contents)

(define-key boon-helm-command-map (kbd "y")   'helm-next-line)
(define-key boon-helm-command-map (kbd "u")   'helm-previous-line)
(define-key boon-helm-command-map (kbd "U")   'helm-previous-source)
(define-key boon-helm-command-map (kbd "Y")   'helm-next-source)
(define-key boon-helm-command-map (kbd ",")   'helm-previous-page)
(define-key boon-helm-command-map (kbd ".")   'helm-next-page)
(define-key boon-helm-command-map (kbd ">")   'helm-goto-next-file)
(define-key boon-helm-command-map (kbd "<")   'helm-goto-precedent-file)


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
(define-key boon-moves-map "fu"  'mc/cycle-forward)
(define-key boon-moves-map "wu"  'mc/cycle-backward)

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
(define-key boon-command-map "'" 'boon-toggle-mark)


(provide 'boon-colemak)
;;; boon-colemak.el ends here


(define-key indent-rigidly-map "i" 'indent-rigidly-right)
(define-key indent-rigidly-map "e" 'indent-rigidly-left)
