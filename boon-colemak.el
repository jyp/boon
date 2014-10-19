;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(require 'boon-core)
(require 'boon-main)
(require 'boon-search)

(define-key boon-select-map "d"  'boon-select-document)
(define-key boon-select-map "p"  'boon-select-paragraph)
(define-key boon-select-map "w"  'boon-select-word)
(define-key boon-select-map "f"  'boon-select-word) ;; 'rf' is easier to type than 'rw'
(define-key boon-select-map "x"  'boon-select-outside-pairs) ;; eXpression
(define-key boon-select-map "c"  'boon-select-inside-pairs) ;; Contents
(define-key boon-select-map "s"  'boon-select-wim) ;; symbol
(define-key boon-select-map "q"  'boon-select-outside-quotes)
(define-key boon-select-map "'"  'boon-select-blanks) ;; blanKs
(define-key boon-select-map " "  'boon-select-line)
(define-key boon-select-map "r"  'boon-select-justline) ;; Ribbon
(define-key boon-select-map "a"  'boon-select-borders) ;; Around
(define-key boon-select-map "z"  'boon-select-content) ;; inZide

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
(define-key boon-moves-map "n"  'smarter-backward)
(define-key boon-moves-map "o"  'smarter-forward)
(define-key boon-moves-map "N"  'smarter-upward)
(define-key boon-moves-map "O"  'smarter-downward)
(define-key boon-moves-map ","  'boon-beginning-of-expression)
(define-key boon-moves-map "."  'boon-end-of-expression)
(define-key boon-moves-map "e"  'backward-char)
(define-key boon-moves-map "i"  'forward-char)
(define-key boon-moves-map "<"  'boon-beginning-of-region)
(define-key boon-moves-map ">"  'boon-end-of-region)

(setq boon-helm-command-map
      (let ((map (make-sparse-keymap)))
        (suppress-keymap map 't)
        ;; "w": widen selection (occur > multi-occur > git grep > ...)
        (define-key map (kbd "f")    'helm-follow-mode)

        (define-key map (kbd "r")    'helm-yank-selection)
        (define-key map (kbd "s")        'next-history-element) ;; has the effect of getting the whole symbol at point
        (define-key map (kbd "t")        'helm-yank-text-at-point)
        (define-key map (kbd "d")        'helm-delete-minibuffer-contents)

        (define-key map (kbd "z")        'helm-select-3rd-action)
        (define-key map (kbd "x")        'helm-select-2nd-action)
        (define-key map (kbd "c")        'helm-exit-minibuffer)
        (define-key map (kbd "v") 'boon-helm-set-insert-state)
        (define-key map (kbd "b")        'helm-execute-persistent-action)

        (define-key map (kbd "SPC") 'boon-helm-set-insert-state)
        (define-key map (kbd "<RET>")      'helm-exit-minibuffer)
        (define-key map (kbd "<tab>")        'helm-select-action)
        (define-key map (kbd "C-<RET>")        'helm-execute-persistent-action)

        (define-key map (kbd "K")        'helm-toggle-all-marks)
        (define-key map (kbd "M")        'helm-mark-all)
        ;; (define-key map (kbd "")        'helm-unmark-all) ;; use M K for this
        (define-key map (kbd "M-SPC")      'helm-toggle-visible-mark)
        (define-key map (kbd "'")      'helm-toggle-visible-mark)

        (define-key map (kbd "l")        'previous-history-element)
        (define-key map (kbd ";")        'next-history-element)
        (define-key map (kbd "y")     'helm-next-line)
        (define-key map (kbd "u")       'helm-previous-line)
        (define-key map (kbd "U")        'helm-previous-source)
        (define-key map (kbd "Y")        'helm-next-source)
        (define-key map (kbd ",")        'helm-previous-page)
        (define-key map (kbd ".")        'helm-next-page)
        (define-key map (kbd "C-y")      'helm-scroll-other-window)
        (define-key map (kbd "C-u")      'helm-scroll-other-window-down)
        (define-key map (kbd "m")        'helm-prev-visible-mark)
        (define-key map (kbd "/")        'helm-next-visible-mark)
        (define-key map (kbd ">")        'helm-goto-next-file)
        (define-key map (kbd "<")        'helm-goto-precedent-file)

        (define-key map (kbd "C-<down>")        'helm-narrow-window)
        (define-key map (kbd "C-<up>")        'helm-enlarge-window)
        (define-key map [(escape)] 'helm-keyboard-quit)
        map
        ))

(define-key isearch-mode-map [(control p)] 'helm-occur-from-isearch)
(define-key isearch-mode-map [(control w)] 'isearch-repeat-backward)
(define-key isearch-mode-map [(control f)] 'isearch-repeat-forward)


  (suppress-keymap boon-command-map 't)  ; so that typing is disabled altogether in command mode

  ;; Special keys
  (define-key boon-command-map [f1] 'helm-apropos)
  (define-key boon-command-map "\\" 'undefined)
  (define-key boon-command-map [(return)] 'undefined)
  (define-key boon-command-map (kbd "RET") 'undefined)
  (define-key boon-command-map [(backspace)] 'undefined)
  (define-key boon-command-map (kbd "DEL") 'undefined)
  (define-key boon-command-map "`" 'boon-toggle-case)
  (dolist (d '("M-0" "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9"
               "C-0" "C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
    (define-key boon-command-map (read-kbd-macro d) 'digit-argument))
  (define-key boon-command-map "_" 'redo)
  (define-key boon-command-map "-" 'undo)
  (define-key boon-command-map "\\" 'universal-argument)
  
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

  (define-key boon-command-map "fa" 'agda2-goto-definition-keyboard)
  

  ;; p
  ;; Pinpoint Place
  (define-key boon-command-map "p" 'helm-occur)

  ;; Misc crap
  (define-key boon-command-map "P" 'kmacro-end-or-call-macro) ; Play
  (define-key boon-command-map "X" 'boon-highlight-regexp)

  ;; g Go to
  (define-key boon-command-map "gg" 'helm-resume)
  (define-key boon-command-map "gf" 'helm-for-files) ;; see http://amitp.blogspot.se/2012/10/emacs-helm-for-finding-files.html
  (define-key boon-command-map "gl" 'goto-line)

  (define-key boon-command-map "gi" 'helm-git-grep)
  (define-key boon-command-map "gt" 'helm-etags-select)
  (define-key boon-command-map "gy" 'helm-flycheck)
  (define-key boon-command-map "gb" 'helm-buffers-list)
  (define-key boon-command-map "gm" 'helm-multi-occur)

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
  (define-key boon-command-map "d" 'boon-replace-character) ; "displace"

  ;; Bottom row
  ;; z
  ;; reserved (repeat?)
  ;; x
  (define-key boon-command-map "x" boon-x-map)
  ;; c
  (define-key boon-command-map "c" 'undefined)
  ;; v
  (define-key boon-command-map (kbd "C-v") 'boon-open-line-and-insert)
  (define-key boon-command-map "V" 'boon-open-next-line-and-insert)
  (define-key boon-command-map "v" 'boon-set-insert-like-state) ; 'v' looks like an insertion mark
  ;; b
  (define-key boon-command-map "B" 'boon-copy-to-register)
  (define-key boon-command-map "b" 'insert-register)
  
  ;; RIGHT HAND: movement and marking commands.
  ;; Most of these are actually in the boon-moves-map; however some don't quite work there; so they end up here.
  (define-key boon-command-map (kbd "C-u") 'scroll-up-line)
  (define-key boon-command-map (kbd "C-y") 'scroll-down-line)
  ;; (define-key boon-command-map "J" 'jump-to-register)
  ;; (define-key boon-command-map "j" 'ace-jump-line-mode) ; jump
  (define-key boon-command-map "h" 'ace-jump-word-mode) ; hop
  (define-key boon-command-map "H" 'ace-jump-char-mode) ; Hop
  (define-key boon-command-map "'" 'boon-toggle-mark)

  (define-key boon-command-map " " 'boon-drop-mark)
  (define-key boon-command-map [(escape)] 'boon-quit)

  (define-key boon-insert-map [remap newline] 'boon-newline-dwim)
  


(provide 'boon-colemak)
;;; boon-colemak.el ends here
