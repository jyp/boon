;;; boon/boon-qwerty-hjkl.el --- An Ergonomic VIM-like Command Mode -*- lexical-binding: t; -*-

;;; Commentary:
;;; Boon-qwerty remapped to be more Vim-like.
;;; Six of the eight movement keys (all except 'u' and 'p') have been changed:
;;; 'i' = move backward by sentence
;;; 'o' = move forward by sentence
;;; 'H' = move backward smarter (by word, element, etc.)
;;; 'h' = move backward by character
;;; 'J' = move faster down (forward-paragraph)
;;; 'j' = move down by line (next-line)
;;; 'k' = move up by line (previous-line)
;;; 'K' = move faster up (backward-paragraph)
;;; 'l' = move forward by character
;;; 'L' = move forward smarter (by word, element, etc.)
;;; In addition, the move commands mapped to h/H have been moved to n/N. ("hop" has become "near")
;;; The command mapped to n/N ("noon-walk") has been moved to m/M ("moon-walk")

;;; Code:
(require 'boon-qwerty)

(define-key boon-moves-map "u"  '("⇤" . boon-beginning-of-line))
(define-key boon-moves-map "i" 'backward-sentence)
(define-key boon-moves-map "I" 'ignore)
(define-key boon-moves-map "o" 'forward-sentence)
(define-key boon-moves-map "O" 'ignore)
(define-key boon-moves-map "p"  '("⇥" . boon-end-of-line))

(define-key boon-moves-map "H"  '("⇠" . boon-smarter-backward))        ;word
(define-key boon-moves-map "h"  '("←" . backward-char))                ;character
(define-key boon-moves-map "J"  'forward-paragraph)                    ;faster down
(define-key boon-moves-map "j"  '("↓" . next-line))                    ;down
(define-key boon-moves-map "k"  '("↑" . previous-line))                ;up
(define-key boon-moves-map "K"  'backward-paragraph)                   ;faster up
(define-key boon-moves-map "l"  '("→" . forward-char))                 ;character
(define-key boon-moves-map "L"  '("→" . boon-smarter-forward))         ;word

(define-key boon-moves-map "n"  '("near hop" . avy-goto-word-1))
(define-key boon-moves-map "N"  'avy-goto-char)
(define-key boon-moves-map "m" '("moon walk" . boon-switch-mark))
(define-key boon-moves-map "M" 'xref-pop-marker-stack)

(provide 'boon-qwerty-hjkl)
;;; boon-qwerty-hjkl ends here
