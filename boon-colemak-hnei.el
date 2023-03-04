;;; boon/boon-colemak-hnei.el --- An Ergonomic VIM-like Command Mode -*- lexical-binding: t; -*-

;;; Commentary:
;;; Boon-colemak remapped to be more Vim-like.
;;; Six of the eight movement keys (all except 'l' and ';') have been changed:
;;; 'u' = move backward by sentence
;;; 'y' = move forward by sentence
;;; 'H' = move backward smarter (by word, element, etc.)
;;; 'h' = move backward by character
;;; 'N' = move faster down (forward-paragraph)
;;; 'n' = move down by line (next-line)
;;; 'e' = move up by line (previous-line)
;;; 'E' = move faster up (backward-paragraph)
;;; 'i' = move forward by character
;;; 'I' = move forward smarter (by word, element, etc.)
;;; In addition, the move commands mapped to h/H have been moved to o/O.

;;; Code:
(require 'boon-colemak)

(define-key boon-moves-map "l"  '("⇤" . boon-beginning-of-line))
(define-key boon-moves-map "u" 'backward-sentence)
(define-key boon-moves-map "U" 'ignore)
(define-key boon-moves-map "y" 'forward-sentence)
(define-key boon-moves-map "Y" 'ignore)
(define-key boon-moves-map ";"  '("⇥" . boon-end-of-line))

(define-key boon-moves-map "H"  '("⇠" . boon-smarter-backward))        ;word
(define-key boon-moves-map "h"  '("←" . backward-char))                ;character
(define-key boon-moves-map "N"  'forward-paragraph)                    ;faster down
(define-key boon-moves-map "n"  '("↓" . next-line))                    ;down
(define-key boon-moves-map "e"  '("↑" . previous-line))                ;up
(define-key boon-moves-map "E"  'backward-paragraph)                   ;faster up
(define-key boon-moves-map "i"  '("→" . forward-char))                 ;character
(define-key boon-moves-map "I"  '("→" . boon-smarter-forward))         ;word

(define-key boon-moves-map "o"  '("hOp" . avy-goto-word-1))
(define-key boon-moves-map "O"  'avy-goto-char)

(provide 'boon-colemak-hnei)
;;; boon-colemak-hnei.el ends here
