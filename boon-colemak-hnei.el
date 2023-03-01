;;; boon/boon-colemak-hnei.el -*- lexical-binding: t; -*-
(require 'boon)

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
(define-key boon-moves-map "O"  'avy-goto-char))

(provide 'boon-colemak-hnei)
;;; boon-colemak-hnei.el ends here
