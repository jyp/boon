;;; boon-keys.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module defines various keymaps and portions of keymaps, common
;; to all keyboard layouts.

;;; Code:

(require 'boon-core)

(define-prefix-command 'boon-backward-search-map)
(define-prefix-command 'boon-forward-search-map)

(define-key boon-forward-search-map " "  'isearch-forward)
(define-key boon-forward-search-map "t"  'boon-qsearch-next-at-point)
(define-key boon-forward-search-map "s"  'boon-qsearch-next-at-point)
(define-key boon-forward-search-map "p"  'boon-qsearch-next)
(define-key boon-forward-search-map "e"  'next-error)
(define-key boon-forward-search-map "k"  'flycheck-next-error)
(define-key boon-forward-search-map "m"  'flymake-goto-next-error)
(define-key boon-forward-search-map "b"  'next-buffer)
(define-key boon-forward-search-map "u"  'mc/cycle-forward)

(define-key boon-backward-search-map " "  'isearch-backward)
(define-key boon-backward-search-map "t"  'boon-qsearch-previous-at-point)
(define-key boon-backward-search-map "s"  'boon-qsearch-previous-at-point)
(define-key boon-backward-search-map "p"  'boon-qsearch-previous)
(define-key boon-backward-search-map "e"  'previous-error)
(define-key boon-backward-search-map "k"  'flycheck-previous-error)
(define-key boon-backward-search-map "m"  'flymake-goto-prev-error)
(define-key boon-backward-search-map "b"  'previous-buffer)
(define-key boon-backward-search-map "u"  'mc/cycle-backward)


(define-prefix-command 'boon-goto-map)
(set-keymap-parent boon-goto-map goto-map)

(define-key boon-goto-map "l" 'goto-line)
(define-key boon-goto-map "." 'find-tag)

(define-key boon-x-map "x" 'execute-extended-command)

(define-key boon-select-map "@"  'boon-select-occurences)
(define-key boon-select-map "#"  'boon-select-all)
(define-key boon-select-map " "  'boon-select-line)
(define-key boon-moves-map  "'" 'boon-switch-mark)
(define-key boon-moves-map  (kbd "<left>") 'left-char)
(define-key boon-moves-map  (kbd "<right>") 'right-char)
(define-key boon-moves-map  (kbd "<up>") 'previous-line)
(define-key boon-moves-map  (kbd "<down>") 'next-line)

(define-key boon-command-map "'" 'boon-toggle-mark)
(define-key boon-command-map [(return)] 'undefined)
(define-key boon-command-map (kbd "<RET>") 'undefined)
(define-key boon-command-map [(backspace)] 'undefined)
(define-key boon-command-map (kbd "<DEL>") 'undefined)
(define-key boon-command-map "`" 'boon-toggle-case)

(define-key boon-command-map "!" 'shell-command)
(define-key boon-command-map "|" 'shell-command-on-region)
(define-key boon-command-map "-" 'undo)
(dolist (number '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (define-key boon-command-map number 'digit-argument))
(define-key boon-command-map "~" 'universal-argument)

(define-key boon-command-map " " 'boon-drop-mark)
(define-key boon-command-map [escape] 'boon-quit)

;; Special mode rebinds
(define-key boon-special-map "`" 'boon-quote-character)
(define-key boon-special-map "'" 'boon-quote-character)
(define-key boon-special-map "x" boon-x-map)
(define-key boon-special-map [escape] 'boon-set-command-state)

;;  Insert mode rebinds
(define-key boon-insert-map [remap newline] 'boon-newline-dwim)
(define-key boon-insert-map [escape] 'boon-set-command-state)

;; Global rebinds
(define-key global-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key isearch-mode-map [escape] 'isearch-abort)


(provide 'boon-keys)
;;; boon-keys.el ends here
