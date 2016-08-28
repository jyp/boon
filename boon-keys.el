;;; boon-keys.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module defines various keymaps and portions of keymaps, common
;; to all keyboard layouts.

;;; Code:

(require 'boon-core)
(require 'boon-main)

(defvar boon-goto-map (make-sparse-keymap))

(define-key boon-select-map " "  'boon-select-line)
(define-key boon-command-map "'" 'boon-toggle-mark)

(define-key boon-special-map "`" 'boon-quote-character)
(define-key boon-special-map "'" 'boon-quote-character)

(define-key boon-command-map (kbd "<RET>") 'undefined)

(define-key boon-command-map [(return)] 'undefined)
(define-key boon-command-map (kbd "<RET>") 'undefined)
(define-key boon-command-map [(backspace)] 'undefined)
(define-key boon-command-map "`" 'boon-toggle-case)

(define-key boon-command-map "!" 'shell-command)
(define-key boon-command-map "|" 'shell-command-on-region)
(define-key boon-command-map "_" 'redo)
(define-key boon-command-map "-" 'undo)
(dolist (number '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (define-key boon-command-map number 'digit-argument))


(define-key boon-command-map " " 'boon-drop-mark)
(define-key boon-command-map [(escape)] 'boon-quit)

(defun boon-push-events (kbd-string)
  "Push back some key events (as KBD-STRING) in the queue."
  (setq unread-command-events
        (append (kbd kbd-string) unread-command-events)))


;; Off mode rebinds


(define-key boon-off-map [(escape)] 'boon-set-command-state)

;;  Insert mode rebinds
(define-key boon-insert-map [remap newline] 'boon-newline-dwim)

(define-key boon-insert-map [(escape)] 'boon-set-command-state)
;; (define-key boon-insert-map [backspace] 'boon-smart-insert-backspace2)
;; (define-key boon-insert-map "\"" 'boon-self-insert-quote) ;; not a good idea in emacs 24.5

;; Global rebinds
(define-key global-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key isearch-mode-map [escape] 'isearch-abort)

(defun boon-completer-space ()
  (interactive)
  (if (= (minibuffer-prompt-end) (point))
      ;(string= "" (ivy--input))
      (progn (next-history-element 1)
             ;; alt: ivy-next-history-element
             (move-end-of-line 1))
    (self-insert-command 1)))

;; Helm keys
(eval-after-load 'helm
  '(progn
     (define-key helm-map [(tab)]            'helm-select-action)
     (define-key helm-map (kbd "C-z")        'undefined)
     (define-key helm-map [(escape)] 'helm-keyboard-quit)
     (define-key helm-map [(control f)] 'helm-follow-mode)
     (define-key helm-map (kbd "SPC")        'boon-completer-space)
     (define-key helm-map (kbd "M-SPC")    'helm-toggle-visible-mark)
     (define-key helm-map (kbd "C-<down>") 'helm-narrow-window)
     (define-key helm-map (kbd "C-<up>")   'helm-enlarge-window)
     (define-key helm-map (kbd "C-<RET>")  'helm-execute-persistent-action)
     (define-key helm-map [(shift backspace)] 'helm-unmark-all)
     ))

(defvar boon-helm-command-map (make-sparse-keymap))
(suppress-keymap boon-helm-command-map 't)

(defun boon-helm-browse (action)
  "Run the ACTION and set 'boon-helm-command-map' as transient keymap."
  (interactive)
  (call-interactively action)
  (setq cursor-type 'box)
  (set-transient-map boon-helm-command-map t (lambda () (setq cursor-type 'bar))))



;; (define-key boon-goto-map "R" 'helm-registers)
(define-key boon-goto-map "a" 'helm-apropos)
(define-key boon-goto-map "b" 'helm-buffers-list)
(define-key boon-goto-map "f" 'helm-for-files) ;; see http://amitp.blogspot.se/2012/10/emacs-helm-for-finding-files.html
(define-key boon-goto-map "g" 'helm-resume)
(define-key boon-goto-map "i" 'helm-git-grep)
(define-key boon-goto-map "k" 'helm-show-kill-ring)
(define-key boon-goto-map "l" 'goto-line) ;; no helm version of this
(define-key boon-goto-map "m" 'helm-all-mark-ring)
(define-key boon-goto-map "o" 'helm-multi-occur)
(define-key boon-goto-map "t" 'helm-etags-select)
(define-key boon-goto-map "y" 'helm-flycheck)

(provide 'boon-keys)
;;; boon-keys.el ends here
