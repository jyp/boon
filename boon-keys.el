;;; boon-keys.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module defines various keymaps and portions of keymaps, common
;; to all keyboard layouts.

;;; Code:

(require 'boon-core)
(require 'boon-main)

(define-key boon-select-map " "  'boon-select-line)

(define-key boon-special-map "x" boon-x-map)
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
(define-key boon-command-map "=" 'universal-argument)
(define-key boon-command-map " " 'boon-drop-mark)
(define-key boon-command-map [(escape)] 'boon-quit)

(defun boon-push-events (kbd-string)
  "Push back some key events (as KBD-STRING) in the queue."
  (setq unread-command-events
        (append (kbd kbd-string) unread-command-events)))

(define-key boon-c-map "!" (lambda () (interactive) (boon-push-events "C-c !")))
(define-key boon-c-map "'" (lambda () (interactive) (boon-push-events "C-c '")))
(define-key boon-c-map " " (lambda () (interactive) (boon-push-events "C-c C-SPC")))
(define-key boon-c-map "," (lambda () (interactive) (boon-push-events "C-c C-,")))
(define-key boon-c-map "." (lambda () (interactive) (boon-push-events "C-c C-.")))
(define-key boon-c-map ">" (lambda () (interactive) (boon-push-events "C-c C->")))
(define-key boon-c-map "<" (lambda () (interactive) (boon-push-events "C-c C-<")))
(define-key boon-c-map "=" (lambda () (interactive) (boon-push-events "C-c C-=")))
(define-key boon-c-map "[" (lambda () (interactive) (boon-push-events "C-c [")))
(define-key boon-c-map "]" (lambda () (interactive) (boon-push-events "C-c ]")))
(define-key boon-c-map "?" (lambda () (interactive) (boon-push-events "C-c C-?")))
(define-key boon-c-map "a" (lambda () (interactive) (boon-push-events "C-c C-a")))
(define-key boon-c-map "b" (lambda () (interactive) (boon-push-events "C-c C-b")))
(define-key boon-c-map "c" (lambda () (interactive) (boon-push-events "C-c C-c")))
(define-key boon-c-map "d" (lambda () (interactive) (boon-push-events "C-c C-d")))
(define-key boon-c-map "e" (lambda () (interactive) (boon-push-events "C-c C-e")))
(define-key boon-c-map "f" (lambda () (interactive) (boon-push-events "C-c C-f")))
(define-key boon-c-map "g" (lambda () (interactive) (boon-push-events "C-c C-g")))
(define-key boon-c-map "h" (lambda () (interactive) (boon-push-events "C-c C-h")))
(define-key boon-c-map "i" (lambda () (interactive) (boon-push-events "C-c C-i")))
(define-key boon-c-map "j" (lambda () (interactive) (boon-push-events "C-c C-j")))
(define-key boon-c-map "k" (lambda () (interactive) (boon-push-events "C-c C-k")))
(define-key boon-c-map "l" (lambda () (interactive) (boon-push-events "C-c C-l")))
(define-key boon-c-map "m" (lambda () (interactive) (boon-push-events "C-c C-m")))
(define-key boon-c-map "n" (lambda () (interactive) (boon-push-events "C-c C-n")))
(define-key boon-c-map "o" (lambda () (interactive) (boon-push-events "C-c C-o")))
(define-key boon-c-map "p" (lambda () (interactive) (boon-push-events "C-c C-p")))
(define-key boon-c-map "q" (lambda () (interactive) (boon-push-events "C-c C-q")))
(define-key boon-c-map "r" (lambda () (interactive) (boon-push-events "C-c C-r")))
(define-key boon-c-map "s" (lambda () (interactive) (boon-push-events "C-c C-s")))
(define-key boon-c-map "t" (lambda () (interactive) (boon-push-events "C-c C-t")))
(define-key boon-c-map "u" (lambda () (interactive) (boon-push-events "C-c C-u")))
(define-key boon-c-map "v" (lambda () (interactive) (boon-push-events "C-c C-v")))
(define-key boon-c-map "w" (lambda () (interactive) (boon-push-events "C-c C-w")))
(define-key boon-c-map "x" (lambda () (interactive) (boon-push-events "C-c C-x")))
(define-key boon-c-map "y" (lambda () (interactive) (boon-push-events "C-c C-y")))
(define-key boon-c-map "z" (lambda () (interactive) (boon-push-events "C-c C-z")))



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


(defvar boon-goto-map (make-sparse-keymap))

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
