;;; boon-keys.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module defines various keymaps and portions of keymaps, common
;; to all keyboard layouts.

;;; Code:

(require 'boon-core)
(require 'boon-main)

(defvar boon-goto-map (make-sparse-keymap))

(define-key boon-select-map "@"  'boon-select-occurences)
(define-key boon-select-map "#"  'boon-select-all)
(define-key boon-select-map " "  'boon-select-line)
(define-key boon-moves-map  "'" 'boon-switch-mark)

(define-key boon-command-map "'" 'boon-toggle-mark)
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

;; Special mode rebinds
(define-key boon-special-map "`" 'boon-quote-character)
(define-key boon-special-map "'" 'boon-quote-character)
(define-key boon-special-map "x" boon-x-map)

;; insert mode rebinds
(define-key boon-insert-map [(escape)] 'boon-set-command-state)

;; Off mode rebinds
(define-key boon-off-map [(escape)] 'boon-set-command-state)

;;  Insert mode rebinds
(define-key boon-insert-map [remap newline] 'boon-newline-dwim)
(define-key boon-insert-map [(escape)] 'boon-set-command-state)

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
(defun boon-helm-browse (action)
  "Run the ACTION and set 'boon-helm-command-map' as transient keymap."
  (lambda ()
    (interactive)
    (call-interactively action)
    (setq cursor-type 'box)
    (set-transient-map boon-helm-command-map t (lambda () (setq cursor-type 'bar)))))

(defun define-helm-key (key action)
  "Bind C-<KEY> in helm-map, and <KEY> in boon-helm-command-map (both to ACTION)."
  (define-key helm-map (vconcat (mapcar 'boon-god-control-swap key)) (boon-helm-browse action))
  (define-key boon-helm-command-map key action))

(defvar boon-helm-command-map (make-sparse-keymap))
(suppress-keymap boon-helm-command-map 't)

(with-eval-after-load 'helm
  (define-key helm-map [(tab)]             'helm-select-action)
  (define-key helm-map (kbd "C-z")         'undefined)
  (define-key helm-map [(escape)]          'helm-keyboard-quit)
  (define-key helm-map (kbd "SPC")         'boon-completer-space)
  (define-key helm-map (kbd "M-SPC")       'helm-toggle-visible-mark)
  (define-key helm-map (kbd "C-<down>")    'helm-narrow-window)
  (define-key helm-map (kbd "C-<up>")      'helm-enlarge-window)
  (define-key helm-map (kbd "C-<return>")  'helm-execute-persistent-action)
  (define-key helm-map [(shift backspace)] 'helm-unmark-all)
  (define-helm-key (kbd "f")               'helm-follow-mode))

(define-key boon-goto-map "a" 'helm-apropos)
(define-key boon-goto-map "b" 'helm-buffers-list)
(define-key boon-goto-map "e" 'helm-projectile-ag)
(define-key boon-goto-map "f" 'helm-projectile-find-file)
(define-key boon-goto-map "g" 'helm-resume)
(define-key boon-goto-map "i" 'helm-git-grep)
(define-key boon-goto-map "k" 'helm-show-kill-ring)
(define-key boon-goto-map "l" 'goto-line) ;; no helm version of this
(define-key boon-goto-map "m" 'helm-all-mark-ring)
(define-key boon-goto-map "o" 'helm-multi-occur)
(define-key boon-goto-map "p" 'helm-projectile)
(define-key boon-goto-map "r" 'helm-register)
(define-key boon-goto-map "t" 'helm-etags-select)
(define-key boon-goto-map "y" 'helm-flycheck)

(defun boon-god-control-swap (event)
  "Swap the control 'bit' in EVENT, if that is a good choice."
  (interactive (list (read-key)))
  (cond
   ((memq event '(9 13)) event)
   ((<= event 27) (+ 96 event))
   ((not (eq 0 (logand (lsh 1 26) event))) (logxor (lsh 1 26) event))
   (t (list 'control event))))

(defun boon-c-god ()
  "Input a key sequence, prepend C- to each key, and run the command bound to that sequence."
  (interactive)
  (let ((keys '((control c)))
        (binding (key-binding (kbd "C-c")))
        (key-vector (kbd "C-c"))
        (prompt "C-c-"))
    (while (and binding (not (symbolp binding)))
      (let ((key (read-key (format "%s" prompt))))
        (if (eq key ?h) (describe-bindings key-vector)
          (push (boon-god-control-swap key) keys)
          (setq key-vector (vconcat (reverse keys)))
          (setq prompt (key-description key-vector))
          (setq binding (key-binding key-vector)))))
    (setq this-command-keys key-vector)
    (cond
     ((not binding) (error "No command bound to %s" prompt))
     ((commandp binding) (call-interactively binding))
     (t (error "Key not bound to a command: %s" binding)))))

(provide 'boon-keys)
;;; boon-keys.el ends here
