;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module defines various keymaps and portions of keymaps, common
;; to all keyboard layouts.

;;; Code:

(require 'boon-core)
(require 'boon-main)

(define-key boon-helm-command-map (kbd "C-<down>") 'helm-narrow-window)
(define-key boon-helm-command-map (kbd "C-<up>")   'helm-enlarge-window)
(define-key boon-helm-command-map [(escape)]       'helm-keyboard-quit)
(define-key boon-helm-command-map (kbd "M-SPC")    'helm-toggle-visible-mark)
(define-key boon-helm-command-map (kbd "SPC")      'boon-helm-set-insert-state)
(define-key boon-helm-command-map (kbd "<RET>")    'helm-exit-minibuffer)
(define-key boon-helm-command-map (kbd "<tab>")    'helm-select-action)
(define-key boon-helm-command-map (kbd "C-<RET>")  'helm-execute-persistent-action)

(define-key boon-command-map [(return)] 'undefined)
(define-key boon-command-map (kbd "RET") 'undefined)
(define-key boon-command-map [(backspace)] 'undefined)
(define-key boon-command-map (kbd "DEL") 'undefined)
(define-key boon-command-map "`" 'boon-toggle-case)
;; (dolist (d '("M-0" "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9"
;;              "C-0" "C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
;;   (define-key boon-command-map (read-kbd-macro d) 'digit-argument))

(define-key boon-command-map "_" 'redo)
(define-key boon-command-map "-" 'undo)
(define-key boon-command-map "\\" 'universal-argument)
(define-key boon-command-map " " 'boon-drop-mark)
(define-key boon-command-map [(escape)] 'boon-quit)

(defun boon-push-events (kbd-string)
  (setq unread-command-events
        (append (kbd kbd-string) unread-command-events)))
  
(define-key boon-c-map "!" (lambda () (interactive) (boon-push-events "C-c !")))
(define-key boon-c-map " " (lambda () (interactive) (boon-push-events "C-c C-SPC")))
(define-key boon-c-map "," (lambda () (interactive) (boon-push-events "C-c C-,")))
(define-key boon-c-map "." (lambda () (interactive) (boon-push-events "C-c C-.")))
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
(define-key boon-insert-map [backspace] 'boon-smart-insert-backspace2)
(define-key boon-insert-map "\"" 'boon-self-insert-quote)

;; Global rebinds
(define-key global-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key isearch-mode-map [escape] 'isearch-abort)

;; Helm keys
(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "C-l")        'previous-history-element)
     (define-key helm-map (kbd "C-;")        'next-history-element)
     (define-key helm-map (kbd "C-u")        'helm-previous-line)
     (define-key helm-map (kbd "C-y")        'helm-next-line)
     (define-key helm-map (kbd "C-,")        'helm-previous-page)
     (define-key helm-map (kbd "C-.")        'helm-next-page)
     (define-key helm-map [(tab)]            'helm-select-action)
     (define-key helm-map (kbd "C-z")        'undefined)
     (define-key helm-map [(control return)] 'helm-execute-persistent-action)
     (define-key helm-map [(escape)] 'boon-helm-set-command-state)

     ;; This won't be needed with emacs 24.4 (helm uses set-transient-map)
     (defun helm-maybe-update-keymap ()
       "Handle differents keymaps in multiples sources.
This function is meant to be run in `helm-move-selection-after-hook'.
It will override `helm-map' with the keymap attribute of current source
if some when multiples sources are present."
       (with-helm-window
         (let* ((source (helm-get-current-source))
                (kmap (and (listp source) ; Check if source is empty.
                           (assoc-default 'keymap source))))
           (when kmap (set-temporary-overlay-map kmap)))))
     ))

(defvar boon-goto-map (make-sparse-keymap))

(define-key boon-goto-map "b" 'helm-buffers-list)
(define-key boon-goto-map "f" 'helm-for-files) ;; see http://amitp.blogspot.se/2012/10/emacs-helm-for-finding-files.html
(define-key boon-goto-map "a" 'helm-apropos)
(define-key boon-goto-map "g" 'helm-resume)
(define-key boon-goto-map "i" 'helm-git-grep)
(define-key boon-goto-map "k" 'helm-all-mark-rings)
(define-key boon-goto-map "l" 'goto-line) ;; no helm version of this
(define-key boon-goto-map "m" 'helm-multi-occur)
(define-key boon-goto-map "r" 'helm-show-kill-ring)
(define-key boon-goto-map "R" 'helm-registers)
(define-key boon-goto-map "t" 'helm-etags-select)
(define-key boon-goto-map "y" 'helm-flycheck)

(provide 'boon-keys)
;;; boon-keys.el ends here
