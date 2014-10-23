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


(define-key key-translation-map "xc " (kbd "C-c C-SPC"))
(define-key key-translation-map "xc," (kbd "C-c C-,"))
(define-key key-translation-map "xc." (kbd "C-c C-."))
(define-key key-translation-map "xc=" (kbd "C-c C-="))
(define-key key-translation-map "xc!" (kbd "C-c !"))
(define-key key-translation-map "xc[" (kbd "C-c ["))
(define-key key-translation-map "xc]" (kbd "C-c ]"))
(define-key key-translation-map "xc?" (kbd "C-c C-?"))
(define-key key-translation-map "xca" (kbd "C-c C-a"))
(define-key key-translation-map "xcb" (kbd "C-c C-b"))
(define-key key-translation-map "xcc" (kbd "C-c C-c"))
(define-key key-translation-map "xcd" (kbd "C-c C-d"))
(define-key key-translation-map "xce" (kbd "C-c C-e"))
(define-key key-translation-map "xcf" (kbd "C-c C-f"))
(define-key key-translation-map "xcg" (kbd "C-c C-g"))
(define-key key-translation-map "xch" (kbd "C-c C-h"))
(define-key key-translation-map "xci" (kbd "C-c C-i"))
(define-key key-translation-map "xcj" (kbd "C-c C-j"))
(define-key key-translation-map "xck" (kbd "C-c C-k"))
(define-key key-translation-map "xcl" (kbd "C-c C-l"))
(define-key key-translation-map "xcm" (kbd "C-c C-m"))
(define-key key-translation-map "xcn" (kbd "C-c C-n"))
(define-key key-translation-map "xco" (kbd "C-c C-o"))
(define-key key-translation-map "xcp" (kbd "C-c C-p"))
(define-key key-translation-map "xcq" (kbd "C-c C-q"))
(define-key key-translation-map "xcr" (kbd "C-c C-r"))
(define-key key-translation-map "xcs" (kbd "C-c C-s"))
(define-key key-translation-map "xct" (kbd "C-c C-t"))
(define-key key-translation-map "xcu" (kbd "C-c C-u"))
(define-key key-translation-map "xcv" (kbd "C-c C-v"))
(define-key key-translation-map "xcw" (kbd "C-c C-w"))
(define-key key-translation-map "xcx" (kbd "C-c C-x"))
(define-key key-translation-map "xcy" (kbd "C-c C-y"))
(define-key key-translation-map "xcz" (kbd "C-c C-z"))

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

(define-key boon-goto-map "g" 'helm-resume)
(define-key boon-goto-map "f" 'helm-for-files) ;; see http://amitp.blogspot.se/2012/10/emacs-helm-for-finding-files.html
(define-key boon-goto-map "l" 'goto-line)
(define-key boon-goto-map "i" 'helm-git-grep)
(define-key boon-goto-map "t" 'helm-etags-select)
(define-key boon-goto-map "y" 'helm-flycheck)
(define-key boon-goto-map "b" 'helm-buffers-list)
(define-key boon-goto-map "m" 'helm-multi-occur)

(provide 'boon-keys)
;;; boon-keys.el ends here
