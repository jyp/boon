;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'boon-core)
(defvar boon-indent-map (make-sparse-keymap))

(defun boon-adjust-indent ()
 "Switch temporarily to adjust indentation mode."
 (interactive)
 (unless (use-region-p)
   (set-mark (line-beginning-position))
   (end-of-line)
   (deactivate-mark))
 (set-temporary-overlay-map boon-indent-map t))

(defun boon-unindent-rigidly (beg end count)
  "The opposite of `indent-rigidly'."
  (interactive "r\np")
  (indent-rigidly beg end (- count)))

(progn
    (setq boon-indent-map (make-sparse-keymap))
    (define-key boon-indent-map "e" 'boon-unindent-rigidly)
    (define-key boon-indent-map "i" 'indent-rigidly))


(defun boon-split-word ()
  "insert a space"
  (interactive)
  (insert " "))

(defun boon-open-word ()
  (interactive)
  (unless (eq (preceding-char) 32)
    (insert (make-string 1 32)))
  (unless (eq (following-char) 32)
    (insert (make-string 1 32))
    (backward-char 1))
  (boon-set-insert-state))

(defun boon-split-line ()
  "split the current line"
  (interactive)
  (let ((indent-col (min (current-line-indentation) (current-column))))
    ;; kill the extra spaces
    (save-excursion
      (delete-and-extract-region (progn
                                   (skip-chars-forward "\n\t " (line-end-position))
                                   (point))
                                 (progn
                                   (skip-chars-backward "\n\t " (line-beginning-position))
                                   (point))))
    (newline)
    (insert (make-string indent-col ?\ ))))


(defun boon-query-replace ()
  "Query replace; but if the region is active, replace its contents"
  (interactive)
  (if (and (use-region-p) (eq (- (line-number-at-pos (region-end)) (line-number-at-pos (region-beginning))) 0))
      (let ((selection (on-region #'buffer-substring-no-properties))) 
      (perform-replace 
       selection
       (read-string "Replace region with:")
       t ; query
       nil ; not a regexp
       nil ; not delimited
       nil ; no specific repeat count
       nil ; default keymap 
       (point-min-marker)
       (point-max-marker) ; replace in the whole buffer
       ))
    (call-interactively 'query-replace)))

(defun boon-prepare-mark ()
  (unless (use-region-p) (call-interactively 'boon-mark-region))
  (when (not (bound-and-true-p multiple-cursors-mode))
    (when (> (mark) (point))
      (exchange-point-and-mark)
      ;; this is to work-around a bug in multiple cursors,
      ;; where the currently marked things is unmarked if the point is after the mark.
      )))

(defun boon-mark-next-like-this ()
  (interactive)
  (boon-prepare-mark)
  (call-interactively 'mc/mark-next-like-this))

(defun boon-mark-previous-like-this ()
  (interactive)
  (boon-prepare-mark)
  (call-interactively 'mc/mark-previous-like-this))    

(defvar boon-flycheck-map
  (let ((pmap (make-sparse-keymap)))
    (define-key pmap "m" 'flycheck-mode)
    (define-key pmap "y" 'flycheck-buffer)
    (define-key pmap "C" 'flycheck-clear)
    (define-key pmap "r" 'flycheck-compile)
    (define-key pmap "n" 'flycheck-next-error)
    (define-key pmap "p" 'flycheck-previous-error)
    (define-key pmap "l" 'flycheck-list-errors)
    (define-key pmap "t" 'flycheck-copy-messages-as-kill)
    (define-key pmap "/" 'flycheck-google-messages)
    (define-key pmap "s" 'flycheck-select-checker)
    (define-key pmap "e" 'flycheck-set-checker-executable)
    (define-key pmap "d" 'flycheck-describe-checker)
    (define-key pmap "i" 'flycheck-info)
    (define-key pmap "V" 'flycheck-version)
    pmap)
  "Keymap to access stuff of `flycheck-mode'.")
  
(define-key boon-x-map "rr" 'boon-query-replace) ; replace the region if it is selected
(define-key boon-x-map "t" 'boon-toggle-comment) ; commenT
(define-key boon-x-map "i" 'boon-adjust-indent)
(define-key boon-x-map [(return)] 'boon-split-line)
(define-key boon-x-map " " 'boon-split-word)

(define-key boon-x-map "-" 'undo-tree-visualize)
(define-key boon-x-map "," 'boon-mark-previous-like-this); cursors: Prev
(define-key boon-x-map "." 'boon-mark-next-like-this); cursors: Next
(define-key boon-x-map "m" 'mc/skip-to-previous-like-this)
(define-key boon-x-map "/" 'mc/skip-to-next-like-this)
(define-key boon-x-map "O" 'previous-window) ;; o is next window
(define-key boon-x-map "S" 'save-some-buffers)
(define-key boon-x-map "\\" 'align-regexp)
(define-key boon-x-map "b" 'ido-switch-buffer)
(define-key boon-x-map "f" 'ido-find-file)
(define-key boon-x-map "hh" 'helm-apropos)
(define-key boon-x-map "j" 'join-line)
(define-key boon-x-map "k" 'kill-this-buffer)
(define-key boon-x-map "K" 'helm-show-kill-ring)
(define-key boon-x-map "l" 'fill-paragraph)
(define-key boon-x-map "M" 'menu-bar-open)
(define-key boon-x-map "s" 'save-buffer)
(define-key boon-x-map "u" 'mc/edit-lines); cUrsors: multiple
(define-key boon-x-map "vv" 'magit-status)
(define-key boon-x-map "g" 'magit-status)
(define-key boon-x-map "x" 'helm-M-x)
(define-key boon-x-map "y" boon-flycheck-map)


(provide 'boon-extras)
;;; boon-extras.el ends here
