;;; boon-extras.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module provides (arguably) more useful bindings to the "x"
;; prefix map.

;;; Code:

(require 'boon-core)
(require 'boon-main)

(defun boon-adjust-indent ()
 "Adjust indentation of the region or current line."
 (interactive)
 (unless (use-region-p)
   (set-mark (line-beginning-position))
   (end-of-line))
 (call-interactively 'indent-rigidly))

(defun boon-query-replace ()
  "Query replace; but if the region is active, replace its contents"
  (interactive)
  (if (and (use-region-p) (eq (- (line-number-at-pos (region-end)) (line-number-at-pos (region-beginning))) 0))
      (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
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

(defun boon-toggle-comment (regs)
  "Toggle comments in the regions REGS."
  (interactive (list (boon-spec-region "toggle comment")))
  (dolist (reg regs)
    (comment-or-uncomment-region (min (car reg) (cdr reg))
                                 (max (car reg) (cdr reg)))))

(define-key boon-x-map "rr" 'boon-query-replace) ; replace the region if it is selected
(define-key boon-x-map "t" 'boon-toggle-comment) ; commenT
(define-key boon-x-map "i" 'boon-adjust-indent)
(define-key boon-x-map [(return)] 'boon-split-line)
(define-key boon-x-map " " 'boon-split-word)

(define-key boon-x-map "U" 'undo-tree-visualize)
(define-key boon-x-map "O" 'previous-window) ;; o is next window
(define-key boon-x-map "S" 'save-some-buffers)
(define-key boon-x-map "\\" 'align-regexp)
(define-key boon-x-map "b" 'ido-switch-buffer)
(define-key boon-x-map "f" 'ido-find-file)
(define-key boon-x-map "h" help-map)
(define-key boon-x-map "j" 'join-line)
(define-key boon-x-map "k" 'kill-this-buffer)
(define-key boon-x-map "l" 'fill-paragraph)
(define-key boon-x-map "M" 'menu-bar-open)
(define-key boon-x-map "s" 'save-buffer)
(define-key boon-x-map "vv" 'magit-status)
(define-key boon-x-map "g" 'magit-status)
(define-key boon-x-map "x" 'helm-M-x)

(eval-after-load 'flycheck
  '(define-key boon-x-map "y" flycheck-command-map)
)


(provide 'boon-extras)
;;; boon-extras.el ends here
