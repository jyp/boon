;;; boon-moves.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains boon moves (jumping around somewhere).  These
;; commands are typically bound to a key in boon-moves-map.  They
;; can be bound to any desired key though (in global-map as well).

;;; Code:
(require 'boon-core)
(require 'boon-utils)
(require 'er-basic-expansions)
(require 'find-func)
(require 'boon-utils)
(require 'subr-x)


;;; Jumping to definitions (at point):

(defun boon-find-elisp-thing-at-point ()
  "Find an elisp thing at point.
Search preferentially for a function, then a variable."
  (interactive)
  (let ((symb (symbol-at-point)))
    (cond
     ((fboundp symb) (find-function-do-it symb nil 'switch-to-buffer))
     ((boundp  symb) (find-function-do-it symb 'defvar 'switch-to-buffer))
     (t (error "Unknown symbol")))))

(defun boon-find-tag-at-point ()
  "Find the symbol at point in the current tags table."
  (interactive)
  (let ((symb (thing-at-point 'symbol)))
    (cond (symb
           (find-tag symb (when (and current-prefix-arg (bound-and-true-p last-tag))
                            (if (< (prefix-numeric-value current-prefix-arg) 0)
                                '-
                              t))))
          (t (call-interactively 'find-tag)))))

(defcustom boon-find-definition-dispatch '()
  "An alist mapping major modes to finding the symbol at point."
  :group 'boon
  :type '(alist :key-type symbol :value-type function))
(setq boon-find-definition-dispatch
      '((c-mode . boon-find-tag-at-point)
        (emacs-lisp-mode . boon-find-elisp-thing-at-point)
        (lisp-interaction-mode . boon-find-elisp-thing-at-point)
        (haskell-mode . (lambda () (interactive) (if intero-mode (intero-goto-definition) (haskell-mode-jump-to-def-or-tag))))))

(defun boon-find-definition ()
  "Find a definition, in a way which is adapted to the 'major-mode'.
If possible, prompt the symbol at point."
  (interactive)
  ;; TODO (ring-insert find-tag-marker-ring (point-marker))
  (let ((mode-fap (assoc major-mode boon-find-definition-dispatch)))
    (if mode-fap (call-interactively (cdr mode-fap))
      (error "Finding definitions is not defined for %s.  Update the variable 'boon-find-definition-dispatch'"
             major-mode))))

(defun boon-find-char-backward (char)
  "Move the cursor backwards, until finding an occurence of the character CHAR."
  (interactive "cType the character to find")
  (search-backward (make-string 1 char))
  (forward-char 1))

(defun boon-find-char-forward (char)
  "Find the given character (as CHAR), forwards."
  (interactive "cType the character to find")
  (search-forward (make-string 1 char))
  (backward-char 1))

(defun boon-edge-of-expression (forward)
  "Jump to the forward or backward (as FORWARD) limit of the current expression."
  (interactive "P")
  (let ((orig-point (point)))
    (goto-char
     (save-excursion
       (deactivate-mark)
       (if (boon-in-string-p)
           (er/mark-inside-quotes) (er/mark-inside-pairs))
       (when forward (exchange-point-and-mark))
       (point)))
    ;; make sure we make some progress
    (when (eq (point) orig-point)
      (forward-char (if forward 1 -1)))))

(defun boon-end-of-expression ()
  "Jump to the end of the current expression."
  (interactive)
  (boon-edge-of-expression 't))

(defun boon-beginning-of-expression ()
  "Jump to the beginning of the current expression."
  (interactive)
  (boon-edge-of-expression nil))

(defun boon-smarter-upward (count)
  "Move upward, to a line with the same level of indentation or less, COUNT times."
  (interactive "p")
  (back-to-indentation)
  (dotimes (_number count)
    (previous-logical-line)
    (while (< (boon-col-relative-to-indent) 0) (previous-logical-line)))
  (back-to-indentation))

(defun boon-smarter-downward (count)
  "Move downward, to a line with the same level of indentation or less, COUNT times."
  (interactive "p")
  (back-to-indentation)
  (dotimes (_number count)
    (next-logical-line)
    (while (< (boon-col-relative-to-indent) 0) (next-logical-line)))
  (back-to-indentation))

(defun boon-smarter-backward (count)
  "Move backward, over COUNT whole syntactic units."
  (interactive "p")
  (dotimes (_number count)
    (boon-jump-over-blanks-backward)
    (cond
     ((boon-looking-at-comment -1)
      (forward-comment -1))
     ((looking-back "\\s\"")
      (backward-char)
      (er--move-point-backward-out-of-string))
     ((looking-back "\\s)")
      (backward-list))
     ((looking-back "\\s_")  ;; symbol
      (skip-syntax-backward "_"))
     ((looking-back "\\s(")
      (backward-char))
     ((looking-back "\\s!")  ;; generic comment delimiter
      (skip-syntax-backward "!"))
     ((looking-back "\\sw")
      (if (not (looking-at "\\(\\s-\\|\\s(\\|\\s)\\)"))
          (skip-syntax-backward "w")
        (skip-syntax-backward "w_")))
     (t
      (backward-char)))))

(defun boon-smarter-forward (count)
  "Move forward, over COUNT whole syntactic unit."
  (interactive "p")
  (dotimes (_number count)
    (boon-jump-over-blanks-forward)
    (cond
     ((boon-looking-at-line-comment-start-p)
      (end-of-line)
      (boon-jump-over-blanks-forward))
     ((boon-looking-at-comment 1);;
      (forward-comment 1))
     ((looking-at "\\s\"")
      (forward-char)
      (er--move-point-forward-out-of-string))
     ((looking-at "\\s(")
      (forward-list))
     ((looking-at "\\s_") ;; symbol
      (skip-syntax-forward "_"))
     ((looking-at "\\s)")
      (forward-char))
     ((looking-at "\\s!")  ;; generic comment delimiter
      (skip-syntax-forward "!"))
     ((looking-at "\\sw")
      (if (not (looking-back "\\(\\s-\\|\\s(\\|\\s)\\)"))
          (skip-syntax-forward "w")
        (skip-syntax-forward "w_")))
     (t
      (forward-char)))))

(defun boon-smarter-forward-spaces (count)
  "Move forward, over COUNT whole syntactic unit.
Handle spaces cleverly."
  (interactive "p")
  (declare (obsolete "does not seem very useful" "20151120"))
  (dotimes (_number count)
    (let ((spaces-skipped (not (equal (boon-jump-over-blanks-forward) 0)))
          (in-middle nil)
          (at-bol (string-blank-p (boon-line-prefix))))
      (cond
       ((boon-looking-at-line-comment-start-p)
        (end-of-line)
        (forward-char))
       ((boon-looking-at-comment 1);;
        (forward-comment 1))
       ((looking-at "\\s\"")
        (forward-char)
        (er--move-point-forward-out-of-string))
       ((looking-at "\\s(")
        (forward-list))
       ((looking-at "\\s_") ;; symbol
        (skip-syntax-forward "_"))
       ((looking-at "\\s)")
        (forward-char)
        (setq in-middle 't))
       ((looking-at "\\s!")  ;; generic comment delimiter
        (skip-syntax-forward "!"))
       ((looking-at "\\sw")
        (setq in-middle 't)
        (if (not (looking-back "\\(\\s-\\|\\s(\\|\\s)\\)"))
            (skip-syntax-forward "w")
          (skip-syntax-forward "w_")))
       (t
        (forward-char)
        (setq in-middle 't)))
      (unless (or spaces-skipped in-middle)
        (if at-bol
            (skip-chars-forward "\t\n ")
          (skip-chars-forward "\t "))))))

(defun boon-smarter-backward-spaces (count)
  "Move backward, over COUNT whole syntactic unit.
Handles spaces smartly."
  (interactive "p")
  (declare (obsolete "does not seem very useful" "20151120"))
  (dotimes (_number count)
    (let ((spaces-skipped (not (equal (boon-jump-over-blanks-backward) 0)))
          (in-middle nil)
          (at-eol (string-blank-p (boon-line-suffix))))
      (cond
       ((boon-looking-at-comment -1)
        (forward-comment -1))
       ((looking-back "\\s\"")
        (backward-char)
        (er--move-point-backward-out-of-string))
       ((looking-back "\\s)")
        (backward-list))
       ((looking-back "\\s_")  ;; symbol
        (skip-syntax-backward "_"))
       ((looking-back "\\s(")
        (backward-char)
        (setq in-middle 't))
       ((looking-back "\\s!")  ;; generic comment delimiter
        (skip-syntax-backward "!"))
       ((looking-back "\\sw")
        (setq in-middle 't)
        (if (not (looking-at "\\(\\s-\\|\\s(\\|\\s)\\)"))
            (skip-syntax-backward "w")
          (skip-syntax-backward "w_")))
       (t
        (backward-char)
        (setq in-middle 't)))
      (unless (or spaces-skipped in-middle)
        (if at-eol
            (skip-chars-backward "\t\n ")
          (skip-chars-backward "\t "))))))

(defun boon-visible-beginning-of-line ()
  "Move point leftwards to the first visible beginning of line."
  (interactive)
  (beginning-of-line)
  (while (bound-and-true-p outline-invisible-p)
    (backward-char 1)
    (beginning-of-line 1)))

(defun boon-beginning-of-line ()
  "Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (when (or (and (fboundp 'outline-invisible-p)
                   (outline-invisible-p))
              (= oldpos (point)))
      (boon-visible-beginning-of-line))))

(defun boon-end-of-line ()
  "Intelligently jump to the end of line.
This function toggles between jumping to 1. the last character of code on the
line 2. the last non-blank char on the line 3. the true end of
line."
  (interactive)
  (let* ((orig (point))
         (orig-eol (eolp))
         (progress (lambda () (and (not (bolp)) (or orig-eol (> (point) orig))))))
    (beginning-of-line)
    (while (not (or (boon-looking-at-line-comment-start-p) (eolp)))
      (forward-char))
    ;; we're now at the last non-comment character of the line
    (skip-chars-backward "\n\t " (line-beginning-position))
    ;; we're now at the last non-blank, non-comment character of the line
    (unless (funcall progress)
      (end-of-line)
      (skip-chars-backward "\n\t " (line-beginning-position))
      ;; we're now at the last non-blank character of the line
      (unless (funcall progress)
        (end-of-line)))))


(defun boon-switch-mark ()
  "If mark active, switch point and mark, otherwise pop mark from mark ring."
  (interactive)
  (if mark-active
      (exchange-point-and-mark)
    (if (mark)
        (progn
          (goto-char (mark))
          (pop-mark)))))

(defun boon-switch-mark-quick ()
  "Pop the mark ring until we find ourselves on a different line."
  (interactive)
  (declare (obsolete "annoying" "20160901"))
  (let ((orig-line (line-number-at-pos)))
    (while (> 1 (abs (- orig-line (line-number-at-pos))))
      (goto-char (mark))
      (pop-mark))))

(provide 'boon-moves)
;;; boon-moves.el ends here

