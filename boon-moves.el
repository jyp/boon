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
     (save-mark-and-excursion
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
    (let ((back-limit (- (point) 5))) ;; looking back at comment delimiter
      (cond
       ((boon-looking-at-comment -1)
        (forward-comment -1))
       ((looking-back "\\s\"" back-limit)
        (backward-char)
        (er--move-point-backward-out-of-string))
       ((looking-back "\\s)" back-limit)
        (backward-list))
       ((looking-back "\\s_" back-limit)  ;; symbol
        (skip-syntax-backward "_"))
       ((looking-back "\\s(" back-limit)
        (backward-char))
       ((looking-back "\\s!" back-limit)  ;; generic comment delimiter
        (skip-syntax-backward "!"))
       ((looking-back "\\sw" back-limit)
        (if (not (looking-at "\\(\\s-\\|\\s(\\|\\s)\\)"))
            (skip-syntax-backward "w")
          (skip-syntax-backward "w_")))
       (t
        (backward-char))))))

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
      (if (not (looking-back "\\(\\s-\\|\\s(\\|\\s)\\)" (1- (point))))
          (skip-syntax-forward "w")
        (skip-syntax-forward "w_")))
     (t
      (forward-char)))))

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
    (when (mark)
      (goto-char (mark))
      (pop-mark))))

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

