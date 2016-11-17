;;; boon-utils.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains several utilities, which shoud probably be part
;; of Emacs.  (Maybe they are and I did not find them.)

;;; Code:

(defmacro boon-with-ordered-region (body)
  "Run the BODY, ensuring that the point is before the mark."
  (declare (obsolete "Used only by obsolete code" "20160904"))
  `(if (< (point) (mark))
       ,body
       (progn (exchange-point-and-mark) ,body (exchange-point-and-mark))))

(defcustom boon-hints-enabled 't "Display hints." :group 'boon :type 'boolean)

(defun boon-hint (msg)
  "Provide MSG as a hint."
  (when boon-hints-enabled
    (message msg)))

(defun boon-current-line-indentation ()
  "Return the indentation of the curent line."
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun boon-line-prefix ()
  "Return the text between beginning of line and point."
  (buffer-substring-no-properties
   (line-beginning-position)
   (point)))

(defun boon-line-suffix ()
  "Return the text between end of line and point."
  (buffer-substring-no-properties
   (line-end-position)
   (point)))

(defun boon-col-relative-to-indent ()
  "Return the position of the cursor relative to indentation.
Can be negative."
  (- (point) (save-excursion (back-to-indentation) (point))))

(defun boon-looking-at-comment (how-many)
  "Is the current point looking at HOW-MANY comments? (negative for backwards)?"
  (save-excursion (forward-comment how-many)))

(defun boon-in-string-p ()
  "Determine if the point is inside a string."
  (nth 3 (syntax-ppss)))

(defun boon-looking-at-line-comment-start-p ()
  "Are we looking at a comment-start?"
  (interactive)
  (and (bound-and-true-p comment-start)
       (looking-at comment-start)
       (not (boon-in-string-p))))

(defun boon-stuff-at-point ()
  "Return a meaningful piece of text around at point.
If no such text exists, throw an error."
  (interactive)
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (or (thing-at-point 'symbol)
          (error "Nothing relevant at point; move to a symbol or select a region"))))

(defun boon-jump-over-blanks-forward ()
  "Jump over blanks, forward."
  (interactive)
  (skip-chars-forward "\n\t "))

(defun boon-jump-over-blanks-backward ()
  "Jump over blanks, backward."
  (interactive)
  (skip-chars-backward "\n\t "))

(provide 'boon-utils)

;;; boon-utils.el ends here
