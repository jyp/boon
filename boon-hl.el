;;; boon-hl.el --- minor mode for interactive automatic highlighting  -*- lexical-binding: t -*-

;; Copyright (C) 2000-2021 Free Software Foundation, Inc.

;; Author: Jean-Philippe Bernardy 
;;         David M. Koppelman <koppel@ece.lsu.edu>
;; Keywords: faces, minor-mode, matching, display

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;  
;;  This is a fork of hi-lock after it was changed to become unusable
;;  for searching highlighted patterns (from Emacs 28).

;;; Code:

(require 'hi-lock) ;; for the definition of faces and and helper functions
(require 'dash)

(defvar-local boon-hl-patterns nil
  "Patterns provided to boon-hl by user.  Should not be changed.")
(put 'boon-hl-patterns 'permanent-local t)

(defcustom boon-hl-face-defaults
  '(hi-yellow hi-pink hi-green hi-blue hi-salmon hi-aquamarine
    hi-black-b hi-blue-b hi-red-b hi-green-b hi-black-hb)
  "Default faces for boon-hl."
  :group 'boon
  :type '(repeat face))

;;;###autoload
(defun boon-hl-regexp (regexp &optional face)
  "Set face of each match REGEXP to FACE using font-lock.
  
If FACE is nil, choose a face from `boon-hl-face-defaults'
or prompt if universal argument is non-nil.  If REGEXP contains
upper case characters (excluding those preceded by `\\') and
`search-upper-case' is non-nil, the matching is case-sensitive."
  (interactive
   (list
    (hi-lock-regexp-okay
     (read-regexp "Regexp to highlight" 'regexp-history-last))))
  (boon-hl-add regexp face nil
                    (if (and case-fold-search search-upper-case)
                        (isearch-no-upper-case-p regexp t)
                      case-fold-search)
                    search-whitespace-regexp))

;;;###autoload
(defun boon-hl-symbol (string &optional face)
  "`book-hi-lock-regexp' (regexp-quote STRING) FACE.
Additionally, do not mess with case-fold."
  (interactive "sSymbol to highlight:")
  (boon-hl-add (hi-lock-regexp-okay (format "\\_<%s\\_>" (regexp-quote string)))
               face string font-lock-keywords-case-fold-search))

(defvar-local boon-hl--unused-faces nil
  "List of faces that is not used and is available for highlighting new text.
Face names from this list come from `boon-hl-face-defaults'.")

;;;###autoload
(defun boon-hl-remove (pattern)
  "Remove PATTERN highlight."
  (interactive 
   (list (assoc (completing-read "Unhighlight: "
                          (-map #'car boon-hl-patterns))
                boon-hl-patterns)))
  (font-lock-remove-keywords nil (list (plist-get (cdr pattern) :kw)))
  (push (plist-get (cdr pattern) :face) boon-hl--unused-faces)
  (setq boon-hl-patterns
        (delete pattern boon-hl-patterns))
  (font-lock-flush))

;;;###autoload
(defun boon-hl-search (pattern &optional direction limit)
  "Search for PATTERN up to LIMIT.
Search backward if DIRECTION is non-nil."
  (funcall (car (plist-get (cdr pattern) :kw)) limit direction))

;;;###autoload
(defun boon-hl-search-backward (pattern &optional limit)
  "Search for PATTERN up to LIMIT backward."
  (boon-hl-search pattern t limit))

(defun boon--pattern-at (pattern pos limit)
  "Search for PATTERN from POS up to LIMIT."
  (save-excursion
    (goto-char pos)
    (boon-hl-search pattern nil limit)))


(defun boon--faces-property (pos)
  ""
  (let ((x (get-text-property pos 'face)))
    (if (listp x) x (list x))))

(defun boon-hl-patterns-at-point ()
  "List of hl'ed patterns at point."
  (--filter
   (let* ((pat-face (plist-get (cdr it) :face))
          (limit (point))
          (pos (if (memq pat-face (boon--faces-property (1- (point))))
                   (1- (point))
                 (point))))
     (while (memq pat-face (boon--faces-property limit))
       (setq limit (next-single-property-change limit 'face)))
     (while (and (not (boon--pattern-at it pos limit))
                 (memq pat-face (boon--faces-property pos)))
       (setq pos (previous-single-property-change pos 'face)))
     (boon--pattern-at it pos limit))
   boon-hl-patterns))

(defun boon-hl-read-face-name ()
  "Get face for highlighting.
The next available face.  With a prefix argument, read a face
from the minibuffer with completion and history."
  (unless boon-hl--unused-faces
    (setq boon-hl--unused-faces boon-hl-face-defaults))
  (let* ((defaults (append boon-hl--unused-faces
			   boon-hl-face-defaults))
	 (face (if current-prefix-arg
	          (completing-read
	           (format-prompt "Highlight using face" (car defaults))
	           obarray 'facep t nil 'face-name-history defaults)
                (car defaults))))
         ;; Update list of unused faces.
         (setq boon-hl--unused-faces
               (remove face boon-hl--unused-faces))
         ;; Grow the list of defaults.
         (add-to-list 'boon-hl-face-defaults face t)
         face))

(defun boon-hl-add (regexp face &optional lighter
                                case-fold spaces-regexp)
  "Highlight SUBEXP of REGEXP with face FACE.
If omitted or nil, SUBEXP defaults to zero, i.e. the entire
REGEXP is highlighted.  LIGHTER is a human-readable string to
display instead of a regexp.  Non-nil CASE-FOLD ignores case.
SPACES-REGEXP is a regexp to substitute spaces in font-lock search."
  (let ((id (list regexp case-fold spaces-regexp)))
    (if-let* ((ix (--find-index (equal id (plist-get (cdr it) :id))
                                boon-hl-patterns)))
        (setq boon-hl-patterns
              (cons (nth ix boon-hl-patterns)
                    (-remove-at ix boon-hl-patterns)))
      (setq face (or face (boon-hl-read-face-name)))
      (let ((kw (list (lambda (limit &optional backward)
                    (let ((case-fold-search case-fold)
                          (search-spaces-regexp spaces-regexp))
                      (if backward
                          (re-search-backward regexp limit t)
                        (re-search-forward regexp limit t))))
                  (list 0 (list 'quote face) 'prepend)))) ;; 0 = subexp
        (push (list (or lighter regexp) :kw kw :face face :id id)
              boon-hl-patterns)
        (font-lock-add-keywords nil (list kw) t)
        (font-lock-flush)))))



(provide 'boon-hl)

;;; boon-hl.el ends here
