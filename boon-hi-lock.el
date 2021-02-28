;;; boon-hi-lock.el --- minor mode for interactive automatic highlighting  -*- lexical-binding: t -*-

;; Copyright (C) 2000-2021 Free Software Foundation, Inc.

;; Author: Jean-Philippe Bernardy 
;;         David M. Koppelman <koppel@ece.lsu.edu>
;; Keywords: faces, minor-mode, matching, display

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;  
;;  This is a fork of hi-lock after it was changed to become unusable
;;  for searching highlighted patterns (from emacs 28).

;;; Code:

(require 'hi-lock) ;; for the definition of faces and and helper functions
(require 'dash)

(defvar-local boon-hi-lock-interactive-patterns nil
  "Patterns provided to boon-hi-lock by user.  Should not be changed.")
(put 'boon-hi-lock-interactive-patterns 'permanent-local t)

(defcustom boon-hi-lock-face-defaults
  '(hi-yellow hi-pink hi-green hi-blue hi-salmon hi-aquamarine
    hi-black-b hi-blue-b hi-red-b hi-green-b hi-black-hb)
  "Default faces for boon-hi-lock."
  :group 'boon
  :type '(repeat face))

;;;###autoload
(defun boon-hi-lock-regexp (regexp &optional face)
  "Set face of each match REGEXP to FACE using font-lock.
  
If FACE is nil, choose a face from `boon-hi-lock-face-defaults'
or prompt if universal argument is non-nil.  If REGEXP contains
upper case characters (excluding those preceded by `\\') and
`search-upper-case' is non-nil, the matching is case-sensitive."
  (interactive
   (list
    (hi-lock-regexp-okay
     (read-regexp "Regexp to highlight" 'regexp-history-last))))
  (boon-hi-lock-add regexp face nil
                    (if (and case-fold-search search-upper-case)
                        (isearch-no-upper-case-p regexp t)
                      case-fold-search)
                    search-whitespace-regexp))

;;;###autoload
(defun boon-hi-lock-symbol (string &optional face)
  "Like `book-hi-lock-regexp', but on STRING instead of regexp,
and do not mess with case-fold."
  (interactive "sSymbol to highlight:")
  (boon-hi-lock-add (hi-lock-regexp-okay (regexp-quote string)) face string case-fold-search))

(defvar-local boon-hi-lock--unused-faces nil
  "List of faces that is not used and is available for highlighting new text.
Face names from this list come from `boon-hi-lock-face-defaults'.")

;;;###autoload
(defun boon-hi-lock-remove (pattern)
  (interactive 
   (list (assoc (completing-read "Unhighlight: "
                          (-map #'car boon-hi-lock-interactive-patterns))
                boon-hi-lock-interactive-patterns)))
  (font-lock-remove-keywords nil (list (plist-get (cdr pattern) :kw)))
  (push (plist-get (cdr pattern) :face) boon-hi-lock--unused-faces)
  (setq boon-hi-lock-interactive-patterns
        (delete pattern boon-hi-lock-interactive-patterns))
  (font-lock-flush))

(defun boon-pattern-search (pattern &optional limit)
  "Search for PATTERN up to LIMIT."
  (funcall (car (plist-get (cdr pattern) :kw)) limit))

(defun boon--pattern-at (pattern pos limit)
  (save-excursion
    (goto-char pos)
    (boon-pattern-search pattern limit)))

(defun boon-hi-lock-patterns-at-point ()
  "List of hi-lock'ed patterns at point"
  (--filter
   (let* ((pat-face (plist-get (cdr it) :face))
           (limit (point))
           (pos (if (memq pat-face (get-text-property (1- (point)) 'face))
                    (1- (point))
                    (point))))
       (while (memq pat-face (get-text-property limit 'face))
         (setq limit (next-single-property-change limit 'face)))
       (while (and (not (boon--pattern-at it pos limit))
                   (memq pat-face (get-text-property pos 'face)))
         (setq pos (previous-single-property-change pos 'face)))
       (boon--pattern-at it pos limit))
     boon-hi-lock-interactive-patterns))
    
(defun boon-hi-lock-read-face-name ()
  "Face for highlighting.
The next available face. With a prefix argument, read a face from
the minibuffer with completion and history."
  (unless boon-hi-lock--unused-faces
    (setq boon-hi-lock--unused-faces boon-hi-lock-face-defaults))
  (let* ((defaults (append boon-hi-lock--unused-faces
			   boon-hi-lock-face-defaults))
	 (face (if current-prefix-arg
	          (completing-read
	           (format-prompt "Highlight using face" (car defaults))
	           obarray 'facep t nil 'face-name-history defaults)
                (car defaults))))
         ;; Update list of unused faces.
         (setq boon-hi-lock--unused-faces
               (remove face boon-hi-lock--unused-faces))
         ;; Grow the list of defaults.
         (add-to-list 'boon-hi-lock-face-defaults face t)
         face))

(defun boon-hi-lock-add (regexp face &optional lighter case-fold spaces-regexp)
  "Highlight SUBEXP of REGEXP with face FACE.
If omitted or nil, SUBEXP defaults to zero, i.e. the entire
REGEXP is highlighted.  LIGHTER is a human-readable string to
display instead of a regexp.  Non-nil CASE-FOLD ignores case.
SPACES-REGEXP is a regexp to substitute spaces in font-lock search."
  (setq lighter (or lighter regexp))
  (setq face (or face (boon-hi-lock-read-face-name)))
  (let ((kw (list (lambda (limit)
                    (let ((case-fold-search case-fold)
                          (search-spaces-regexp spaces-regexp))
                      (re-search-forward regexp limit t)))
                  (list 0 (list 'quote face) 'prepend)))  ;; 0 = subexp
        (id (list regexp case-fold spaces-regexp)))
    (push (list lighter :kw kw :face face :id id) boon-hi-lock-interactive-patterns)
    (font-lock-add-keywords nil (list kw) t)
    (font-lock-flush)))

(provide 'boon-hi-lock)

;;; boon-hi-lock.el ends here
