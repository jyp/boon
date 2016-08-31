;;; boon-search.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'boon-utils)

(defvar-local boon-regexp nil "Current regexp search. Use boon-set-search-regexp to set this variable.")
(defvar-local boon-search-success t "Last search was successful or non-existent.")

(defun boon-set-search-regexp (regexp)
  "Set boon-regexp to REGEXP and manage highlighting."
  (when boon-regexp (hi-lock-unface-buffer boon-regexp))
  (setq boon-search-success t)
  (setq boon-regexp regexp)
  (boon-highlight-regexp))


(defun boon-qsearch (forward)
  "Search the current boon-regexp, in the direction specified (as FORWARD).
Point is set at the beginning of the match.  Moreover, highlight
the regexp."
  (when (not boon-regexp)
    (error "Search string not set"))
  (boon-highlight-regexp)
  (save-excursion ;; so that we don't move the point if an exception is thrown
    (goto-char (if boon-search-success
                   (if forward (+ 1 (point)) (- (point) 1))
                 (progn
                   (message "Wrapping around")
                   (if forward (point-min) (point-max)))))
    (setq boon-search-success nil)
    (if forward (re-search-forward boon-regexp) (re-search-backward boon-regexp))
    ;; If search fails an exception is thrown and this won't be set.
    (setq boon-search-success t))
  (goto-char (match-beginning 0)))

(defun boon-qsearch-next ()
  "Search the next occurence of the current search regexp."
  (interactive)
  (boon-qsearch t))

(defun boon-qsearch-previous ()
  "Search the previous occurence of the current search regexp."
  (interactive)
  (boon-qsearch nil))

(defun boon-qsearch-next-at-point ()
  "Search the next occurence of the current string at point and select the match."
  (interactive)
  (boon-set-search-string (boon-stuff-at-point))
  (boon-qsearch t)
  (deactivate-mark))

(defun boon-qsearch-previous-at-point ()
  "Search the previous occurence of the current string at point and select the match."
  (interactive)
  (boon-set-search-string (boon-stuff-at-point))
  (save-excursion
    (when (use-region-p)
      ;; make sure that we don't find the stuff that we've just
      ;; selected, by moving the point at the beginning of the match.
      (goto-char (region-beginning)))
    (boon-qsearch nil))
  (deactivate-mark))

(defun boon-set-search-string (string)
  "Set the search regexp by providing a string so match (as STRING)."
  (interactive "M")
  (boon-set-search-regexp (cond ((if (and (eq isearch-case-fold-search t)
                                   search-upper-case)
                              (isearch-no-upper-case-p
                               string isearch-regexp)
                            isearch-case-fold-search)
                          ;; Turn isearch-string into a case-insensitive
                          ;; regexp.
                          (mapconcat
                           (lambda (c)
                             (let ((s (string c)))
                               (if (string-match "[[:alpha:]]" s)
                                   (format "[%s%s]" (upcase s) (downcase s))
                                 (regexp-quote s))))
                           string ""))
                         (t (regexp-quote string)))))



(defun boon-highlight-regexp ()
  "Make sure boon-regexp is highlighted."
  (interactive)
  (hi-lock-face-buffer boon-regexp 'hi-yellow))

(defadvice isearch-exit (after ysph-hl-search activate compile)
  "After isearch, highlight the search term and set it as boon current regexp."
  (boon-set-search-regexp isearch-string))

(provide 'boon-search)
;;; boon-search.el ends here
