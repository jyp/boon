;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'boon-main)

(defvar-local boon-regexp nil)

(defun boon-search-regexp (forward)
  "Re-seraach the current regexp, in the direction specified (as FORWARD)."
  (when (not boon-regexp)
    (error "Search string not set"))
  (when (not isearch-success)
    (message "Wrapping around")
    (goto-char (if forward (point-min) (point-max))))
  (setq isearch-success nil)
  (if forward
      (re-search-forward boon-regexp)
    (re-search-backward boon-regexp))
  (setq isearch-success t) ;; If search fails an exception is thrown and this won't be set.
  )

(defun boon-qsearch (forward)
  "Re-search the current regexp, in the direction specified (as FORWARD).
Moreover, highlight the regexp."
  (boon-highlight-regexp)
  (boon-search-regexp forward)
  (deactivate-mark))

(defun boon-qsearch-next ()
  "Search the next occurence of the current search regexp."
  (interactive)
  (boon-qsearch t))

(defun boon-qsearch-previous ()
  "Search the previous occurence of the current search regexp."
  (interactive)
  (boon-qsearch nil))

(defun boon-qsearch-next-at-point ()
  "Search the next occurence of the current search regexp at point."
  (interactive)
  (boon-set-search-string (boon-stuff-at-point))
  (boon-qsearch t))

(defun boon-qsearch-previous-at-point ()
  "Search the previous occurence of the current search regexp at point."
  (interactive)
  (boon-set-search-string (boon-stuff-at-point))
  (boon-qsearch nil))

(defun boon-set-search-string (string)
  "Set the search regexp by providing a string so match (as STRING)."
  (interactive "M")
  (setq boon-regexp (cond ((if (and (eq isearch-case-fold-search t)
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
  "Make sure the current regexp is highlighted."
  (interactive)
  ;; (global-hi-lock-mode 1)
  (hi-lock-face-buffer boon-regexp 'hi-yellow))

(defun boon-unhighlight ()
  "Pop a highlight regexp."
  (interactive)
  (when (bound-and-true-p hi-lock-interactive-patterns)
    (hi-lock-unface-buffer (car (car hi-lock-interactive-patterns)))))

(defun boon-isearch-region (forward beg end)
 "Search the current selection in the direction specified (as FORWARD).
The selection is between (as BEG END)."
 (let ((selection (buffer-substring-no-properties beg end)))
   (deactivate-mark)
   (if forward (goto-char end) (goto-char (- beg 1))) ; ensuring that we find the next match
   (isearch-mode forward nil nil nil)
   (isearch-yank-string selection)))

(defadvice isearch-exit (after ysph-hl-search activate compile)
  "After isearch, highlight the search term."
  (setq boon-regexp isearch-string)
  (boon-highlight-regexp))

(provide 'boon-search)
;;; boon-search.el ends here
