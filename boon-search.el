;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'boon-main)

(defun boon-search-regexp (forward)
  (when (not boon-regexp)
    (error "Search string not set"))
  (when (not isearch-success)
    (message "Wrapping around")
    (if forward (beginning-of-buffer) (end-of-buffer)))
  (setq isearch-success nil)
  (if forward 
      (re-search-forward boon-regexp)
    (re-search-backward boon-regexp))
  (setq isearch-success t) ; If search fails an exception is thrown and this won't be set. 
  )

(defun boon-qsearch (forward)
  (boon-highlight-regexp)
  (boon-search-regexp forward)
  (deactivate-mark))

(defun boon-qsearch-next ()
  "search the next occurence of the current search regexp"
  (interactive)
  (boon-qsearch t))

(defun boon-qsearch-previous ()
  "search the previous occurence of the current search regexp"
  (interactive)
  (boon-qsearch nil))

(defun boon-qsearch-next-at-point ()
  "search the next occurence of the current search regexp at point"
  (interactive)
  (boon-set-search-string (boon-stuff-at-point))
  (boon-qsearch t))

(defun boon-qsearch-previous-at-point ()
  "search the previous occurence of the current search regexp at point"
  (interactive)
  (boon-set-search-string (boon-stuff-at-point))
  (boon-qsearch nil))

(defun boon-set-search-string (string)
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
  (interactive)
  ;; (global-hi-lock-mode 1)
  (hi-lock-face-buffer boon-regexp 'hi-yellow))

(defun boon-unhighlight ()
  (interactive)
  (when (bound-and-true-p hi-lock-interactive-patterns)
    (hi-lock-unface-buffer (car (car hi-lock-interactive-patterns)))))

(defun boon-isearch-region (forward beg end)
 "search the selection"
 (let ((selection (buffer-substring-no-properties beg end)))
   (deactivate-mark)
   (if forward (goto-char end) (goto-char (- beg 1))) ; ensuring that we find the next match
   (isearch-mode forward nil nil nil)
   (isearch-yank-string selection)))

(defadvice isearch-exit (after ysph-hl-search activate compile)
  "after isearch, highlight the search term "
  (setq boon-regexp isearch-string)
  (boon-highlight-regexp))

(provide 'boon-search)
;;; boon-search.el ends here
