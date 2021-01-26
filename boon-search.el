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
                   (if forward (1+ (point)) (1- (point)))
                 (message "Wrapping around")
                 (if forward (point-min) (point-max))))
    (setq boon-search-success nil)
    (let ((case-fold-search nil)) ;; because hi-lock is case-sensitive
      (if forward (re-search-forward boon-regexp) (re-search-backward boon-regexp)))
    ;; If search fails an exception is thrown and this won't be set.
    (setq boon-search-success t))
  (goto-char (match-beginning 0)))

;;;###autoload
(defun boon-qsearch-next ()
  "Search the next occurence of the current search regexp."
  (interactive)
  (boon-qsearch t))

;;;###autoload
(defun boon-qsearch-previous ()
  "Search the previous occurence of the current search regexp."
  (interactive)
  (boon-qsearch nil))

;;;###autoload
(defun boon-qsearch-next-at-point ()
  "Search the next occurence of the current string at point and select the match."
  (interactive)
  (boon-set-search-string (boon-stuff-at-point))
  (boon-qsearch t)
  (deactivate-mark))

;;;###autoload
(defun boon-qsearch-previous-at-point ()
  "Search the previous occurence of the current string at point and select the match."
  (interactive)
  (boon-set-search-string (boon-stuff-at-point))
  (boon-qsearch nil)
  (deactivate-mark))

;;;###autoload
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

(defun boon-case-fold-regex (regex)
  "Make REGEX case-insensitive, depending on `case-fold-search'.
This is an extremely bugged first draft."
  (if (not case-fold-search) regex
    (replace-regexp-in-string
           "[[:alpha:]]"
           (lambda (m) (format "[%s%s]"
                               (upcase (match-string 0 m))
                               (match-string 0 m)))
           regex)))

;;;###autoload
(defun boon-highlight-regexp ()
  "Make sure boon-regexp is highlighted."
  (interactive)
  (hi-lock-face-buffer boon-regexp))

;;;###autoload
(defun boon-navigate (forward)
  "Go to the next item of interest, FORWARD or backwards."
  (cond
   ((and (bound-and-true-p multiple-cursors-mode) (> (mc/num-cursors) 1))
    (if forward (mc/cycle-forward) (mc/cycle-backward)))
   ((and boon-regexp
         (bound-and-true-p hi-lock-interactive-patterns)
         (equal boon-regexp (car (car hi-lock-interactive-patterns))))
    (boon-qsearch forward))
   (t (next-error (if forward 1 -1)))))

;;;###autoload
(defun boon-navigate-forward ()
  "Go to the next item of interest."
  (interactive)
  (boon-navigate t))

;;;###autoload
(defun boon-navigate-backward ()
  "Go to the next item of interest."
  (interactive)
  (boon-navigate nil))

(defadvice isearch-exit (after boon-isearch-set-search activate compile)
  "After isearch, highlight the search term and set it as boon current regexp."
  (boon-set-search-string isearch-string))

(defadvice swiper--action (after boon-swiper-set-search activate compile)
  "After swiper, highlight the search term and set it as boon current regexp."
  (boon-set-search-regexp (boon-case-fold-regex (car regexp-search-ring))))

(provide 'boon-search)
;;; boon-search.el ends here
