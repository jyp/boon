;;; boon-search.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'boon-utils)
(require 'isearch)
(require 's)
(require 'dash)

(defcustom boon-highlight-searched-regexp t "Should the searched regexp be highlighted?
See also `hi-lock-auto-select-face' for automatically choosing a highlight face.")

;;;###autoload
(defun boon-set-search-regexp (regexp)
  "Set REGEXP as current search."
  (interactive (list (completing-read "Regexp:" regexp-search-ring)))
  (unless (equal (car regexp-search-ring) regexp) (push regexp regexp-search-ring))
  (boon-highlight-regexp regexp))

(defun boon-qsearch (forward)
  "Search the `car' of `regexp-search-ring'.
Do so in the direction specified (as FORWARD).  Point is set at
the beginning of the match."
  (when (not regexp-search-ring)
    (error "No set search"))
  (boon-re-search (car regexp-search-ring) forward))


(defun boon--re (regexp forward)
  "Search REGEXP in the direction specified (as FORWARD).
Point is set at the beginning of the match."
  (save-excursion ;; so that we don't move the point if an exception is thrown
    (goto-char (if isearch-success
                   (if forward (1+ (point)) (1- (point)))
                 (message "Wrapping around")
                 (if forward (point-min) (point-max))))
    (setq isearch-success nil)
    (if forward (re-search-forward regexp) (re-search-backward regexp))
    ;; If search fails an exception is thrown and this won't be done:
    (setq isearch-success t))
  (goto-char (match-beginning 0)))

(defun boon-re-search (regexp forward)
  "Search REGEXP in the direction specified (as FORWARD).
Point is set at the beginning of the match. Also set the current
search regexp."
  (boon--re regexp forward)
  (boon-set-search-regexp regexp))


(with-eval-after-load 'dap
  (defun boon-re-next (regexp)
    (boon-re-search regexp t))
  (defun boon-re-previous (regexp)
    (boon-re-search regexp nil))
  (define-key dap-hi-lock-regexp-map [re-search-forward] 'boon-re-next)
  (define-key dap-hi-lock-regexp-map [re-search-backward] 'boon-re-previous))

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
  (boon-qsearch (regexp-quote (boon-stuff-at-point)) t)
  (deactivate-mark))

;;;###autoload
(defun boon-qsearch-previous-at-point ()
  "Search the previous occurence of the current string at point and select the match."
  (interactive)
  (boon-re-search (regexp-quote (boon-stuff-at-point)) nil)
  (deactivate-mark))

(defun boon--case-fold-regex (regex)
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
(defun boon-highlight-regexp (regexp)
  "Make sure REGEXP is highlighted using `hi-lock'."
  (interactive)
  (when (and boon-highlight-searched-regexp
	     (not (assoc regexp (bound-and-true-p hi-lock-interactive-patterns))))
    (require 'hi-lock)
    (hi-lock-set-pattern (boon--case-fold-regex regexp) (hi-lock-read-face-name))))

(defun boon-search-hi-lock (forward)
  "Search any `hi-lock-interactive-patterns'.
Do so in the FORWARD direction."
  (boon--re (s-join "\\|" (-map 'car hi-lock-interactive-patterns)) forward)
  (when-let* ((re (-first 'looking-at (-map 'car hi-lock-interactive-patterns))))
    (boon-set-search-regexp re)))

;;;###autoload
(defun boon-hi-lock-next ()
  "Search forward for any `hi-lock-interactive-patterns'."
  (interactive)
  (boon-search-hi-lock t))

;;;###autoload
(defun boon-hi-lock-previous ()
  "Search backwards for any `hi-lock-interactive-patterns'."
  (interactive)
  (boon-search-hi-lock nil))

;;;###autoload
(defun boon-navigate (forward)
  "Go to the next item of interest, FORWARD or backwards."
  (cond
   ((and (bound-and-true-p multiple-cursors-mode) (> (mc/num-cursors) 1))
    (if forward (mc/cycle-forward) (mc/cycle-backward)))
   ((and (bound-and-true-p regexp-search-ring)
         (assoc (car regexp-search-ring) (bound-and-true-p hi-lock-interactive-patterns)))
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


(provide 'boon-search)
;;; boon-search.el ends here
