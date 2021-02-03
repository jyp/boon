;;; boon-search.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'boon-utils)
(require 'isearch)
(require 's)
(require 'dash)
(require 'hi-lock)


(defun boon--case-fold-regex (regex)
  "Make REGEX case-insensitive. This is an extremely bugged first draft."
  (replace-regexp-in-string
   "[[:alpha:]]"
   (lambda (m) (format "[%s%s]"
                       (upcase (match-string 0 m))
                       (match-string 0 m)))
   regex))

(defun boon-maybe-fold (regexp)           
  "Make REGEX case-insensitive, depending on configuration."
  (if (and case-fold-search
           (if search-upper-case
	       (isearch-no-upper-case-p regexp t) t))
      (boon--case-fold-regex regexp)
      regexp))

;;;###autoload
(defun boon-set-user-regexp (regexp)
  "Set REGEXP as current search. Apply case-folding as necessary."
  (interactive (list (completing-read "Regexp:" regexp-search-ring)))
  (boon-set-regexp (boon-maybe-fold regexp)))

;;;###autoload
(defun boon-set-regexp (regexp)
  "Set REGEXP as current search. No case folding is applied on REGEXP.
This function ensures that REGEXP is highlighted using `hi-lock'
and on the `car' of `hi-lock-interactive-patterns'."
  (interactive)
  (let* ((pat (assoc regexp hi-lock-interactive-patterns)))
    (if pat
        ;; pattern already set. Manipulate the patterns directly to put it on top.
        (setq hi-lock-interactive-patterns
              (cons pat (assoc-delete-all regexp hi-lock-interactive-patterns)))
      ;; hi-lock-face-buffer also turns on hi-lock mode, which asks
      ;; about reading file patterns. This is annoying. So use the following instead:
      (hi-lock-set-pattern regexp (hi-lock-read-face-name)))))

(defun boon-cur-regexp ()
  (when hi-lock-interactive-patterns (car (car hi-lock-interactive-patterns))))

(defun boon-qsearch (forward)
  "Search the `boon-cur-regexp'.
Do so in the direction specified (as FORWARD).  Point is set at
the beginning of the match."
  (when (not (boon-cur-regexp))
    (error "Nothing to search: hi-lock something to search before using boon-qsearch."))
  (boon--re (boon-cur-regexp) forward))

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
  (boon-set-regexp regexp))


(with-eval-after-load 'dap
  (defun boon-re-next (regexp)
    (boon-re-search regexp t))
  (defun boon-re-previous (regexp)
    (boon-re-search regexp nil))
  (define-key dap-hi-lock-regexp-map [remap re-search-forward] 'boon-re-next)
  (define-key dap-hi-lock-regexp-map [remap re-search-backward] 'boon-re-previous))

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
  (boon-re-search (regexp-quote (boon-stuff-at-point)) t)
  (deactivate-mark))

;;;###autoload
(defun boon-qsearch-previous-at-point ()
  "Search the previous occurence of the current string at point and select the match."
  (interactive)
  (boon-re-search (regexp-quote (boon-stuff-at-point)) nil)
  (deactivate-mark))




(defun boon-search-hi-lock (forward)
  "Search any `hi-lock-interactive-patterns'.
Do so in the FORWARD direction."
  (boon--re (s-join "\\|" (-map 'car hi-lock-interactive-patterns)) forward)
  (when-let* ((re (-first 'looking-at (-map 'car hi-lock-interactive-patterns))))
    (boon-set-regexp re)))

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
   ((boon-cur-regexp)
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
