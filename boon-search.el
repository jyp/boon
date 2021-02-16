;;; boon-search.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'boon-utils)
(require 'isearch)
(require 's)
(require 'dash)
(require 'hi-lock)
(require 'pcre2el) ; rxt- functions


(defun boon-fold-set (set)
  "Foldcase SET by returning two sets, one for each case."
  (pcase set
    (`(,a . ,z) (list (cons (downcase a) (downcase z))
                      (cons (upcase a) (upcase z)))) ; not always correct (characters can mix low and upper carse), but good enough for now.
    ('lower '(or lower upper))
    ('upper '(or lower upper))
    ((pred symbolp) rx) ;; digit, alpha, etc.
    ((pred integerp) (list (downcase set) (upcase set)))))

(defun boon-fold-rx (rx)
  "Fold-case RX."
  (pcase rx
    ((pred integerp) (list 'any (downcase rx) (upcase rx)))
    ((pred stringp ) (cons 'seq (-map 'boon-fold-rx rx)))
    (`(not (any . ,rest)) `(not (any . ,(apply '-concat (-map 'boon-fold-set rest)))))
    (     `(any . ,rest)       `(any . ,(apply '-concat (-map 'boon-fold-set rest))))
    (`(= . (,n . ,rest)) (cons '= (cons n (-map 'boon-fold-rx rest))))
    (`(>= . (,n . ,rest)) (cons '>= (cons n (-map 'boon-fold-rx rest))))
    (`(group-n . (,n . ,rest)) (cons '= (cons n (-map 'boon-fold-rx rest))))
    (`(submatch-n . (,n . ,rest)) (cons '>= (cons n (-map 'boon-fold-rx rest))))
    (`(repeat . (,n . ,rest)) (cons '= (cons n (-map 'boon-fold-rx rest)))) ;; rx documentation contradictory for this case!
    (`(syntax ,s) `(syntax ,s)) 
    (`(category ,s) `(category ,s)) 
    (`(backref ,n) `(backref ,n)) 
    ('lower '(or lower upper))
    ('upper '(or lower upper))
    ((pred symbolp) rx) ;; digit, alpha, etc.
    (`(,head . ,rest) (cons head (-map 'boon-fold-rx rest))))) ; or, and, etc.

;; (boon--case-fold-regex "abcc\\|abec")
;; (boon--case-fold-regex "[^a-z]")
;; (boon--case-fold-regex "[^[:digit:]]")

(defun boon--case-fold-regex (regex)
  "Make REGEX case-insensitive."
  (rx-to-string (boon-fold-rx (rxt-adt->rx (rxt-parse-elisp regex))) t)
  ;; rxt-parse-elisp is bugged as of v. 1.8.:
  ;; (rxt-adt->rx (rxt-parse-elisp "[[:lower:][X-Z]]"))
  ;; (rxt-adt->rx (rxt-parse-elisp "[[a-c][X-Z]]")) ;nok
  ;; (rxt-adt->rx (rxt-parse-elisp "[^a-cX-Z]")) ;; ok 
  )

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
        ;; pattern already set. Manipulate the patterns directly to put it on top of the stack.
        (setq hi-lock-interactive-patterns
              (cons pat (assoc-delete-all regexp hi-lock-interactive-patterns)))
      (let ((hi-lock-auto-select-face t))
        ;; hi-lock-face-buffer also turns on hi-lock mode, which asks
        ;; about reading file patterns. This is annoying. So use the following instead:
        (hi-lock-set-pattern regexp (hi-lock-read-face-name))))))

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
    (let ((case-fold-search font-lock-keywords-case-fold-search))
      ;; we're matching what is being highlighted here.
      (if forward (re-search-forward regexp) (re-search-backward regexp)))
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
