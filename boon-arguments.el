;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'boon-core)

(defvar boon-enclosures
      '(
        (?$ . ("$" "$")) 
        (?| . ("|" "|")) 
        (?@ . ("@" "@")) 
        (?/ . ("/" "/")) 
        (?` . ("`" "`"))
        (?A . ("⟨" "⟩"))
        (?a . ("<" ">"))
        (?b . ("[" "]"))
        (?c . ("{-" "-}"))
        (?d . ("\"" "\""))
        (?f . ("«" "»")) ;; french quotes
        (?h . ("#" "#")) ;; hash
        (?m . ("`" "'"))
        (?p . ("(" ")"))
        (?q . ("'" "'"))
        (?r . ("{" "}"))
        (?o . ("⟦" "⟧")) ;; oxford brackets
        (?t . ("~" "~")) ;; tilda
        ))

(defun boon-spec-enclosure ()
  "Specify an enclosure style."
  (let ((c (read-char "Specify the enclosure")))
    (message "Char: %c " c)
    (list (cdr (assoc c boon-enclosures)))))

(defun boon-select-thing-at-point (thing)
  (list 'region (bounds-of-thing-at-point thing)))

(defun boon-select-from-region (select-fun)
  (interactive)
  (save-excursion
    (call-interactively select-fun)
    (list 'region (cons (region-beginning) (region-end)))))

(defun boon-select-wim () ;; what i mean
  (interactive)
  (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                    (bounds-of-thing-at-point 'sexp))))
    ;; TODO: use expand-region if bounds is nil.
    (list 'region bounds)))
(defun boon-jump-over-blanks ()
  (interactive)
  (skip-chars-forward "\n\t "))

(defun boon-jump-over-blanks-backward ()
  (interactive)
  (skip-chars-backward "\n\t "))


(defun boon-select-justline () (interactive) (list 'region (line-beginning-position) (line-end-position)))
(defun boon-select-line () (interactive) (boon-select-thing-at-point 'line))
(defun boon-select-paragraph () (interactive) (boon-select-thing-at-point 'paragraph))
(defun boon-select-document () (interactive)
  (list 'region (cons (point-min) (point-max))))
(defun boon-select-word () (interactive) (boon-select-thing-at-point 'word))
(defun boon-select-sentence () (interactive) (boon-select-thing-at-point 'sentence))
(defun boon-select-symbol () (interactive) (boon-select-thing-at-point 'symbol))
(defun boon-select-list () (interactive) (boon-select-thing-at-point 'list))
(defun boon-select-sexp () (interactive) (boon-select-thing-at-point 'sexp))
(defun boon-select-outside-pairs () (interactive) (boon-select-from-region 'er/mark-outside-pairs))
(defun boon-select-inside-pairs () (interactive) (boon-select-from-region 'er/mark-inside-pairs))
(defun boon-select-outside-quotes () (interactive) (boon-select-from-region 'er/mark-outside-quotes))
(defun boon-select-whitespace () (interactive) (boon-select-thing-at-point 'whitespace))
(defun boon-select-blanks ()
  (interactive)
  (list 'region (cons
                 (save-excursion
                   (boon-jump-over-blanks-backward)
                   (point))
                 (save-excursion
                   (boon-jump-over-blanks)
                   (point)))))

(defun boon-normalize-reg (reg)
  (cons (min (cdr reg) (car reg)) (max (cdr reg) (car reg))))

(defun boon-borders (reg how-much)
  (list (cons (cdr reg) (- (cdr reg) how-much))
        (cons (car reg) (+ (car reg) how-much))))
(defun boon-content (reg)
  (cons (+ (car reg) 1) (- (cdr reg) 1)))

(defun boon-select-borders (how-much regs)
  "Return the bordering (of size (as HOW-MUCH)) of a region (as REGS)."
  (interactive (cons (prefix-numeric-value prefix-arg) (boon-spec-region "select contents")))
  (cons 'region (apply 'append (mapcar (lambda (reg) (boon-borders reg how-much)) (mapcar 'boon-normalize-reg regs)))))

(defun boon-select-content (regs)
  (interactive (boon-spec-region "select borders"))
  (cons 'region (mapcar 'boon-content (mapcar 'boon-normalize-reg regs))))

;; TODO: this should not return a list
(defun boon-spec-region (msg)
  "Specify a region concisely using the keyboard.
The prompt (as MSG) is displayed."
  (list (if (use-region-p) (list (cons (region-beginning) (region-end)))
          (let (prefix-arg
                ;; this code fiddles with the prefix arg; but if we do
                ;; not hide our fiddling, the next command will use
                ;; the prefix arg that we have set.
                (orig (point))
                (km boon-select-map))
            (setq prefix-arg 0)
            (while (and km (keymapp km))
              (let ((last-char (read-char (format "%s %s" msg prefix-arg))))
               (if (and (>= last-char ?0) (<= last-char ?9))
                   (setq prefix-arg (+ (- last-char ?0) (* 10 prefix-arg )))
                 (setq km (lookup-key km (make-string 1 last-char))))))
            (when (eq prefix-arg 0)
              (setq prefix-arg nil))
            (if km
                (let (regs final)
                  (save-excursion
                    (setq regs (call-interactively km))
                    (setq final (point)))
                  ;; (message (format "Reg = %s" regs))
                  (if (and regs
                           (listp regs)
                           (eq (car regs) 'region))
                      (cdr regs)
                    (list (cons orig final))))
              (error "Unknown region specifier"))))))

(provide 'boon-arguments)
;;; boon-arguments.el ends here
