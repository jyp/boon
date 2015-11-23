;;; boon-arguments.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:


;; A region list has the following form: ('region (begining . end) (begining . end) ...)

;;; Code:

(require 'boon-core)
(require 'boon-regs)
(require 'multiple-cursors)

(defcustom boon-enclosures
      '(
        (?A . ("⟨" "⟩"))
        (?a . ("<" ">"))
        (?b . ("[" "]"))
        (?c . ("{-" "-}"))
        (?d . ("\"" "\"")) ;; double quotes
        (?D . ("``" "''")) ;; Double quotes
        (?f . ("«" "»")) ;; french quotes
        (?h . ("#" "#")) ;; hash
        (?m . ("`" "'"))
        (?p . ("(" ")"))
        (?q . ("'" "'"))
        (?r . ("{" "}"))
        (?o . ("⟦" "⟧")) ;; oxford brackets
        (?t . ("~" "~")) ;; tilda
        )
        "Enclosures to use with the boon-enclose command."
        :type '(alist :key-type character :value-type (list string))
        :group 'boon
        )

(defun boon-spec-enclosure ()
  "Specify an enclosure style.  To be used as an argument to interactive."
  (let* ((c (boon-read-char "Specify the enclosure"))
         (s (make-string 1 c))
         (choice (assoc c boon-enclosures)))
    (if choice (cdr choice) (list s s))))

(defun boon-select-thing-at-point (thing)
  "Return a region list with a single item pointing to the THING at point."
  (list 'region (bounds-of-thing-at-point thing)))

(defun boon-select-from-region (select-fun)
  "Return a region list with a single item: the region selected after calling SELECT-FUN (interactively)."
  (interactive)
  (save-excursion
    ;; FIXME: deactivate mark
    (call-interactively select-fun)
    (list 'region (cons (region-beginning) (region-end)))))

(defun boon-select-wim () ;; what i mean
  "Return a region list with a single item: either the symbol at point, or, if this fails, the sexp at point."
  (interactive)
  (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                    (bounds-of-thing-at-point 'sexp))))
    (list 'region bounds)))

(defun boon-jump-over-blanks-forward ()
  "Jump over blanks, forward."
  (interactive)
  (skip-chars-forward "\n\t "))

(defun boon-jump-over-blanks-backward ()
  "Jump over blanks, backward."
  (interactive)
  (skip-chars-backward "\n\t "))

(defun boon-select-org-table-cell ()
  (interactive)
  (list 'region
        (cons (save-excursion
                (skip-chars-backward "^|") (point))
              (save-excursion
                (skip-chars-forward "^|") (point)))))
(defun boon-select-justline () (interactive) (list 'region (cons (line-beginning-position) (line-end-position))))
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
(defun boon-select-comment () (interactive) (boon-select-from-region 'er/mark-comment))
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
                   (boon-jump-over-blanks-forward)
                   (point)))))


(defun boon-select-borders (how-much regs)
  "Return the bordering (of size HOW-MUCH) of a region list REGS.
This function is meant to be called interactively."
  (interactive (list (prefix-numeric-value current-prefix-arg) (boon-spec-region "select contents")))
  (cons 'region (apply 'append (mapcar (lambda (reg) (boon-borders reg how-much)) (mapcar 'boon-normalize-reg regs)))))

(defun boon-select-with-spaces (regs)
  "Return the regions REGS, including some surrounding spaces.
This function is meant to be called interactively."
  (interactive (list (boon-spec-region "select contents")))
  (cons 'region (mapcar (lambda (reg) (boon-include-surround-spaces reg)) (mapcar 'boon-normalize-reg regs))))

(defun boon-select-content (regs)
  "Return the contents (of size HOW-MUCH) of a region list REGS.
This function is meant to be called interactively."
  (interactive (list (boon-spec-region "select borders")))
  (cons 'region (mapcar 'boon-content (mapcar 'boon-normalize-reg regs))))

(defun boon-bypass-mc ()
  "Should we bypass multiple cursors when gathering regions?"
  (and (bound-and-true-p multiple-cursors-mode)
       (memq this-command mc/cmds-to-run-once)))
  
(defun boon-multiple-cursor-regs ()
  "Return all regions defined by multiple-cursors-mode, and outside."
  (cons (cons (mark) (point))
        (if (boon-bypass-mc)
            ;; TODO: is marker-position really necessary here?
            (mapcar (lambda (o) (cons (marker-position (overlay-get o 'mark)) (marker-position (overlay-get o 'point))))
                    (mc/all-fake-cursors))
          nil)))

(defun boon-read-char (&optional prompt inherit-input-method seconds)
  "Read a character, bypassing multiple cursors defadvice if applicable."
  ;; do this so that mc's read-char defadvice does not kick in; so we can actually read characters here.
  ;; a hack for now: as read-event doesn't do the same thing as read char.
  (if (boon-bypass-mc)
      (read-event prompt inherit-input-method seconds)
    (read-char prompt inherit-input-method seconds)))

(defun boon-spec-region (msg)
  "Specify a region concisely using the keyboard.
The prompt (as MSG) is displayed.  This function actually returns
a list of regions, in the form ((beginning . end) ...).  If
multiple-cursors are enabled BUT the command is executed just
once (not once per cursor), you get a region for each cursor."
  (let ((orig-regs (boon-multiple-cursor-regs)))
    (if (use-region-p)
        orig-regs
    (let (current-prefix-arg
          ;; this code fiddles with the prefix arg; but if we do not
          ;; hide our fiddling, the next command will use the prefix
          ;; arg that we have set. So we dynamically bind another
          ;; current-prefix-arg here.
          (km boon-select-map))
      ;; We read an entry in the appropriate keymap. This is done "by hand."
      (setq current-prefix-arg 0)
      (while (and km (keymapp km))
        (let ((last-char (boon-read-char (format "%s %s" msg current-prefix-arg))))
          (if (and (>= last-char ?0) (<= last-char ?9))
              (setq current-prefix-arg (+ (- last-char ?0) (* 10 current-prefix-arg )))
            (setq km (lookup-key km (vector last-char))))))
      (when (eq current-prefix-arg 0)
        (setq current-prefix-arg nil))
      ;; The command is ready; we now execute it (once per cursor if applicable).
      (if km (apply 'append (mapcar (lambda (in-reg)
                                     (let (regs final (orig (cdr in-reg)))
                                       (save-excursion
                                         (goto-char orig)
                                         (setq regs (call-interactively km))
                                         (setq final (point)))
                                       ;; (message "in-reg=%s regs=%s orig=%s final=%s" in-reg regs orig final)
                                       (if (and regs
                                                (listp regs)
                                                (eq (car regs) 'region))
                                           (cdr regs)
                                         (list (cons orig final)))))
                                   orig-regs))
        (error "Unknown region specifier"))))))

(provide 'boon-arguments)
;;; boon-arguments.el ends here
