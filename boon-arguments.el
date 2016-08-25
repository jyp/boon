;;; boon-arguments.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

 
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
  (lambda ()(boon-regs-from-bounds (bounds-of-thing-at-point thing))))

(defun boon-select-from-region (select-fun)
  "Return a region list with a single item: the region selected after calling SELECT-FUN (interactively)."
  (interactive)
  (save-excursion
    ;; FIXME: deactivate mark
    (call-interactively select-fun)
    (lambda ()(boon-regs-from-bounds (cons (region-beginning) (region-end))))))

(defun boon-select-wim () ;; what i mean
  "Return a region list with a single item: either the symbol at point, or, if this fails, the sexp at point."
  (interactive)
  (lambda ()(let ((bounds (or (bounds-of-thing-at-point 'symbol)
                              (bounds-of-thing-at-point 'sexp))))
              (boon-regs-from-bounds bounds))))

(defun boon-jump-over-blanks-forward ()
  "Jump over blanks, forward."
  (interactive)
  (skip-chars-forward "\n\t "))

(defun boon-jump-over-blanks-backward ()
  "Jump over blanks, backward."
  (interactive)
  (skip-chars-backward "\n\t "))

(defun boon-select-org-table-cell ()
  "Return the region between pipes (|)."
  (interactive)
  (lambda ()(boon-regs-from-bounds
        (cons (save-excursion
                (skip-chars-backward "^|") (point))
              (save-excursion
                (skip-chars-forward "^|") (point))))))

(defun boon-select-justline ()
  "Return the region of the current line, without any newline."
  (interactive) (boon-regs-from-bounds (cons (line-beginning-position) (line-end-position))))

(defun boon-select-line (count)
  "Return a region of COUNT visual lines."
  (interactive "p")
  (setq temporary-goal-column 0)
  (boon-select-n count 'beginning-of-visual-line 'line-move-visual))

(defun boon-select-n (count goto-beginning forward-n)
  "Return a region of COUNT objects defined by GOTO-BEGINNING and FORWARD-N."
  (lambda()(save-excursion
    (funcall goto-beginning)
    (boon-regs-from-bounds (cons (point) (progn (funcall forward-n count) (point)))))))

(defun boon-select-paragraph (count) (interactive "p") (boon-select-n count 'start-of-paragraph-text 'forward-paragraph))
(defun boon-select-document () (interactive) (lambda () (boon-regs-from-bounds (cons (point-min) (point-max)))))
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
  (boon-regs-from-bounds (cons
                 (save-excursion
                   (boon-jump-over-blanks-backward)
                   (point))
                 (save-excursion
                   (boon-jump-over-blanks-forward)
                   (point)))))

(defun boon-spec-string-lazy (prompt)
  "Read a string using the region selection functionality.
Intented to be used as an argument to interactive.
Display PROMPT in the echo area."
  (let ((head (read-event)))
    (if (equal head ? ) (let ((str (read-string (concat prompt ": ")))) (lambda () str))
      ; if space, read a literal string, otherwise use the region specifier.
      (setq unread-command-events (cons head unread-command-events))
      (let ((regs (boon-spec-region-lazy prompt)))
        (lambda ()
          (let ((reg (car (funcall regs))))
            (buffer-substring-no-properties (boon-reg-begin reg) (boon-reg-end reg))))))))

(defun boon-select-occurences (what-fun where)
  "Return the occurences of WHAT as sub-regions of WHERE."
  (interactive (list (boon-spec-string-lazy "occurences of what?") (boon-spec-region-lazy "where?")))
  (lambda ()
    (let ((result nil)
          (what (funcall what-fun)))
      (save-excursion
        (dolist (reg (funcall where))
          (goto-char (boon-reg-begin reg))
          (while (search-forward what (boon-reg-end reg) t)
            (setq result (cons (boon-mk-reg (match-beginning 0)
                                            (match-end 0)
                                            (boon-reg-cursor reg))
                               result))))
        result))))

(defun boon-select-borders (how-much regs)
  "Return the bordering (of size HOW-MUCH) of a region list REGS.
This function is meant to be called interactively."
  (interactive (list (prefix-numeric-value current-prefix-arg) (boon-spec-region-lazy "select contents")))
  (lambda ()(apply 'append (mapcar (lambda (reg) (boon-borders reg how-much)) (mapcar 'boon-normalize-reg (funcall regs))))))

(defun boon-select-with-spaces (regs)
  "Return the regions REGS, including some surrounding spaces.
This function is meant to be called interactively."
  (interactive (list (boon-spec-region-lazy "select with spaces")))
  (lambda ()(mapcar (lambda (reg) (boon-include-surround-spaces reg)) (mapcar 'boon-normalize-reg (funcall regs)))))

(defun boon-select-content (regs)
  "Return the contents (of size HOW-MUCH) of a region list REGS.
This function is meant to be called interactively."
  (interactive (list (boon-spec-region-lazy "select borders")))
  (lambda ()(mapcar 'boon-content (mapcar 'boon-normalize-reg (funcall regs)))))

(defun boon-bypass-mc ()
  "Should we bypass multiple cursors when gathering regions?"
  (and (bound-and-true-p multiple-cursors-mode)
       (memq this-command mc/cmds-to-run-once)))

(defun boon-multiple-cursor-regs ()
  "Return all regions defined by multiple-cursors-mode, and outside."
  (cons (boon-mk-reg (mark) (point) nil)
        (if (boon-bypass-mc)
            ;; TODO: is marker-position really necessary here?
            (mapcar (lambda (o) (boon-mk-reg (marker-position (overlay-get o 'mark)) (marker-position (overlay-get o 'point)) o))
                    (mc/all-fake-cursors)))))

(defun boon-read-char (&optional prompt inherit-input-method seconds)
  "Read a character, bypassing multiple cursors defadvice if applicable."
  ;; do this so that mc's read-char defadvice does not kick in; so we can actually read characters here.
  ;; a hack for now: as read-event doesn't do the same thing as read char.
  (if (boon-bypass-mc)
      (read-event prompt inherit-input-method seconds)
    (read-char prompt inherit-input-method seconds)))

(defun boon-spec-region (msg)
  "Specify a region concisely using the keyboard.
The prompt (as MSG) is displayed.  This function returns a list
of regions (See boon-regs.el).  If multiple-cursors are enabled
BUT 'this-command' is executed just once (not once per
cursor), you get a region for each cursor.
"
  (let ((orig-regs (boon-multiple-cursor-regs)))
    (if (use-region-p) orig-regs
      (let ((selector (boon-spec-region-lazy msg)))
        (apply 'append
               (mapcar (lambda (in-reg)
                         (save-excursion
                           (goto-char (boon-reg-point in-reg))
                           (mapcar (lambda (r) (boon-mk-reg (boon-reg-mark r)
                                                            (boon-reg-point r)
                                                            (boon-reg-cursor in-reg)))
                                   (funcall selector))))
                       orig-regs))))))

(defun boon-spec-region-lazy (msg)
  "Specify a region selector concisely using the keyboard.
The prompt (as MSG) is displayed.  This function returns a
non-interactive function which, when run, will return
bounds. This allows to run the function in question multiple
times (describing the region just once with the keyboard). This
can be useful when having multiple cursors, or just using
descriptors referring to several disjoint subregions.  The bounds
that are eventually returned are in the form of a list of regs,
see boon-regs.el.
"
  (let ((my-prefix-arg 0)
        (kmv boon-moves-map)
        (kms boon-select-map))
    ;; We read a move or selection, in both keymaps in parallel. First command found wins.
    (while (and (or kmv kms) (not (commandp kms)) (not (commandp kmv)))
      (let ((last-char (boon-read-char (format "%s %s" msg my-prefix-arg))))
        (if (and (>= last-char ?0) (<= last-char ?9))
            (setq my-prefix-arg (+ (- last-char ?0) (* 10 my-prefix-arg )))
          (if kms (setq kms (lookup-key kms (vector last-char))))
          (if kmv (setq kmv (lookup-key kmv (vector last-char)))))))
    (when (eq my-prefix-arg 0) (setq my-prefix-arg nil))
    ;; The command is ready; we now execute it (once per cursor if applicable).
    (if (or kms kmv)
        (if (commandp kms)
          ;; we have a 'selection'. These commands may take prefix
          ;; args, which they parse right away, and return a
          ;; continuation constructing the region.
            (let ((current-prefix-arg my-prefix-arg))
              (call-interactively kms))
          ;; we have a 'move'. These commands do not take non-universal arguments. So just run it.
            (lambda ()
              (save-excursion
                (let ((orig (point))
                      (current-prefix-arg my-prefix-arg)) ;; dynamic bindig so env remains clean
                  (call-interactively kmv)
                  (list (boon-mk-reg orig (point) nil))))))
      (error "Unknown region specifier"))))

(provide 'boon-arguments)
;;; boon-arguments.el ends here
