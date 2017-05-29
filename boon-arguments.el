;;; boon-arguments.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This file defines functions which are intended to be used as
;; 'interactive' specifications: `boon-spec-select-top' and
;; `boon-spec-enclosure'.  These are used by boon commands, but can be
;; used by any commands.
;;
;; In this module can also be found functions which are bound in
;; `boon-select-map'.  Those functions return a no-argument lambda which
;; returns a list of boon-regs.

;;; Code:

(require 'boon-core)
(require 'boon-regs)
(require 'boon-utils)
(require 'multiple-cursors)
(require 'dash)

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
        (?m . ("`" "'")) ;; mixed
        (?o . ("⟦" "⟧")) ;; oxford brackets
        (?p . ("(" ")"))
        (?q . ("'" "'"))
        (?r . ("{" "}")) ;; bRaces
        (?t . ("~" "~")) ;; tilda
        )
        "Enclosures to use with the `boon-enclose' command."
        :type '(alist :key-type character :value-type (group (string :tag "Open ") (string :tag "Close")))
        :group 'boon)

(defun boon-spec-enclosure ()
  "Specify an enclosure style.  To be used as an argument to interactive."
  (let* ((c (read-char "Specify the enclosure"))
         (s (make-string 1 c))
         (choice (assoc c boon-enclosures)))
    (if choice (cdr choice) (list s s))))

(defun boon-select-from-region (select-fun)
  "Return a region list with a single item: the region selected after calling SELECT-FUN (interactively)."
  (lambda ()
    (save-mark-and-excursion
     (call-interactively select-fun)
     (boon-regs-from-bounds (cons (region-beginning) (region-end))))))

(defun boon-select-wim () ;; what i mean
  "Return a region list with a single item.
This item is either the symbol at point, or, if this fails, the sexp at point."
  (interactive)
  (lambda () (boon-regs-from-bounds (or (bounds-of-thing-at-point 'symbol)
                                        (bounds-of-thing-at-point 'sexp)))))

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
  "Return a selector of COUNT visual lines."
  (interactive "p")
  (setq temporary-goal-column 0)
  (boon-select-n count 'visible-line))

(defun boon-select-n (count thing)
  "Return a region of COUNT THING's."
  (lambda() (save-excursion
              (let ((bnds (bounds-of-thing-at-point thing)))
                (goto-char (cdr bnds))
                (forward-thing thing (1- count))
                (list (boon-mk-reg (car bnds) (point)))))))

(defun boon-select-n-copies (count thing)
  "Return list of regions with COUNT copies of the THING."
  (lambda() (save-mark-and-excursion
              (let* ((bnds (bounds-of-thing-at-point thing))
                     (what (buffer-substring-no-properties (car bnds) (cdr bnds)))
                     (local-count count)
                     ;; local-count variable necessary because the argument
                     ;; is shared between all calls to this closure.
                     (result nil))
                (goto-char (car bnds))
                (while (and (> local-count 0) (search-forward what nil t))
                  (setq local-count (1- local-count))
                  (push (boon-mk-reg (match-beginning 0)
                                     (match-end 0))
                        result))
                result))))

(defun boon-select-document () (interactive) (lambda () (boon-regs-from-bounds (cons (point-min) (point-max)))))
(defun boon-select-paragraph      (count) (interactive "p") (boon-select-n count 'paragraph))
(defun boon-select-word           (count) (interactive "p") (boon-select-n-copies count 'word))
(defun boon-select-sentence       (count) (interactive "p") (boon-select-n count 'sentence))
(defun boon-select-symbol         (count) (interactive "p") (boon-select-n-copies count 'symbol))
(defun boon-select-list           (count) (interactive "p") (boon-select-n count 'list))
(defun boon-select-sexp           (count) (interactive "p") (boon-select-n count 'sexp))
(defun boon-select-whitespace     (count) (interactive "p") (boon-select-n count 'whitespace))
(defun boon-select-outside-pairs  () (interactive) (boon-select-from-region 'er/mark-outside-pairs))
(defun boon-select-comment        () (interactive) (boon-select-from-region 'er/mark-comment))
(defun boon-select-inside-pairs   () (interactive) (boon-select-from-region 'er/mark-inside-pairs))
(defun boon-select-outside-quotes () (interactive) (boon-select-from-region 'er/mark-outside-quotes))
(defun boon-select-blanks ()
  "Select the blanks around the point, including newlines and tabs."
  (interactive)
  (lambda ()(boon-regs-from-bounds (cons
                                    (save-excursion
                                      (boon-jump-over-blanks-backward)
                                      (point))
                                    (save-excursion
                                      (boon-jump-over-blanks-forward)
                                      (point))))))
(defun boon-select-block ()
  "Select the lines contiguous with the current line and have same indentation or more."
  (interactive)
  (lambda ()
    (boon-regs-from-bounds
     (save-excursion
       (back-to-indentation)
       (setq temporary-goal-column (current-column))
       (cons
        (save-excursion
          (while (and (not (bolp)) (<= (boon-col-relative-to-indent) 0))
            (previous-logical-line))
          (next-logical-line)
          (beginning-of-line)
          (point))
        (save-excursion
          (while (and (not (bolp)) (<= (boon-col-relative-to-indent) 0))
            (next-logical-line))
          (beginning-of-line)
          (point)))))))

(defun boon-spec-string-lazy (prompt)
  "Read a string using the region selection functionality.
Intented to be used as an argument to interactive.  Returns a
lambda that returns a string.  Display PROMPT in the echo
area.  Reads a selector and evaluate the selector to fetch a
buffer substring to return.  If the character read is a space,
then ask for the string interactively instead."
  (let ((head (read-event prompt)))
    (if (equal head ? ) (let ((str (read-string (concat prompt ": ")))) (lambda () str))
      ; if space, read a literal string, otherwise use the region specifier.
      (setq unread-command-events (cons head unread-command-events))
      (let ((regs (boon-spec-selector prompt)))
        (lambda ()
          (let ((reg (car (funcall regs))))
            (buffer-substring-no-properties (boon-reg-begin reg) (boon-reg-end reg))))))))

(defun boon-select-occurences (what-fun where)
  "Return the occurences of WHAT-FUN as sub-regions of WHERE."
  (interactive (list (boon-spec-string-lazy "occurences of what?") (boon-spec-selector "where?")))
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

(defun boon-select-all (what where)
  "Return a list of empty regions starting at the WHAT subregions of WHERE.
Example: r#<spc>p places a cursor at every begining of line in
the region, in insertion mode.  Subregions won't be overlapping."
  (interactive (list (boon-spec-selector "what?") (boon-spec-selector "where?")))
  (lambda ()
    (let ((result nil))
      (save-excursion
        (dolist (reg (funcall where))
          (goto-char (boon-reg-begin reg))
          (while (and (< (point) (boon-reg-end reg)))
            (let ((subregs (-remove 'boon-reg-nil
                                    (-filter (lambda (r) (> (boon-reg-end r) (point)))
                                             (funcall what)))))
              ;; some selectors may return nil. (for exmaple sexp on a non-sexp, etc.)
              (setq result (append (mapcar (lambda (r) (boon-mk-reg (boon-reg-mark r)
                                                                    (boon-reg-mark r)))
                                           subregs) result))
              (goto-char (apply 'max (+ 1 (point)) (mapcar 'boon-reg-end subregs))))))
        result))))

(defun boon-select-borders (how-much regs)
  "Return the bordering (of size HOW-MUCH) of a region list REGS."
  (interactive (list (prefix-numeric-value current-prefix-arg) (boon-spec-selector "select contents")))
  (lambda ()(apply 'append (mapcar (lambda (reg) (boon-borders reg how-much)) (funcall regs)))))

(defun boon-select-with-spaces (regs)
  "Return the regions REGS, including some surrounding spaces on one side."
  (interactive (list (boon-spec-selector "select with spaces")))
  (lambda ()(mapcar (lambda (reg) (boon-include-surround-spaces reg)) (funcall regs))))

(defun boon-select-content (regs)
  "Return the contents (of size HOW-MUCH) of a region list REGS."
  (interactive (list (boon-spec-selector "select borders")))
  (lambda ()(mapcar 'boon-content (funcall regs))))

(defun boon-bypass-mc ()
  "Should we bypass multiple cursors when gathering regions?"
  (and (bound-and-true-p multiple-cursors-mode)
       (or (memq this-command mc--default-cmds-to-run-once)
           (memq this-command mc/cmds-to-run-once))))

(defun boon-multiple-cursor-regs ()
  "Return the selected region and those defined by `multiple-cursors-mode'."
  (cons (boon-mk-reg (mark) (point) nil)
        (if (boon-bypass-mc)
            ;; TODO: is marker-position really necessary here?
            (mapcar (lambda (o) (boon-mk-reg (marker-position (overlay-get o 'mark)) (marker-position (overlay-get o 'point)) o))
                    (mc/all-fake-cursors)))))

(defun boon-spec-select-top (msg)
  "Like (`boon-spec-selector' MSG), but select the region if it is active."
  (if (use-region-p) 'orig-regs (boon-spec-selector msg)))

(defun boon-run-selector (selector)
  "Return the list of regions specified by SELECTOR.
See boon-regs.el for return type.  If `multiple-cursors' are
enabled BUT `this-command' is executed just once (not once per
cursor), you get a region for each cursor."
  (if (eq selector 'orig-regs) (boon-multiple-cursor-regs)
    (-mapcat (lambda (in-reg)
               (save-excursion
                 (goto-char (boon-reg-point in-reg))
                 (-map (lambda (r) (boon-mk-reg (boon-reg-mark r)
                                                (boon-reg-point r)
                                                (boon-reg-cursor in-reg)))
                       (funcall selector))))
             (boon-multiple-cursor-regs))))

(defvar boon-selected-by-move nil
  "Non nil if the last selection was made by a move, nil otherwise.
When killing, if a selection is made by a move, it makes sense to
aggregate the region in the killring, but not so if it was made
by a 'true' selector.")

(defun boon-spec-selector (msg)
  "Specify a region selector concisely using the keyboard.
MSG is displayed as prompt.  This function returns a
non-interactive function which, when run, will return bounds.
This indirection allows to run the function in question multiple
times, without further interaction.  This is useful when having
multiple cursors, when using descriptors referring to several
subregions or when repeating a command.  The bounds that are
eventually returned are in the form of a list of regs.  See
boon-regs.el."
  (let ((my-prefix-arg 0)
        (kmv boon-moves-map)
        (kms boon-select-map))
    ;; We read a move or selection, in both keymaps in parallel. First command found wins.
    (while (and (or kmv kms) (not (commandp kms)) (not (commandp kmv)))
      (let ((last-char (read-event (format "%s %s" msg my-prefix-arg))))
        ;; read-event, because mc badly advises read-char
        (if (and (integerp last-char) (>= last-char ?0) (<= last-char ?9))
            (setq my-prefix-arg (+ (- last-char ?0) (* 10 my-prefix-arg )))
          (if kms (setq kms (lookup-key kms (vector last-char))))
          (if kmv (setq kmv (lookup-key kmv (vector last-char)))))))
    (when (eq my-prefix-arg 0) (setq my-prefix-arg nil))
    ;; The command is ready; we now execute it (once per cursor if applicable).
    (if (or kms kmv)
        (prog1
            (if (commandp kms)
          ;; we have a 'selector'. These commands may take prefix
          ;; args, which they input right away, and return a
          ;; continuation constructing the region depending on the
          ;; point/mark.
            (let ((current-prefix-arg my-prefix-arg))
              (call-interactively kms))
          ;; we have a 'move' command. Such commands do not take
          ;; non-universal arguments. So just run it in the
          ;; continuation.
            (lambda ()
              (save-excursion
                (let ((orig (point))
                      (current-prefix-arg my-prefix-arg)) ;; dynamic bindig so env remains clean
                  (call-interactively kmv)
                  (list (boon-mk-reg orig (point) nil))))))
          (setq boon-selected-by-move (not (commandp kms))))
      (error "Unknown selector"))))

(provide 'boon-arguments)
;;; boon-arguments.el ends here
