;;; boon-main.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains (most of) the boon commands. These commands are
;; typically bound to a key in boon-keys or boon-colemak.

;;; Code:

(require 'boon-core)
(require 'boon-arguments)

(require 'er-basic-expansions)
(require 'multiple-cursors)
(require 'subr-x)


;;; Jumping to definitions (at point):

(defun boon-find-elisp-thing-at-point ()
  "Find an elisp thing at point.
Search preferentially for a function, then a variable."
  (interactive)
  (let ((symb (symbol-at-point)))
    (cond
     ((fboundp symb) (find-function-do-it symb nil 'switch-to-buffer))
     ((boundp  symb) (find-function-do-it symb 'defvar 'switch-to-buffer))
     (t (call-interactively 'helm-apropos)))))

(defun boon-find-tag-at-point ()
  "Find the symbol at point in the current tags table."
  (interactive)
  (let ((symb (thing-at-point 'symbol)))
    (cond (symb (find-tag symb))
          (t (call-interactively 'find-tag)))))

(defcustom boon-find-definition-dispatch '()
  "An alist mapping major modes to finding the symbol at point."
  :group 'boon
  :type '(alist :key-type symbol :value-type function))
(setq boon-find-definition-dispatch
      '((c-mode . (lambda () (interactive) (boon-find-tag-at-point)))
        (emacs-lisp-mode . boon-find-elisp-thing-at-point)
        (lisp-interaction-mode . boon-find-elisp-thing-at-point)
        (haskell-mode . haskell-mode-jump-to-def-or-tag)))

(defun boon-find-definition ()
  "Find a definition, in a way which is adapted to the 'major-mode'.
If possible, prompt the symbol at point."
  (interactive)
  (let ((mode-fap (assoc major-mode boon-find-definition-dispatch)))
    (if mode-fap (call-interactively (cdr mode-fap))
      (error "Finding definitions is not defined for %s. Update the variable 'boon-find-definition-dispatch'."
             major-mode))))

(defcustom boon-hints-enabled 't "Display hints." :group 'boon)

(defun boon-hint (msg)
  "Provide MSG as a hint."
  (when boon-hints-enabled
    (message msg)))

(defmacro boon-with-ordered-region (body)
  "Run the BODY, ensuring that the point is before the mark."
  `(if (< (point) (mark))
       ,body
       (progn (exchange-point-and-mark) ,body (exchange-point-and-mark))))

(defun boon-drop-cursor ()
  "Drop a new cursor at the first position given by REGS.
NOTE: Do not run for every cursor."
  (interactive)
  (if multiple-cursors-mode
      ;; attempt to deactivate mc without deleting the cursors
      (let ((cursors (mapcar (lambda (o) (cons (overlay-get o 'point) (overlay-get o 'mark)))(mc/all-fake-cursors))))
        ;; does not work because deactivating mc destroys the overlays
        (multiple-cursors-mode 0)
        (dolist (p (cdr cursors))
          (goto-char (car p))
          (set-marker (mark-marker) (cdr p))
          (mc/create-fake-cursor-at-point)))
    (when (and mark-active (> (point) (mark)))
      (exchange-point-and-mark))
    ;; so that searching for the region gives a similar position
    (mc/create-fake-cursor-at-point)))

(defun boon-move-cursor (regs)
  "Move the cursor at the first position given by REGS.
NOTE: Do not run for every cursor."
  (interactive (list (boon-spec-region "cursor")))
  (goto-char (boon-reg-point (car regs))))

(defun boon-drop-or-extend-mark ()
  "Drop a mark; or extend the region to the next full line; or revert to original state."
  (interactive)
  (declare (obsolete "Use boon-drop-mark instead" "20151020"))
  (if mark-active
      (if (and (bolp)
               (save-excursion (goto-char (mark)) (bolp))
               (not (eq (point) (mark))))
          (progn ;; here we have a number of full lines selected, and that number is more than 0  
            (pop-mark) ;; load the saved position into the mark
            (goto-char (mark));; jump there
            (deactivate-mark))
      (boon-with-ordered-region
       (progn ;; here we have at least one non-full line selected. Extend to the full lines. 
         (beginning-of-line)
         (exchange-point-and-mark)
         (end-of-line)
         (forward-char)
         (exchange-point-and-mark))))
    (progn
      (set-mark (point))
      (push-mark) ;; Save the starting position, so we can go back to it.
      (call-interactively 'boon-mark-region))))

(defun boon-deactivate-mark ()
  "Deactivate the mark robustly."
  (mc/execute-command-for-all-fake-cursors (lambda () (interactive) (deactivate-mark)))
  (deactivate-mark t))

(defun boon-drop-mark ()
  "Drop or deactivate the mark."
  (interactive)
  (if mark-active (boon-deactivate-mark)
    (call-interactively 'boon-mark-region)))

(defun boon-current-line-indentation ()
  "Return the indentation of the curent line."
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun boon-enclose (enclosure regs)
  "Wrap with the given ENCLOSURE the regions given as REGS."
  (interactive (list (boon-spec-enclosure) (boon-spec-region "enclose")))
  ;; (message "boon-enclose regs=%s" regs)
  (dolist (reg (mapcar 'boon-reg-to-markers regs))
    (save-excursion
      (goto-char (boon-reg-end reg))
      (insert (cadr enclosure))
      (goto-char (boon-reg-begin reg))
      (insert (car enclosure)))))

(defun boon-find-char-backward (char)
  "Move the cursor backwards, until finding an occurence of the character CHAR."
  (interactive "cType the character to find")
  (search-backward (make-string 1 char))
  (forward-char 1))

(defun boon-find-char-forward (char)
  "Find the given character (as CHAR), forwards."
  (interactive "cType the character to find")
  (search-forward (make-string 1 char))
  (backward-char 1))

(defun boon-edge-of-expression (forward)
  "Jump to the forward or backward (as FORWARD) limit of the current expression."
  (interactive "P")
  (let ((orig-point (point))) 
    (goto-char
     (save-excursion
       (deactivate-mark)
       (if (boon-in-string-p)
           (er/mark-inside-quotes) (er/mark-inside-pairs))
       (when forward (exchange-point-and-mark))
       (point)))
    ;; make sure we make some progress
    (when (eq (point) orig-point)
      (forward-char (if forward 1 -1)))))

(defun boon-end-of-expression ()
  "Jump to the end of the current expression."
  (interactive)
  (boon-edge-of-expression 't))

(defun boon-beginning-of-expression ()
  "Jump to the beginning of the current expression."
  (interactive)
  (boon-edge-of-expression nil))

(defun boon-delete-region ()
  "Delete the region if it is active."
  (when (use-region-p)
    (delete-region (region-beginning) (region-end))))

(defun boon-insert-register ()
  "Insert register, replacing the region if it is active."
  (boon-delete-region)
  (call-interactively 'insert-register))

(defun boon-copy-to-register ()
  "Copy to register and deactivate mark."
  (interactive)
  (call-interactively 'copy-to-register)
  (deactivate-mark))

(defun boon-splice ()
  "Yank, replacing the region if it is active.
When repeated, fix the spacing if necessary."
  (interactive)
  (when (not (and (eq last-command 'yank)
                  (boon-splice-fix-spaces)))
    (progn (boon-delete-region)
           (yank)
           (boon-hint "If spaces are wrong, run boon-splice again."))))

(defun boon-need-space ()
  "Is it necessary to insert a space here to separate words or expressions?"
  (and (not (or (eolp) (looking-at "\\s-")))
       (not (or (bolp) (looking-back "\\s-")))
       (or (and (looking-back "\\sw\\|\\s_") (looking-at "\\sw\\|\\s_"))
           ;; this isn't quite ideal for haskell mode, because special
           ;; characters are defined as punctuations, but there should
           ;; be spaces between operators and identifiers
           (and (looking-back "\\s)") (not (looking-at "\\s)")))
           (and (not (looking-back "\\s(")) (looking-at "\\s(")))))

(defun boon-fix-a-space ()
  "Fix the text to have exactly one space at the point.
Return nil if no changes are made, t otherwise."
  (cond ((and (looking-at " ") (looking-back " "))
         (delete-char 1)
         t)
        ((boon-need-space)
         (insert " ")
         t)
        (t nil)))

(defun boon-splice-fix-spaces ()
  "Yank, replacing the region if it is active.
Fix the surroundings so that they become nicely spaced.
Return nil if no changes are made."
  (interactive)
  (let ((fix-here (boon-fix-a-space))
        (fix-there (save-excursion
                     (goto-char (mark))
                     (boon-fix-a-space))))
    ;; done this way because 'or' is lazy
    (or fix-here fix-there)))

(defun boon-line-prefix ()
  "Return the text between beginning of line and point."
  (buffer-substring-no-properties
   (line-beginning-position)
   (point)))

(defun boon-line-suffix ()
  "Return the text between end of line and point."
  (buffer-substring-no-properties
   (line-end-position)
   (point)))

(defun boon-at-indent-or-more-p ()
  "Return non-nil if the point is at the current line indentation; or to the right."
  (or (eolp)
      (and (not (boon-at-indent-p))
           (string-blank-p (boon-line-prefix)))))

(defun boon-at-indent-p ()
  "Return non-nil if the point is at the current line indentation."
(eq (save-excursion (back-to-indentation) (point)) (point)))

(defun boon-smarter-upward (count)
  "Move upward, to a line with the same level of indentation, or less, COUNT times."
  (interactive "p")
  (back-to-indentation)
  (dotimes (_number count)
    (previous-logical-line)
    (while (boon-at-indent-or-more-p) (previous-logical-line)))
  (back-to-indentation))

(defun boon-smarter-downward (count)
  "Move downward, to a line with the same level of indentation, or less COUNT times."
  (interactive "p")
  (back-to-indentation)
  (dotimes (_number count)
    (next-logical-line)
    (while (boon-at-indent-or-more-p) (next-logical-line)))
  (back-to-indentation))

(defun boon-smarter-backward (count)
  "Move backward, over COUNT whole syntactic units."
  (interactive "p")
  (dotimes (_number count)
    (boon-jump-over-blanks-backward)
    (cond
     ((boon-looking-at-comment -1)
      (forward-comment -1))
     ((looking-back "\\s\"")
      (backward-char)
      (er--move-point-backward-out-of-string))
     ((looking-back "\\s)")
      (backward-list))
     ((looking-back "\\s_")  ;; symbol
      (skip-syntax-backward "_"))
     ((looking-back "\\s(")
      (backward-char))
     ((looking-back "\\s!")  ;; generic comment delimiter
      (skip-syntax-backward "!"))
     ((looking-back "\\sw")
      (if (not (looking-at "\\(\\s-\\|\\s(\\|\\s)\\)"))
          (skip-syntax-backward "w")
        (skip-syntax-backward "w_")))
     (t
      (backward-char)))))

(defun boon-smarter-forward (count)
  "Move forward, over COUNT whole syntactic unit."
  (interactive "p")
  (dotimes (_number count)
    (boon-jump-over-blanks-forward)
    (cond
     ((boon-looking-at-line-comment-start-p)
      (end-of-line)
      (boon-jump-over-blanks-forward))
     ((boon-looking-at-comment 1);;
      (forward-comment 1))
     ((looking-at "\\s\"")
      (forward-char)
      (er--move-point-forward-out-of-string))
     ((looking-at "\\s(")
      (forward-list))
     ((looking-at "\\s_") ;; symbol 
      (skip-syntax-forward "_"))
     ((looking-at "\\s)")
      (forward-char))
     ((looking-at "\\s!")  ;; generic comment delimiter
      (skip-syntax-forward "!"))
     ((looking-at "\\sw")
      (if (not (looking-back "\\(\\s-\\|\\s(\\|\\s)\\)"))
          (skip-syntax-forward "w")
        (skip-syntax-forward "w_")))
     (t
      (forward-char)))))

(defun boon-smarter-forward-spaces (count)
  "Move forward, over COUNT whole syntactic unit.
Handle spaces cleverly."
  (interactive "p")
  (declare (obsolete "does not seem very useful" "20151120"))
  (dotimes (_number count)
    (let ((spaces-skipped (not (equal (boon-jump-over-blanks) 0)))
          (in-middle nil)
          (at-bol (string-blank-p (boon-line-prefix))))
      (cond
       ((boon-looking-at-line-comment-start-p)
        (end-of-line)
        (forward-char))
       ((boon-looking-at-comment 1);;
        (forward-comment 1))
       ((looking-at "\\s\"")
        (forward-char)
        (er--move-point-forward-out-of-string))
       ((looking-at "\\s(")
        (forward-list))
       ((looking-at "\\s_") ;; symbol
        (skip-syntax-forward "_"))
       ((looking-at "\\s)")
        (forward-char)
        (setq in-middle 't))
       ((looking-at "\\s!")  ;; generic comment delimiter
        (skip-syntax-forward "!"))
       ((looking-at "\\sw")
        (setq in-middle 't)
        (if (not (looking-back "\\(\\s-\\|\\s(\\|\\s)\\)"))
            (skip-syntax-forward "w")
          (skip-syntax-forward "w_")))
       (t
        (forward-char)
        (setq in-middle 't)))
      (unless (or spaces-skipped in-middle)
        (if at-bol
            (skip-chars-forward "\t\n ")
          (skip-chars-forward "\t "))))))

(defun boon-smarter-backward-spaces (count)
  "Move backward, over COUNT whole syntactic unit.
Handles spaces smartly."
  (interactive "p")
  (declare (obsolete "does not seem very useful" "20151120"))
  (dotimes (_number count)
    (let ((spaces-skipped (not (equal (boon-jump-over-blanks-backward) 0)))
          (in-middle nil)
          (at-eol (string-blank-p (boon-line-suffix))))
      (cond
       ((boon-looking-at-comment -1)
        (forward-comment -1))
       ((looking-back "\\s\"")
        (backward-char)
        (er--move-point-backward-out-of-string))
       ((looking-back "\\s)")
        (backward-list))
       ((looking-back "\\s_")  ;; symbol
        (skip-syntax-backward "_"))
       ((looking-back "\\s(")
        (backward-char)
        (setq in-middle 't))
       ((looking-back "\\s!")  ;; generic comment delimiter
        (skip-syntax-backward "!"))
       ((looking-back "\\sw")
        (setq in-middle 't)
        (if (not (looking-at "\\(\\s-\\|\\s(\\|\\s)\\)"))
            (skip-syntax-backward "w")
          (skip-syntax-backward "w_")))
       (t
        (backward-char)
        (setq in-middle 't)))
      (unless (or spaces-skipped in-middle)
        (if at-eol
            (skip-chars-backward "\t\n ")
          (skip-chars-backward "\t "))))))

(defun boon-toggle-character-case ()
  "Toggle the case of the character at point."
  (interactive)
  (let ((case-fold-search nil))
    (if (looking-at "[[:upper:]]")
        (progn
          (downcase-region (point) (+ (point) 1)))
      (progn
        (upcase-region (point) (+ (point) 1))))))

(defun boon-toggle-case ()
  "Toggle the case of the character at point, or cycle the case of the region if it is active."
  (interactive)
  (if (use-region-p)
      (call-interactively 'boon-toggle-region-case)
      (boon-toggle-character-case)))

(defun boon-toggle-region-case (regs)
  "Cycle regions through 3 capitalizations: UPPER CASE, lower case, Title Case.
Regions are given by  REGS.
NOTE: Do not run for every cursor."
  (interactive (list (boon-spec-region "toggle-case")))
  (let* ((deactivate-mark nil)
         (case-fold-search nil)
         (cur-state (if (eq last-command this-command)
                        (get this-command 'state)
                      (save-excursion
                        (goto-char (boon-reg-begin (car regs)))
                        (cond
                         ((looking-at "[[:upper:]][[:upper:]]") 'upcase-region)
                         ((looking-at "[[:upper:]][[:lower:]]") 'capitalize-region)
                         (t 'downcase-region))))))
    (setq cur-state (cdr (assoc cur-state '((downcase-region . capitalize-region)
                                            (capitalize-region . upcase-region)
                                            (upcase-region . downcase-region)
                                            ))))
    (dolist (reg regs)
      (funcall cur-state (boon-reg-begin reg) (boon-reg-end reg)))
    (put this-command 'state cur-state)))

(defun boon-toggle-mark ()
  "Toggle region activation."
  (interactive)
  (if mark-active
      (deactivate-mark)
      (when (eq (point) (mark))
          (message "mark placed at point"))
      (activate-mark)))

(defun boon-visible-beginning-of-line ()
  "Move point leftwards to the first visible beginning of line."
  (interactive)
  (beginning-of-line)
  (while (outline-invisible-p)
    (backward-char 1)
    (beginning-of-line 1)))

(defun boon-beginning-of-line ()
  "Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (when (or (outline-invisible-p) (= oldpos (point)))
      (boon-visible-beginning-of-line))))

(defun boon-looking-at-comment (how-many)
  "Is the current point looking at HOW-MANY comments? (negative for backwards)?"
  (save-excursion
    (forward-comment how-many)))

(defun boon-in-string-p ()
  "Determine if the point is inside a string."
  (nth 3 (syntax-ppss)))

(defun boon-looking-at-line-comment-start-p ()
  "Are we looking at a comment-start?"
  (interactive)
  (and (bound-and-true-p comment-start)
       (looking-at comment-start)
       (not (boon-in-string-p))))

(defun boon-end-of-line ()
  "Intelligently jump to the end of line.
This function toggles between jumping to 1. the last character of code on the
line 2. the last non-blank char on the line 3. the true end of
line."
  (interactive)
  (let* ((orig (point))
         (orig-eol (eolp))
         (progress (lambda () (and (not (bolp)) (or orig-eol (> (point) orig))))))
    (beginning-of-line)
    (while (not (or (boon-looking-at-line-comment-start-p) (eolp))) 
      (forward-char))
    ;; we're now at the last non-comment character of the line
    (skip-chars-backward "\n\t " (line-beginning-position))
    ;; we're now at the last non-blank, non-comment character of the line
    (unless (funcall progress)
      (end-of-line)
      (skip-chars-backward "\n\t " (line-beginning-position))
      ;; we're now at the last non-blank character of the line
      (unless (funcall progress)
        (end-of-line)))))

(defun boon-open-line-and-insert ()
  "Open a new line, indented as much as the current one, and switch to insert mode."
  (interactive)
  (let ((indent-lvl (boon-current-line-indentation))) 
      (beginning-of-line)
      (open-line 1)
      (insert (make-string indent-lvl 32))
      (boon-set-insert-state)))
(defun boon-open-next-line-and-insert ()
  "Open the line after the current one."
  (interactive)
  (next-logical-line)
  (boon-open-line-and-insert))

(defun boon-open-line ()
  "Open the line before the current one."
  (interactive)
  (save-excursion
    (let ((line-prefix (boon-line-prefix)))
      ;; (message "next-line-prefix is %S" next-line-prefix)
      (open-line 1)
      (when (string-blank-p line-prefix)
        (progn
          (forward-char 1)
          (insert line-prefix))))))

(defun boon-switch-mark ()
  "If mark active, switch point and mark, otherwise pop mark from mark ring."
  (interactive)
    (if mark-active
      (exchange-point-and-mark)
      (progn
        (goto-char (mark))
        (pop-mark))))

(defun boon-switch-mark-quick ()
  "Pop the mark ring until we find ourselves on a different line."
  (interactive)
  (let ((orig-line (line-number-at-pos)))
    (while (> 1 (abs (- orig-line (line-number-at-pos))))
      (goto-char (mark))
      (pop-mark))))

(defun boon-split-line ()
  "Split the current line."
  (interactive)
  (let ((indent-col (min (boon-current-line-indentation) (current-column))))
    ;; kill the extra spaces
    (save-excursion
      (delete-region (progn
                       (skip-chars-forward "\n\t " (line-end-position))
                       (point))
                     (progn
                       (skip-chars-backward "\n\t " (line-beginning-position))
                       (point))))
    (newline)
    (insert (make-string indent-col ?\ ))))

(defun boon-newline-dwim ()
  "Insert a new line do-what-i-mean style."
  (interactive)
  (if (and (not (eolp)) (string-blank-p (boon-line-prefix)))
      (call-interactively 'boon-open-line)
    (boon-split-line)))

(defun boon-lay-multiple-cursors (place-cursor regs)
  "Create multiple cursor regions.
This is done by calling PLACE-CURSOR for each element of REGS.
If there is more than one, use mc/create-fake-cursor-at-point."
  (mc/remove-fake-cursors)
  (dolist (reg (cdr regs))
    (funcall place-cursor reg)
    (mc/create-fake-cursor-at-point))
  (funcall place-cursor (car regs))
  (mc/maybe-multiple-cursors-mode))

(defun boon-mark-region (regs)
  "Mark the regions REGS."
  (interactive (list (boon-spec-region "mark")))
  (boon-lay-multiple-cursors (lambda (reg)
                               (set-mark (boon-reg-mark reg))
                               (goto-char (boon-reg-point reg))) regs)
  (activate-mark))

(defun boon-end-of-region (regs)
  "Move the point the end region REGS."
  (interactive (list (boon-spec-region "go to end")))
  (dolist (reg regs)
    (goto-char (boon-reg-end reg))))

(defun boon-beginning-of-region (regs)
  "Move the point to the beginning region REGS."
  (interactive (list (boon-spec-region "go to beginning")))
  (dolist (reg regs)
    (goto-char (boon-reg-begin reg))))

(defun boon-take-region (regs)
  "Kill the region given as REGS."
  (interactive (list (boon-spec-region "take")))
  (dolist (reg (sort regs 'boon-reg-after)
               ; Wrong: (mapcar 'boon-reg-to-markers regs)
               ;; We can't run 'kill-region' on markers. Indeed, using
               ;; markers messes the logic used in kill-region to
               ;; determine whether to prepend or append the thing
               ;; just killed to the top of the kill ring.  So, we
               ;; sort the regions by latest first, so that killing
               ;; does not affect the positions of the next regions in
               ;; the list.
               )
    ;; (message "Taking: %s %s" reg (< (boon-reg-point reg) (boon-reg-mark reg)))
    (kill-region (boon-reg-mark reg) (boon-reg-point reg))))

(defun boon-treasure-region (regs)
  "Copy (kill-ring-save) the regions REGS."
  (interactive (list (boon-spec-region "treasure")))
  (dolist (reg regs)
    (kill-ring-save (boon-reg-begin reg) (boon-reg-end reg))))

(defun boon-substitute-region (regs)
  "Kill the regions REGS, and switch to insertion mode."
  (interactive (list (boon-spec-region "replace")))
  (let ((markers (mapcar 'boon-reg-to-markers regs)))
    ;; use markers so that deleting things does not mess the positions
    (boon-take-region regs)
    (deactivate-mark t)
    (boon-lay-multiple-cursors (lambda (reg)
                                 (goto-char (boon-reg-point reg)))
                               markers)
    (boon-set-insert-state)))

(defun boon-replace-by-character (replacement)
  "Replace the character at point, or the region if it is active, by the REPLACEMENT character."
  (interactive (list (read-char)))
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char 1))
  (insert replacement))

(defun boon-quote-character (char)
  "Execute the command bound to the character CHAR if boon was not enabled."
  (interactive (list (read-char))) ;; use read-char so that multiple-cursors advice kicks in.
  (let ((cmd
         (or (and (current-local-map) (lookup-key (current-local-map) (vector char)))
             (lookup-key (current-global-map) (vector char)))))
    (setq last-command-event char)
    (message (format "Executing the command bound to %c" char))
    (call-interactively cmd nil [char])))

(defun boon-unhighlight ()
  "Pop a highlight regexp."
  (interactive)
  (when (bound-and-true-p hi-lock-interactive-patterns)
    (hi-lock-unface-buffer (car (car hi-lock-interactive-patterns)))))

(defun boon-quit ()
  "Exit the current modes we're in until no special state is remaining.
NOTE: do not run for every cursor."
  (interactive)
  (cond
   ((and (boundp multiple-cursors-mode)
         (not multiple-cursors-mode)
         (> (mc/num-cursors) 1))
    (multiple-cursors-mode 1)
    (message "Activated multiple cursors. Repeat this command to deactivate."))
   ((use-region-p)
    (boon-deactivate-mark)
    (message "Deactivated region (use ' to reactivate)"))
   ((bound-and-true-p multiple-cursors-mode)
    (message "Exitted from multiple cursors")
    (multiple-cursors-mode 0))
   ((bound-and-true-p hi-lock-interactive-patterns)
    (message "Removed highlighting")
    (boon-unhighlight))
   (t
    (keyboard-quit))))

(defun boon-stuff-at-point ()
  "Return a meaningful piece of text around at point.
If no such text exists, throw an error."
  (interactive)
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (or (thing-at-point 'symbol)
          (error "Nothing relevant at point; move to a symbol or select a region"))))

;; TODO: remove
(require 'skeleton)
(setq skeleton-pair t)

(defun boon-empty-pair-p ()
  "Is the point at the middle of an empty pair of matched parens?"
  (interactive)
  (declare (obsolete "emacs 24.5 electric pair mode is good enough" "20150527"))
  (eq (caddr
            (assq (preceding-char)
             (or skeleton-pair-alist skeleton-pair-default-alist)))
           (following-char)))

(defun boon-empty-quotes-p ()
  "Is the point in the middle of an empty pair of quotes?"
  (interactive)
  (declare (obsolete "emacs 24.5 electric pair mode is good enough" "20150527"))
  (and (eq (preceding-char) (following-char))
       (member (following-char) '(?\" ?\'))))

(defun boon-smart-insert-backspace2 ()
  (interactive)
  (declare (obsolete "emacs 24.5 electric pair mode is good enough" "20150527"))
  (when (or (boon-empty-pair-p) (boon-empty-quotes-p))
    (delete-char 1))
  (backward-delete-char-untabify 1))

(defun boon-self-insert-quote ()
  "Insert doubled quote.
unless: 1. the previous character is a backslash, in which case a
  single quote is inserted or 2. the next character is a quote in
  which case the cursor simply jumps over it."
  (interactive)
  (declare (obsolete "emacs 24.5 electric pair mode is good enough" "20150527"))
  (cond
   ((equal (this-command-keys) (make-string 1 (following-char)))
    (forward-char 1))
   ((eq (preceding-char) ?\\)
    (self-insert-command 1))
   (t
    (self-insert-command 2)
    (backward-char 1))))

(provide 'boon-main)
;;; boon-main.el ends here

