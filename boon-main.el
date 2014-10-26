;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(require 'boon-core)
(require 'boon-arguments)

(require 'er-basic-expansions)

(defmacro boon-with-ordered-region (body)
  `(if (< (point) (mark)) 
       ,body
       (progn (exchange-point-and-mark) ,body (exchange-point-and-mark))))

(defun boon-drop-mark ()
  "Drop a mark; or extend the region to the next full line; or revert to original state."
  (interactive)
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

(defun boon-current-line-indentation ()
  "Return the indentation of the curent line."
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun boon-enclose (enclosure regs)
  "Wrap, with the ENCLOSURE the regions given as REGS."
  (interactive (list (boon-spec-enclosure) (boon-spec-region "enclose")))
  (dolist (reg regs)
    (let ((beg (min (cdr reg) (car reg)))
          (end (max (cdr reg) (car reg))))
      (cond
       (enclosure
        (save-excursion
          (goto-char end)
          (insert (cadr enclosure))
          (goto-char beg)
          (insert (car enclosure))))
       (t (message "unknown enclosure"))))))



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
  (interactive)
  (boon-edge-of-expression 't))

(defun boon-beginning-of-expression ()
  (interactive)
  (boon-edge-of-expression nil))

(defun boon-extract-region ()
  (when (use-region-p)
    (delete-and-extract-region (region-beginning) (region-end))))

(defun boon-insert-register ()
  "Insert register, replacing the region if it is active."
  (boon-extract-region)
  (call-interactively 'insert-register))

(defun boon-copy-to-register ()
  "Copy to register and deactivate mark"
  (interactive)
  (call-interactively 'copy-to-register)
  (deactivate-mark))

(defun boon-splice ()
  "Yank, replacing the region if it is active."
  (interactive)
  (boon-extract-region)
  (yank))

(defun boon-line-prefix ()
  "return the text between beginning of line and position"
  (buffer-substring-no-properties
   (line-beginning-position) 
   (point)))

(defun boon-at-indent-or-more-p ()
  "return non-nil if the point is at the current line
indentation; or to the right."
  (or (eolp)
      (and (not (boon-at-indent-p))
           (boon-blank-string-p (boon-line-prefix)))))

(defun boon-at-indent-p ()
  "return non-nil if the point is at the current line
indentation"
(eq (save-excursion (back-to-indentation) (point)) (point)))

(defun boon-smarter-upward ()
  "move upward, to a line with the same level of indentation, or less"
  (interactive)
  (back-to-indentation)
  (previous-logical-line)
  (while (boon-at-indent-or-more-p) (previous-logical-line))
  (back-to-indentation))

(defun boon-smarter-downward ()
  "move downward, to a line with the same level of indentation, or less"
  (interactive)
  (back-to-indentation)
  (next-logical-line)
  (while (boon-at-indent-or-more-p) (next-logical-line))
  (back-to-indentation))

(defun boon-smarter-backward ()
  "move backward, over a whole syntactic unit"
  (interactive)
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
      (if (not (or (looking-at "\\s-") ;; FIXME: merge regexps with \\|
                   (looking-at "\\s(")
                   (looking-at "\\s)")))
          (skip-syntax-backward "w")
        (skip-syntax-backward "w_")))
     (t
      (backward-char)))
    )

(defun boon-smarter-forward ()
  (interactive)
    (boon-jump-over-blanks)
    (cond
     ((boon-looking-at-line-comment-start-p)
      (end-of-line)
      (boon-jump-over-blanks))
     ((boon-looking-at-comment 1);;
      (forward-comment 1))
     ((looking-at "\\s\"")
      (forward-char)
      (er--move-point-forward-out-of-string))
     ((looking-at "\\s(")
      ;; (message "open paren")
      (forward-list))
     ((looking-at "\\s_") ;; symbol 
      (skip-syntax-forward "_"))
     ((looking-at "\\s)") 
      (forward-char))
     ((looking-at "\\s!")  ;; generic comment delimiter
      ;; (message "generic")
      (skip-syntax-forward "!"))
     ((looking-at "\\sw") 
      (if (not (or (looking-back "\\s-")
                   (looking-back "\\s(")
                   (looking-back "\\s)")))
          (skip-syntax-forward "w")
        (skip-syntax-forward "w_")))
     (t 
      (forward-char)))
    ;; (when (and no-spaces-skipped (not in-middle)) 
    ;;   (skip-chars-forward "\t\n "))
    )


(defun boon-toggle-character-case ()
  "Toggle the case of the character at point"
  (interactive)
  (let ((case-fold-search nil))
    (if (looking-at "[[:upper:]]")
        (progn
          (downcase-region (point) (+ (point) 1)))
      (progn
        (upcase-region (point) (+ (point) 1))))))

(defun boon-toggle-case ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'boon-toggle-region-case)
      (boon-toggle-character-case)))

(defun boon-toggle-region-case (pos1 pos2)
  "Cycles the region between 3 capitalizations: UPPER CASE, lower case, Title Case"
  (interactive "r")
  (let* ((deactivate-mark nil)
         (case-fold-search nil)
         (cur-state (if (eq last-command this-command)
                        (get this-command 'state)
                      (save-excursion
                        (goto-char pos1)
                        (cond
                         ((looking-at "[[:upper:]][[:upper:]]") 'upcase-region)
                         ((looking-at "[[:upper:]][[:lower:]]") 'capitalize-region)
                         (t 'downcase-region))))))
    (setq cur-state (cdr (assoc cur-state '((downcase-region . capitalize-region)
                                            (capitalize-region . upcase-region)
                                            (upcase-region . downcase-region)
                                            ))))
    (funcall cur-state pos1 pos2)
    (put this-command 'state cur-state)))

(defun boon-toggle-mark ()
  "Toggle region activation"
  (interactive)
  (if mark-active
      (deactivate-mark)
      (when (eq (point) (mark))
          (message "mark placed at point"))
      (activate-mark)))

(defun boon-beginning-of-line ()
  "Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line."
  (interactive)
  (let ((oldpos (point)))
    (beginning-of-line)
    (when (= oldpos (point))
      (back-to-indentation))))

(defun boon-looking-at-comment (how-many)
  "Is the current point looking at 'how many' comments? (negative for backwards)"
  (save-excursion
    (forward-comment how-many)))

(defun boon-in-string-p ()
  "Determine if the point is inside a string"
  (nth 3 (syntax-ppss)))

(defun boon-looking-at-line-comment-start-p ()
  "Are we looking at a comment-start? (and not in a string)"
  (interactive)
  (and (boundp 'comment-start)
       comment-start
       (looking-at comment-start)
       (not (boon-in-string-p))))

(defun boon-end-of-line ()
  "Toggle between jumping to 1. the last character of code on the
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

(defun boon-blank-string-p (string)
  "Is the argument composed only of spaces and other blank characters?"
  (equal "" (replace-regexp-in-string "[[:space:]]" "" string)))

(defun boon-open-line-and-insert ()
  "Open a new line, indented as much as the current one, and switch to insert mode."
  (interactive)
  (let ((indent-lvl (boon-current-line-indentation))) 
      (beginning-of-line)
      (open-line 1)
      (insert (make-string indent-lvl 32))
      (boon-set-insert-state)))
(defun boon-open-next-line-and-insert ()
  (interactive)
  (next-logical-line)
  (boon-open-line-and-insert))

(defun boon-open-line ()
  (interactive)
  (save-excursion
    (let ((line-prefix (boon-line-prefix)))
      ;; (message "next-line-prefix is %S" next-line-prefix)
      (open-line 1)
      (when (boon-blank-string-p line-prefix)
        (progn
          (forward-char 1)
          (insert line-prefix))))))

(defun boon-switch-mark ()
  "if mark active, switch point and mark. Otherwise pop mark from mark ring."
  (interactive)
    (if mark-active
      (exchange-point-and-mark)
      (progn
        (goto-char (mark))
        (pop-mark))))

(defun boon-switch-mark-quick ()
  "Pop marks until we find ourselves on a different line"
  (interactive)
  (let ((orig-line (line-number-at-pos)))
    (while (> 1 (abs (- orig-line (line-number-at-pos))))
      (goto-char (mark))
      (pop-mark))))

(defun boon-newline-dwim ()
  "insert a new line do-what-i-mean style"
  (interactive)
  (if (and (not (eolp)) (boon-blank-string-p (boon-line-prefix)))
      (call-interactively 'boon-open-line)
    (boon-split-line)))      
(defun boon-mark-region (regs)
  (interactive (list (boon-spec-region "mark")))
  (dolist (reg regs)
    (set-mark (car reg))
    (goto-char (cdr reg)))
  (activate-mark))

(defun boon-end-of-region (regs)
  "Move the point the end region REGS."
  (interactive (list (boon-spec-region "go to end")))
  (dolist (reg regs)
    (goto-char (cdr reg))))

(defun boon-beginning-of-region (regs)
  "Move the point to the beginning region REGS."
  (interactive (list (boon-spec-region "go to beginnig")))
  (dolist (reg regs)
    (goto-char (car reg))))

(defun boon-take-region (regs)
  "Kill the region given as REGS."
  (interactive (list (boon-spec-region "take")))
  (dolist (reg regs)
    (kill-region (car reg) (cdr reg))))


(defun boon-swap-region (regs)
  "Swap the region with the top of the kill ring"
  (interactive (list (boon-spec-region "swap")))
  (dolist (reg regs)
    (kill-region (car reg) (cdr reg)))
  (insert-for-yank (current-kill 1))
  
  (save-excursion
    (goto-char (car mark-ring))
    (insert-for-yank (current-kill -1)))
  )
  
(defun boon-treasure-region (regs)
  (interactive (list (boon-spec-region "treasure")))
  (dolist (reg regs)
    (kill-ring-save (car reg) (cdr reg))))

(defun boon-substitute-region (regs)
  (interactive (list (boon-spec-region "replace")))
  (boon-take-region regs)
  (boon-set-insert-state))

(defun boon-replace-character (replacement)
  "Replace the character at point"
  (interactive "cType the character to use as a replacement")
  (if (use-region-p)
      (delete-and-extract-region (region-beginning) (region-end ))
    (delete-char 1))
  (insert replacement))

(defun boon-quote-character (char)
  "Execute the 1-character command which would be executed if boon was not enabled."
  (interactive "cThe character to insert or command to execute")
  (let ((cmd
         (or (and (current-local-map) (lookup-key (current-local-map) (make-string 1 char)))
             (lookup-key (current-global-map) (make-string 1 char)))))
    (setq last-command-event char)
    (message (format "Executing the command bound to %c" char))
    (call-interactively cmd nil [char])))
    
(defun boon-quit ()
  "Exit the current modes we're in until no special state is
remaining"
  (interactive)
  (cond
   ((use-region-p)
    (message "Deactivated region (use ' to reactivate)")
    (deactivate-mark))
   ((bound-and-true-p multiple-cursors-mode)
    (message "Exitted from multiple cursors")
    (multiple-cursors-mode 0))
   ((bound-and-true-p hi-lock-interactive-patterns)
    (message "Removed highlighting")
    (boon-unhighlight))
   (t
    ;; (message "Already in command mode; doing keyboard quit")
    (keyboard-quit))))

(defun boon-stuff-at-point ()
  (interactive)
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (or (thing-at-point 'symbol)
          (error "Nothing relevant at point; move to a symbol or select a region"))))
  (require 'skeleton)
  (setq skeleton-pair t)
  (electric-pair-mode)
;; (define-key boon-insert-map "\'" 'self-insert-quote)

(defun boon-empty-pair-p ()
  "Is the point at the middle of an empty pair of matched parens?"
  (interactive)
  (eq (caddr
            (assq (preceding-char)
             (or skeleton-pair-alist skeleton-pair-default-alist)))
           (following-char)))

(defun boon-empty-quotes-p ()
  "Is the point in the middle of an empty pair of quotes?"
  (interactive)
  (and (eq (preceding-char) (following-char))
       (member (following-char) '(?\" ?\'))))

(defun boon-smart-insert-backspace2 ()
  (interactive)
  (when (or (boon-empty-pair-p) (boon-empty-quotes-p))
    (delete-char 1))
  (backward-delete-char-untabify 1))

(defun boon-self-insert-quote ()
  "Insert doubled quote.
unless: 1. the previous character is a backslash, in which case a
  single quote is inserted or 2. the next character is a quote in
  which case the cursor simply jumps over it."
  (interactive)
  (cond
   ((equal (this-command-keys) (make-string 1 (following-char)))
    (forward-char 1))
   ((eq (preceding-char) ?\\)
    (self-insert-command 1))
   (t
    (self-insert-command 2)
    (backward-char 1))))


(defun boon-on-region (f)
 "apply the argument to the current region"
   (funcall f (region-beginning) (region-end)))

(provide 'boon-main)          
;;; boon-main ends here

