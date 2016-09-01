;;; boon-main.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains boon actions (modification of text).  These
;; commands are typically bound to a key in boon-command-map.  They
;; can be bound to any desired key though (in global-map as well).

;;; Code:

(require 'boon-core)
(require 'boon-utils)
(require 'boon-arguments)
(require 'multiple-cursors)
(require 'subr-x)
(require 'dash)


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

(defun boon-splice (number-of-copies)
  "Yank NUMBER-OF-COPIES times, replacing the region if it is active.
When repeated, fix the spacing if necessary."
  (interactive "p")
  (unless (and (eq number-of-copies 1)
               (eq last-command 'yank)
               (boon-splice-fix-spaces))
    (boon-delete-region)
    (dotimes (_ number-of-copies) (yank))
    (boon-hint "If spaces are wrong, run boon-splice again.")))

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

(defun boon-toggle-character-case ()
  "Toggle the case of the character at point."
  (interactive)
  (let* ((case-fold-search nil)
         (action (if (looking-at "[[:upper:]]") 'downcase-region 'upcase-region)))
    (dolist (p (mapcar (lambda (r) (boon-reg-point r)) (boon-multiple-cursor-regs)))
      (funcall action p (1+ p)))))

(defun boon-toggle-case ()
  "Toggle the case of the character at point, or cycle the case of the region if it is active."
  (interactive)
  (if (use-region-p)
      (call-interactively 'boon-toggle-region-case)
      (boon-toggle-character-case)))

(defun boon-toggle-region-case (regs)
  "Cycle regions through 3 capitalizations: UPPER CASE, lower case, Title Case.
Regions are given by  REGS."
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

;; alternative:
;; (defalias 'boon-open-line 'crux-smart-open-line-above)
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

(defun boon-execute-for-cursor (cursor fun)
  "In the context of the fake CURSOR, run FUN."
  (if cursor
        (mc/save-excursion
         (mc/save-window-scroll
          (mc/execute-command-for-fake-cursor (lambda () (interactive)(funcall fun)) cursor)))
    (funcall fun)))

(defun boon-take-region (regs)
  "Kill the region given as REGS."
  (interactive (list (boon-spec-region "take")))
  ;; convert to markers, so that deleting text does not mess with
  ;; positions
  (dolist (reg-group (-partition-by 'boon-reg-cursor (mapcar 'boon-reg-to-markers regs)))
    (boon-execute-for-cursor (boon-reg-cursor (car reg-group))
     (lambda ()
       (dolist (reg (mapcar 'boon-reg-from-markers reg-group))
         ;; We can't run 'kill-region' on markers. Indeed, using
         ;; markers messes the logic used in kill-region to
         ;; determine whether to prepend or append the thing
         ;; just killed to the top of the kill ring.
         (kill-region (boon-reg-mark reg) (boon-reg-point reg)))))))

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
  "Exit the current modes we're in until no special state is remaining."
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

(defun boon-god-control-swap (event)
  "Swap the control 'bit' in EVENT, if that is a good choice."
  (interactive (list (read-key)))
  (cond
   ((memq event '(9 13)) event)
   ((<= event 27) (+ 96 event))
   ((not (eq 0 (logand (lsh 1 26) event))) (logxor (lsh 1 26) event))
   (t (list 'control event))))

(defun boon-c-god ()
  "Input a key sequence, prepend C- to each key, and run the command bound to that sequence."
  (interactive)
  (let ((keys '((control c)))
        (binding (key-binding (kbd "C-c")))
        (key-vector (kbd "C-c"))
        (prompt "C-c-"))
    (while (and binding (not (symbolp binding)))
      (let ((key (read-key (format "%s" prompt))))
        (if (eq key ?h) (describe-bindings key-vector)
          (push (boon-god-control-swap key) keys)
          (setq key-vector (vconcat (reverse keys)))
          (setq prompt (key-description key-vector))
          (setq binding (key-binding key-vector)))))
    (setq this-command-keys key-vector)
    (cond
     ((not binding) (error "No command bound to %s" prompt))
     ((commandp binding) (call-interactively binding))
     (t (error "Key not bound to a command: %s" binding)))))

(defun boon-adjust-indent ()
 "Adjust indentation of the region or current line."
 (interactive)
 (unless (use-region-p)
   (set-mark (line-beginning-position))
   (end-of-line))
 (call-interactively 'indent-rigidly))

(defun boon-query-replace ()
  "Query replace; but if the region is active, replace its contents."
  (interactive)
  (if (and (use-region-p) (eq (- (line-number-at-pos (region-end)) (line-number-at-pos (region-beginning))) 0))
      (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
      (perform-replace
       selection
       (read-string "Replace region with:")
       t ; query
       nil ; not a regexp
       nil ; not delimited
       nil ; no specific repeat count
       nil ; default keymap
       (point-min-marker)
       (point-max-marker) ; replace in the whole buffer
       ))
    (call-interactively 'query-replace)))

(defun boon-toggle-comment (regs)
  "Toggle comments in the regions REGS."
  (interactive (list (boon-spec-region "toggle comment")))
  (dolist (reg regs)
    (comment-or-uncomment-region (boon-reg-begin reg)(boon-reg-end reg))))

(provide 'boon-main)
;;; boon-main.el ends here

