;;; boon-main.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains boon actions (modification of text).  These
;; commands are typically bound to a key in boon-command-map.  They
;; can be bound to any desired key though (in global-map as well).

;;; Code:

(require 'boon-core)
(require 'boon-utils)
(require 'boon-arguments)
(require 'boon-hl)
(require 'multiple-cursors)
(require 'subr-x)
(require 'dash)

;;;###autoload
(defun boon-set-insert-like-state (&optional changes)
  "Switch to special or insert state, depending on mode.
When CHANGES are non-nil, replay those instead."
  (interactive)
  (boon-interactive-insert)
  (if (boon-special-mode-p)
      (boon-set-special-state)
    (boon-insert changes)))

;;;###autoload
(defun boon-insert (&optional changes)
  "Switch to insert state.
When CHANGES are non-nil, replay those instead."
  (interactive)
  (boon-interactive-insert)
  (if changes ;; replay changes if we have them, otherwise switch to insert state normally
      (progn
        (mc/execute-command-for-all-fake-cursors (lambda () (interactive) (boon/replay-changes changes)))
        (boon/replay-changes changes))
    (boon-set-insert-state)))

;;;###autoload
(defun boon-repeat-command (count)
  "Repeat the most recent command in the history, COUNT times."
  (interactive "p")
  (let ((cmd (car command-history)))
    (dotimes (_ count)
      (apply #'funcall-interactively
             (car cmd)
             (mapcar (lambda (e) (eval e t)) (cdr cmd))))))

;;;###autoload
(defun boon-deactivate-mark ()
  "Deactivate the mark robustly."
  (mc/execute-command-for-all-fake-cursors (lambda () (interactive) (deactivate-mark)))
  (deactivate-mark t))

;;;###autoload
(defun boon-drop-mark ()
  "Drop or deactivate the mark."
  (interactive)
  (if mark-active (boon-deactivate-mark)
    (call-interactively 'boon-mark-region)))

;;;###autoload
(defun boon-enclose (enclosure regs)
  "Wrap with the given ENCLOSURE the regions given as REGS."
  (interactive (list (boon-spec-enclosure) (boon-spec-select-top "enclose")))
  ;; (message "boon-enclose regs=%s" regs)
  (dolist (reg (mapcar 'boon-reg-to-markers (boon-run-selector regs)))
    (save-excursion
      (goto-char (boon-reg-end reg))
      (insert (cadr enclosure))
      (goto-char (boon-reg-begin reg))
      (insert (car enclosure)))))

;;;###autoload
(defun boon-delete-region ()
  "Delete the region if it is active."
  (when (use-region-p)
    (delete-region (region-beginning) (region-end))))

;;;###autoload
(defun boon-insert-register ()
  "Insert register, replacing the region if it is active."
  (boon-delete-region)
  (call-interactively 'insert-register))

;;;###autoload
(defun boon-copy-to-register ()
  "Copy to register and deactivate mark."
  (interactive)
  (call-interactively 'copy-to-register)
  (deactivate-mark))

;;;###autoload
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

;;;###autoload
(defun boon-need-space ()
  "Is it necessary to insert a space here to separate words or expressions?"
  (let ((back-limit (1- (point))))
  (and (not (or (eolp) (looking-at "\\s-") (looking-at "\\s)")))
       (not (or (bolp) (looking-back "\\s-" back-limit) (looking-back "\\s(" back-limit)))
       (or (and (looking-back "\\sw\\|\\s_\\|\\s.\\|\\s)" back-limit) (looking-at "\\sw\\|\\s_\\|\\s("))))))

;;;###autoload
(defun boon-fix-a-space ()
  "Fix the text to have the right amout of spacing at the point.
Return nil if no changes are made, t otherwise."
  (interactive)
  (let ((back-limit (1- (point))))
  (cond ((bolp) nil) ;; inserted at least one full line.
        ((looking-at " ")
         (when (or (bolp) (looking-back "\\s-\\|\\s(" back-limit))
           (delete-char 1)
           t))
        ((looking-back " " back-limit)
         (when (or (eolp) (looking-at "\\s-\\|\\s)\\|\\s."))
           (delete-char -1)
           t))
        ((boon-need-space)
         (insert " ")
         t)
        (t nil))))

;;;###autoload
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

;;;###autoload
(defun boon-toggle-character-case ()
  "Toggle the case of the character at point."
  (interactive)
  (let* ((case-fold-search nil)
         (action (if (looking-at "[[:upper:]]") 'downcase-region 'upcase-region)))
    (dolist (p (mapcar (lambda (r) (boon-reg-point r)) (boon-multiple-cursor-regs)))
      (funcall action p (1+ p)))))

;;;###autoload
(defun boon-toggle-case ()
  "Toggle the case of the character at point, or cycle the case of the region if it is active."
  (interactive)
  (if (use-region-p)
      (call-interactively 'boon-toggle-region-case)
      (boon-toggle-character-case)))

;;;###autoload
(defun boon-toggle-region-case (regs)
  "Cycle regions through 3 capitalizations: UPPER CASE, lower case, Title Case.
Regions are given by  REGS."
  (interactive (list (boon-spec-select-top "toggle-case")))
  (let* ((deactivate-mark nil)
         (case-fold-search nil)
         (cur-state (if (eq last-command this-command)
                        (get this-command 'state)
                      (save-excursion
                        (goto-char (boon-reg-begin (car (boon-run-selector regs))))
                        (cond
                         ((looking-at "[[:upper:]][[:upper:]]") 'upcase-region)
                         ((looking-at "[[:upper:]][[:lower:]]") 'capitalize-region)
                         (t 'downcase-region))))))
    (setq cur-state (cdr (assoc cur-state '((downcase-region . capitalize-region)
                                            (capitalize-region . upcase-region)
                                            (upcase-region . downcase-region)
                                            ))))
    (dolist (reg (boon-run-selector regs))
      (funcall cur-state (boon-reg-begin reg) (boon-reg-end reg)))
    (put this-command 'state cur-state)))

;;;###autoload
(defun boon-toggle-mark ()
  "Toggle region activation."
  (interactive)
  (if mark-active
      (deactivate-mark)
      (when (eq (point) (mark))
          (message "mark placed at point"))
      (activate-mark)))

;;;###autoload
(defun boon-open-line-and-insert ()
  "Open a new line, indented as much as the current one, and switch to insert mode."
  (interactive)
  (let ((indent-lvl (boon-current-line-indentation)))
      (beginning-of-line)
      (open-line 1)
      (insert (make-string indent-lvl 32))
      (boon-set-insert-state)))

;;;###autoload
(defun boon-open-next-line-and-insert ()
  "Open the line after the current one."
  (interactive)
  (save-excursion
    (end-of-line)
    (when (eq (point) (point-max))
      (insert "\n")))
  (forward-line)
  (boon-open-line-and-insert))

;; alternative:
;; (defalias 'boon-open-line 'crux-smart-open-line-above)
;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun boon-newline-dwim ()
  "Insert a new line do-what-i-mean style."
  (interactive)
  (if (and (not (eolp)) (<= (boon-col-relative-to-indent) 0))
      (call-interactively 'boon-open-line)
    (boon-split-line)))

;;;###autoload
(defun boon-lay-multiple-cursors (place-cursor regs)
  "Create multiple cursor regions.
This is done by calling PLACE-CURSOR for each element of REGS.
If there is more than one, use mc/create-fake-cursor-at-point."
  (mc/remove-fake-cursors)
  (dolist (reg (cdr regs)) ; 1st region handled specially (not a fake cursor, but real one)
    (funcall place-cursor reg)
    (mc/create-fake-cursor-at-point))
  (funcall place-cursor (car regs))
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun boon-mark-region (regs)
  "Mark the regions REGS."
  (interactive (list (boon-spec-select-top "mark")))
  (boon-lay-multiple-cursors (lambda (reg)
                               (set-mark (boon-reg-mark reg))
                               (goto-char (boon-reg-point reg)))
                             (boon-run-selector regs))
  (activate-mark))

(defun boon-execute-for-cursor (cursor fun)
  "In the context of the fake CURSOR, run FUN."
  (if cursor
        (mc/save-excursion
         (mc/save-window-scroll
          (mc/execute-command-for-fake-cursor (lambda () (interactive)(funcall fun)) cursor)))
    (funcall fun)))

;;;###autoload
(defun boon-exchange (regs)
  (interactive (list (boon-spec-select-top "exchange")))
  (let ((last-command nil)) ;; we never want to append to the last kill here
    (boon-take-region regs)) 
  (insert-for-yank (nth 1 kill-ring))
  (save-excursion
    (goto-char (nth 0 mark-ring))
    (insert-for-yank (nth 0 kill-ring))))

;;;###autoload
(defun boon-take-region (regs)
  "Kill the region given as selector REGS."
  (interactive (list (boon-spec-select-top "take")))
  (boon-take-region-0 (boon-run-selector regs)))

(defun boon-take-region-0 (regs)
  "Kill the region given as REGS."
  (unless boon-selected-by-move (setq last-command 'not-a-kill))
  (dolist (reg-group
           ;; work cursor-by-cursor, so that cursor-specific tricks in mc package work.
           (-partition-by 'boon-reg-cursor
                          ;; convert to markers, so that deleting text does not mess with
                          ;; positions
                          (-map #'boon-reg-to-markers regs)))
    (boon-execute-for-cursor
     (boon-reg-cursor (car reg-group))
     (lambda ()
       ;; We can't run 'kill-region' on markers. Indeed, using
       ;; markers messes the logic used in kill-region to
       ;; determine whether to prepend or append the thing
       ;; just killed to the top of the kill ring.
       (dolist (reg (-map #'boon-reg-from-markers
                          ;; to later regions first so that killing
                          ;; does not invalidate positions
                          (-sort #'boon-reg-after reg-group)))
         ;; push mark first, because kill-region will move text
         (push-mark (boon-reg-mark reg) t nil)
         (kill-region (boon-reg-mark reg) (boon-reg-point reg)))))))

;;;###autoload
(defun boon-treasure-region (regs)
  "Copy (kill-ring-save) the regions REGS."
  (interactive (list (boon-spec-select-top "treasure")))
  (dolist (reg (boon-run-selector regs))
    (kill-ring-save (boon-reg-begin reg) (boon-reg-end reg))))

;;;###autoload
(defun boon-substitute-region (reg-sel &optional changes)
  "Kill the regions REGS, and switch to insertion mode or replay CHANGES."
  (interactive (list (boon-spec-select-top "replace")))
  (boon-interactive-insert reg-sel)
  
  (let* ((regs (boon-run-selector reg-sel))
         (markers (mapcar 'boon-reg-to-markers (-sort 'boon-reg-before regs))))
    ;; Sort so that the changes recorded will be relative to a consistent position.
    ;; (The actual cursor will be 1st and will not jump around).
    ;; Use markers so that deleting things does not mess the positions
    (boon-take-region-0 regs)
    (deactivate-mark t)
    (if changes ; if we have a change to apply then we do not want to lay new cursors, just apply the changes.
        (save-excursion
          (dolist (reg markers)
            (goto-char (boon-reg-point reg))
            (boon/replay-changes changes)))
      (boon-lay-multiple-cursors (lambda (reg) (goto-char (boon-reg-point reg)))
                                 markers)
      (boon-insert nil))))

;;;###autoload
(defun boon-replace-by-character (replacement)
  "Replace the character at point by the REPLACEMENT character.
Replace the region if it is active."
  (interactive (list (read-char)))
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char 1))
  (insert replacement))

;;;###autoload
(defun boon-quote-character (char)
  "Execute the command which were bound to the character CHAR if boon was not enabled."
  (interactive (list (read-char))) ;; use read-char so that multiple-cursors advice kicks in.
  (let* ((keymap (make-composed-keymap
                  (let ((boon-mode-map-alist nil)) (current-active-maps))))
         (cmd (lookup-key keymap (vector char)))
         (cmd (or (command-remapping cmd nil keymap)
                  cmd)))
    (setq last-command-event char)
    (setq this-command cmd)
    (message "Executing the command bound to %c" char)
    (call-interactively cmd nil [char])))


;;;###autoload
(defun boon-unhighlight (&optional n)
  "Pop N highlighted patterns, by calling `boon-hl-remove'."
  (interactive "p")
  (dotimes (_ (or n 1))
    (boon-hl-remove (car boon-hl-patterns))))

;;;###autoload
(defun boon-quit ()
  "Exit the current modes we're in until no special state is remaining."
  (interactive)
  (cond
   ((and (boundp multiple-cursors-mode)
         (not multiple-cursors-mode)
         (> (mc/num-cursors) 1))
    (multiple-cursors-mode 1)
    ;; this branch is obsolete as 20160902: to cursor should be laid without activating mc's
    (message "Activated multiple cursors. Repeat this command to deactivate."))
   ((use-region-p)
    (boon-deactivate-mark)
    (message "Deactivated region (use ' to reactivate)"))
   ((bound-and-true-p multiple-cursors-mode)
    (message "Exitted from multiple cursors")
    (multiple-cursors-mode 0))
   (boon-hl-patterns
    (message "Removed highlighting")
    (boon-unhighlight))
   (t
    (keyboard-quit))))

(defun boon-god-control-swap (event)
  "Swap the control 'bit' in EVENT, unless C-c <event> is a prefix reserved for modes."
  (interactive (list (read-key)))
  (cond
   ((memq event '(9 13 ?{ ?} ?\[ ?\] ?$ ?& ?= ?< ?> ?: ?\; ?/ ?? ?. ?, ?' ?` ?\" )) event)
   ((<= event 27) (+ 96 event))
   ((not (eq 0 (logand (lsh 1 26) event))) (logxor (lsh 1 26) event))
   (t (list 'control event))))

;;;###autoload
(defun boon-c-god (arg)
  "Input a key sequence, prepending C- to each key (unless such
key is already reserved for minor mode, see
`boon-god-control-swap'), and run the command bound to that
sequence."
  (interactive "P")
  (let ((keys '((control c)))
        (binding (key-binding (kbd "C-c")))
        (key-vector (kbd "C-c"))
        (prompt "C-c-"))
    (while (and binding
                (or (eq binding 'mode-specific-command-prefix)
                    ;; if using universal prefix, the above will happen.
                    (not (commandp binding))))
      (let ((key (read-key (format "%s" prompt))))
        (if (eq key ?h) (describe-bindings key-vector) ;; h -> show help
          (push (boon-god-control-swap key) keys)
          (setq key-vector (vconcat (reverse keys)))
          (setq prompt (key-description key-vector))
          (setq binding (key-binding key-vector)))))
    (cond
     ((not binding) (error "No command bound to %s" prompt))
     ((commandp binding)
      (setq this-command binding)
      (let ((current-prefix-arg arg)) (call-interactively binding)))
     (t (error "Key not bound to a command: %s" binding)))))

;;;###autoload
(defun boon-adjust-indent ()
 "Adjust indentation of the region or current line."
 (interactive)
 (unless (use-region-p)
   (set-mark (line-beginning-position))
   (end-of-line))
 (call-interactively 'indent-rigidly))

;;;###autoload
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

;;;###autoload
(defun boon-toggle-comment (regs)
  "Toggle comments in the regions REGS."
  (interactive (list (boon-spec-select-top "toggle comment")))
  (dolist (reg (boon-run-selector regs))
    (comment-or-uncomment-region (boon-reg-begin reg)(boon-reg-end reg))))

;;;###autoload
(defun boon-narrow (regs)
  "Narrow to the first region of REGS."
  (interactive (list (boon-spec-select-top "narrow")))
  (let ((reg (car (boon-run-selector regs))))
    (narrow-to-region (boon-reg-begin reg) (boon-reg-end reg))))


(provide 'boon-main)
;;; boon-main.el ends here

