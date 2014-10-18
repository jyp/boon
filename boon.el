;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; fix swap-region
;; change helm-command-map to call stuff from the (overridden) helm-map 
;; implement helm command mode as a translation map
;;  h (help) C-c ?
;; 
;;
;; When 24.4 rolls out:
;;    use string-blank-p
;;    bind rectangle-mark-mode
;;    checkout electric-pair-mode options (http://www.masteringemacs.org/articles/2013/12/29/whats-new-in-emacs-24-4/)
;;
;; Enclosure " "textRET for arbitrary enclosures
;; Region specifier ' (former selected region)
;; Bind c to mode-specific things (instead of x c)
;; Command d should work on the selection if any
;; When inserting a newline; remove spaces before and after the point.
;; Support Fundamental buffers (see evil code)
;; Repeat "last command" (bind on return?)

;;; Code:

;; Require
(require 'thingatpt)
(require 'er-basic-expansions)

;; General config
(delete-selection-mode 0)

;; helm configuration
(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "C-l")        'previous-history-element)
     (define-key helm-map (kbd "C-;")        'next-history-element)
     (define-key helm-map (kbd "C-u")        'helm-previous-line)
     (define-key helm-map (kbd "C-y")        'helm-next-line)
     (define-key helm-map (kbd "C-,")        'helm-previous-page)
     (define-key helm-map (kbd "C-.")        'helm-next-page)
     (define-key helm-map [(tab)]            'helm-select-action)
     (define-key helm-map (kbd "C-z")        'undefined)
     (define-key helm-map [(control return)] 'helm-execute-persistent-action)
     (define-key helm-map [(escape)] 'boon-helm-set-command-state)

     ;; This won't be needed with emacs 24.4 (helm uses set-transient-map)
     (defun helm-maybe-update-keymap ()
       "Handle differents keymaps in multiples sources.
This function is meant to be run in `helm-move-selection-after-hook'.
It will override `helm-map' with the keymap attribute of current source
if some when multiples sources are present."
       (with-helm-window
         (let* ((source (helm-get-current-source))
                (kmap (and (listp source) ; Check if source is empty.
                           (assoc-default 'keymap source))))
           (when kmap (set-temporary-overlay-map kmap)))))
     ))

(defvar boon-helm-command-map)
(setq boon-helm-command-map
      (let ((map (make-sparse-keymap)))
        (suppress-keymap map 't)
        ;; "w": widen selection (occur > multi-occur > git grep > ...)
        (define-key map (kbd "f")    'helm-follow-mode)

        (define-key map (kbd "r")    'helm-yank-selection)
        (define-key map (kbd "s")        'next-history-element) ;; has the effect of getting the whole symbol at point
        (define-key map (kbd "t")        'helm-yank-text-at-point)
        (define-key map (kbd "d")        'helm-delete-minibuffer-contents)

        (define-key map (kbd "z")        'helm-select-3rd-action)
        (define-key map (kbd "x")        'helm-select-2nd-action)
        (define-key map (kbd "c")        'helm-exit-minibuffer)
        (define-key map (kbd "v") 'boon-helm-set-insert-state)
        (define-key map (kbd "b")        'helm-execute-persistent-action)

        (define-key map (kbd "SPC") 'boon-helm-set-insert-state)
        (define-key map (kbd "<RET>")      'helm-exit-minibuffer)
        (define-key map (kbd "<tab>")        'helm-select-action)
        (define-key map (kbd "C-<RET>")        'helm-execute-persistent-action)

        (define-key map (kbd "K")        'helm-toggle-all-marks)
        (define-key map (kbd "M")        'helm-mark-all)
        ;; (define-key map (kbd "")        'helm-unmark-all) ;; use M K for this
        (define-key map (kbd "M-SPC")      'helm-toggle-visible-mark)
        (define-key map (kbd "'")      'helm-toggle-visible-mark)

        (define-key map (kbd "l")        'previous-history-element)
        (define-key map (kbd ";")        'next-history-element)
        (define-key map (kbd "y")     'helm-next-line)
        (define-key map (kbd "u")       'helm-previous-line)
        (define-key map (kbd "U")        'helm-previous-source)
        (define-key map (kbd "Y")        'helm-next-source)
        (define-key map (kbd ",")        'helm-previous-page)
        (define-key map (kbd ".")        'helm-next-page)
        (define-key map (kbd "C-y")      'helm-scroll-other-window)
        (define-key map (kbd "C-u")      'helm-scroll-other-window-down)
        (define-key map (kbd "m")        'helm-prev-visible-mark)
        (define-key map (kbd "/")        'helm-next-visible-mark)
        (define-key map (kbd ">")        'helm-goto-next-file)
        (define-key map (kbd "<")        'helm-goto-precedent-file)

        (define-key map (kbd "C-<down>")        'helm-narrow-window)
        (define-key map (kbd "C-<up>")        'helm-enlarge-window)
        (define-key map [(escape)] 'helm-keyboard-quit)
        map
        ))

(defvar-local boon-helm-command-state nil
  "non-nil if the helm command mode is active. Makes sense only
  in a helm minibuffer.")

(defun boon-helm-set-insert-state ()
  (interactive)
  (setq boon-helm-command-state nil)
  (setq cursor-type 'bar))

(defun boon-helm-set-command-state ()
  (interactive)
  (setq boon-helm-command-state t)
  (setq cursor-type 'box))


;; Various helper functions and commands

(defun boon-find-char-backward (char)
  "find the given character, backwards"
  (interactive "cType the character to find")
  (search-backward (make-string 1 char))
  (forward-char 1))

(defun boon-find-char-forward (char)
  "find the given character, forwards"
  (interactive "cType the character to find")
  (search-forward (make-string 1 char))
  (backward-char 1))

(defun boon-edge-of-expression (forward)
  "Jump to the forward or backward limit of the current expression"
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

(defun boon-jump-over-blanks ()
  (interactive)
  (skip-chars-forward "\n\t "))

(defun boon-jump-over-blanks-backward ()
  (interactive)
  (skip-chars-backward "\n\t "))

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

(defun boon-at-indent-or-more-p ()
  "return non-nil if the point is at the current line
indentation; or to the right."
  (or (eolp)
      (and (not (boon-at-indent-p))
           (blank-string-p (boon-line-prefix)))))

(defun boon-at-indent-p ()
  "return non-nil if the point is at the current line
indentation"
(eq (save-excursion (back-to-indentation) (point)) (point)))

(defun smarter-upward ()
  "move upward, to a line with the same level of indentation, or less"
  (interactive)
  (back-to-indentation)
  (previous-logical-line)
  (while (boon-at-indent-or-more-p) (previous-logical-line))
  (back-to-indentation))

(defun smarter-downward ()
  "move downward, to a line with the same level of indentation, or less"
  (interactive)
  (back-to-indentation)
  (next-logical-line)
  (while (boon-at-indent-or-more-p) (next-logical-line))
  (back-to-indentation))

(defun smarter-backward ()
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

(defun smarter-forward ()
  (interactive)
    (boon-jump-over-blanks)
    (cond
     ((looking-at-line-comment-start-p)
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

(defun boon-unindent-rigidly (beg end count)
  "opposite of `indent-rigidly'"
  (interactive "r\np")
  (indent-rigidly beg end (- count)))

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

(defun boon-toggle-comment (regs)
  "Toggle comments in the region"
  (interactive (boon-spec-region "toggle comment"))
  (dolist (reg regs)
    (comment-or-uncomment-region (min (car reg) (cdr reg))
                                 (max (car reg) (cdr reg)))))

(defun boon-line-prefix ()
  "return the text between beginning of line and position"
  (buffer-substring-no-properties
   (line-beginning-position) 
   (point)))

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

(defun looking-at-line-comment-start-p ()
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
    (while (not (or (looking-at-line-comment-start-p) (eolp))) 
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

(defun blank-string-p (string)
  "Is the argument composed only of spaces and other blank characters?"
  (equal "" (replace-regexp-in-string "[[:space:]]" "" string)))

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

(defun boon-query-replace ()
  "Query replace; but if the region is active, replace its contents"
  (interactive)
  (if (and (use-region-p) (eq (- (line-number-at-pos (region-end)) (line-number-at-pos (region-beginning))) 0))
      (let ((selection (on-region #'buffer-substring-no-properties))) 
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

(defun boon-replace-character (replacement)
  "Replace the character at point"
  (interactive "cType the character to use as a replacement")
  (delete-char 1)
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

(defun boon-prepare-mark ()
  (unless (use-region-p) (call-interactively 'boon-mark-region))
  (when (not (bound-and-true-p multiple-cursors-mode))
    (when (> (mark) (point))
      (exchange-point-and-mark)
      ;; this is to work-around a bug in multiple cursors,
      ;; where the currently marked things is unmarked if the point is after the mark.
      )))

(defun boon-mark-next-like-this ()
  (interactive)
  (boon-prepare-mark)
  (call-interactively 'mc/mark-next-like-this))

(defun boon-mark-previous-like-this ()
  (interactive)
  (boon-prepare-mark)
  (call-interactively 'mc/mark-previous-like-this))

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
  (interactive (cons prefix-arg (boon-spec-region "select contents")))
  (cons 'region (apply 'append (mapcar (lambda (reg) (boon-borders reg how-much)) (mapcar 'boon-normalize-reg regs)))))

(defun boon-select-content (regs)
  (interactive (boon-spec-region "select borders"))
  (cons 'region (mapcar 'boon-content (mapcar 'boon-normalize-reg regs))))

(defvar boon-command-map (make-sparse-keymap) "Keymap used in Boon command mode")
(defvar boon-moves-map (make-sparse-keymap))
(defvar boon-select-map (make-sparse-keymap))

(set-keymap-parent boon-select-map boon-moves-map)
(set-keymap-parent boon-command-map boon-moves-map)

(progn
  (define-key boon-select-map "d"  'boon-select-document)
  (define-key boon-select-map "p"  'boon-select-paragraph)
  (define-key boon-select-map "w"  'boon-select-word)
  (define-key boon-select-map "f"  'boon-select-word) ;; 'rf' is easier to type than 'rw'
  (define-key boon-select-map "x"  'boon-select-outside-pairs) ;; eXpression
  (define-key boon-select-map "c"  'boon-select-inside-pairs) ;; Contents
  (define-key boon-select-map "s"  'boon-select-wim) ;; symbol
  (define-key boon-select-map "q"  'boon-select-outside-quotes)
  (define-key boon-select-map "'"  'boon-select-blanks) ;; blanKs
  (define-key boon-select-map " "  'boon-select-line)
  (define-key boon-select-map "r"  'boon-select-justline) ;; Ribbon
  (define-key boon-select-map "a"  'boon-select-borders) ;; Around
  (define-key boon-select-map "z"  'boon-select-content) ;; inZide

  (define-key boon-moves-map "k" 'boon-switch-mark) ; bacK to marK 
  (define-key boon-moves-map "K" 'boon-switch-mark-quick) ; quicK bacK to marK

(define-key boon-moves-map "j"  'boon-find-char-backward)
(define-key boon-moves-map "J"  'boon-find-char-forward)
(define-key boon-moves-map "u"  'previous-line)
(define-key boon-moves-map "y"  'next-line)
(define-key boon-moves-map "U"  'backward-paragraph)
(define-key boon-moves-map "Y"  'forward-paragraph)
(define-key boon-moves-map "l"  'boon-beginning-of-line)
(define-key boon-moves-map ";"  'boon-end-of-line)
(define-key boon-moves-map "n"  'smarter-backward)
(define-key boon-moves-map "o"  'smarter-forward)
(define-key boon-moves-map "N"  'smarter-upward)
(define-key boon-moves-map "O"  'smarter-downward)
(define-key boon-moves-map ","  'boon-beginning-of-expression)
(define-key boon-moves-map "."  'boon-end-of-expression)
(define-key boon-moves-map "e"  'backward-char)
(define-key boon-moves-map "i"  'forward-char)
(define-key boon-moves-map "<"  'boon-beginning-of-region)
(define-key boon-moves-map ">"  'boon-end-of-region)
)

;; TODO: this should not return a list
(defun boon-spec-region (msg)
  "specify a region concisely using the keyboard"
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

;; (defun boon-sel-reg ()
;;   "Used to debug `boon-spec-region'"
;;   (interactive)
;;   (message (format "Specified region: %s" (boon-spec-region "test"))))

(defmacro with-ordered-region (body)
  `(if (< (point) (mark)) 
       ,body
       (progn (exchange-point-and-mark) ,body (exchange-point-and-mark))))

(defun boon-drop-mark ()
  "drop a mark; or extend the region to the next full line; or revert to original state"
  (interactive)
  (if mark-active
      (if (and (bolp)
               (save-excursion (goto-char (mark)) (bolp))
               (not (eq (point) (mark))))
          (progn ;; here we have a number of full lines selected, and that number is more than 0  
            (pop-mark) ;; load the saved position into the mark
            (goto-char (mark));; jump there
            (deactivate-mark))
      (with-ordered-region
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

(defun current-line-indentation ()
  "Indentation of the curent line"
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun boon-open-line-and-insert ()
  "Open a new line, indented as much as the current one, and switch to insert mode."
  (interactive)
  (let ((indent-lvl (current-line-indentation))) 
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
      (when (blank-string-p line-prefix)
        (progn
          (forward-char 1)
          (insert line-prefix))))))

(defun boon-split-word ()
  "insert a space"
  (interactive)
  (insert (make-string 1 32)))

(defun boon-open-word ()
  (interactive)
  (unless (eq (preceding-char) 32)
    (insert (make-string 1 32)))
  (unless (eq (following-char) 32)
    (insert (make-string 1 32))
    (backward-char 1))
  (boon-set-insert-state))

(defun boon-split-line ()
  "split the current line"
  (interactive)
  (let ((indent-col (min (current-line-indentation) (current-column))))
    ;; kill the extra spaces
    (save-excursion
      (delete-and-extract-region (progn
                                   (skip-chars-forward "\n\t " (line-end-position))
                                   (point))
                                 (progn
                                   (skip-chars-backward "\n\t " (line-beginning-position))
                                   (point))))
    (newline)
    (insert (make-string indent-col ?\ ))))

(defun boon-newline-dwim ()
  "insert a new line do-what-i-mean style"
  (interactive)
  (if (and (not (eolp)) (blank-string-p (boon-line-prefix)))
      (call-interactively 'boon-open-line)
    (boon-split-line)))

(defun boon-adjust-indent ()
 "switch temporarily to adjust indentation mode"
 (interactive)
 (unless (use-region-p)
   (set-mark (line-beginning-position))
   (end-of-line)
   (deactivate-mark))
 (set-temporary-overlay-map boon-indent-map t))

(defun boon-replace-region ()
 "switch to insert mode; replacing the current region if there is one"
 (interactive) 
 (when (use-region-p)
   (on-region #'kill-region))
 (boon-set-insert-state))

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
  "specify an enclosure style"
  (let ((c (read-char "Specify the enclosure")))
    (message "Char: %c " c)
    (list (cdr (assoc c boon-enclosures)))))

(defun boon-enclose (enclosure regs)
  "Enclose the region with before and after"
  (interactive (append (boon-spec-enclosure) (boon-spec-region "enclose")))
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


;; forced to do this because all modes/etc. map "control i" to TAB action.
(define-key input-decode-map [(control i)] [(control-i)])
(define-key input-decode-map [(control super i)] [(super control-i)])
(define-key input-decode-map [(control meta i)] [(meta control-i)])
(define-key input-decode-map (kbd "C-SPC") [(escape)])

;; Off mode rebinds
(setq boon-off-map (make-sparse-keymap))
(define-key boon-off-map [(escape)] 'boon-set-command-state)

;;  Insert mode rebinds
(setq boon-insert-map (make-sparse-keymap))
(define-key boon-insert-map (kbd "<up>") 'undefined) 
(define-key boon-insert-map (kbd "<down>") 'undefined)
(define-key boon-insert-map [(escape)] 'boon-set-command-state)


(progn
  (require 'skeleton)
  (setq skeleton-pair t)
  (electric-pair-mode)
;; (define-key boon-insert-map "\'" 'self-insert-quote)
(define-key boon-insert-map [backspace] 'smart-insert-backspace2)
(define-key boon-insert-map "\"" 'self-insert-quote)

(defun empty-pair-p ()
  "Is the point at the middle of an empty pair of matched parens?"
  (interactive)
  (eq (caddr
            (assq (preceding-char)
             (or skeleton-pair-alist skeleton-pair-default-alist)))
           (following-char)))

(defun empty-quotes-p ()
  "Is the point in the middle of an empty pair of quotes?"
  (interactive)
  (and (eq (preceding-char) (following-char))
       (member (following-char) '(?\" ?\'))))

(defun smart-insert-backspace2 ()
  (interactive)
  (when (or (empty-pair-p) (empty-quotes-p))
    (delete-char 1))
  (backward-delete-char-untabify 1))

(defun self-insert-quote ()
  "Insert doubled quote, unless 1. the previous character is a
backslash, in which case a single quote is inserted or 2. the
next character is a quote in which case the cursor simply jumps
over it."
  (interactive)
  (cond
   ((equal (this-command-keys) (make-string 1 (following-char)))
    (forward-char 1))
   ((eq (preceding-char) ?\\)
    (self-insert-command 1))
   (t
    (self-insert-command 2)
    (backward-char 1))))

(defun close-parens () 
  (interactive)
  (if (and (char-after)
           (equal (this-command-keys) (make-string 1 (char-after))))
      (forward-char 1)
    (self-insert-command 1))))

(define-key global-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key isearch-mode-map [escape] 'isearch-abort)
(define-key isearch-mode-map [(control p)] 'helm-occur-from-isearch)
(define-key isearch-mode-map [(control w)] 'isearch-repeat-backward)
(define-key isearch-mode-map [(control f)] 'isearch-repeat-forward)



(defvar boon-flycheck-map
  (let ((pmap (make-sparse-keymap)))
    (define-key pmap "m" 'flycheck-mode)
    (define-key pmap "y" 'flycheck-buffer)
    (define-key pmap "C" 'flycheck-clear)
    (define-key pmap "r" 'flycheck-compile)
    (define-key pmap "n" 'flycheck-next-error)
    (define-key pmap "p" 'flycheck-previous-error)
    (define-key pmap "l" 'flycheck-list-errors)
    (define-key pmap "t" 'flycheck-copy-messages-as-kill)
    (define-key pmap "/" 'flycheck-google-messages)
    (define-key pmap "s" 'flycheck-select-checker)
    (define-key pmap "e" 'flycheck-set-checker-executable)
    (define-key pmap "d" 'flycheck-describe-checker)
    (define-key pmap "i" 'flycheck-info)
    (define-key pmap "V" 'flycheck-version)
    pmap)
  "Keymap to access stuff of `flycheck-mode'.")


(progn
 (setq boon-x-map (make-sparse-keymap))
 (set-keymap-parent boon-x-map ctl-x-map)

 (define-key boon-x-map "rr" 'boon-query-replace) ; replace the region if it is selected
 (define-key boon-x-map "t" 'boon-toggle-comment) ; commenT
 (define-key boon-x-map "i" 'boon-adjust-indent)
 (define-key boon-x-map [(return)] 'boon-split-line)
 (define-key boon-x-map " " 'boon-split-word)

 (define-key boon-x-map "-" 'undo-tree-visualize)
 (define-key boon-x-map "," 'boon-mark-previous-like-this); cursors: Prev
 (define-key boon-x-map "." 'boon-mark-next-like-this); cursors: Next
 (define-key boon-x-map "m" 'mc/skip-to-previous-like-this)
 (define-key boon-x-map "/" 'mc/skip-to-next-like-this)
 (define-key boon-x-map "O" 'previous-window) ;; o is next window
 (define-key boon-x-map "S" 'save-some-buffers)
 (define-key boon-x-map "\\" 'align-regexp)
 (define-key boon-x-map "b" 'ido-switch-buffer)
 (define-key boon-x-map "f" 'ido-find-file)
 (define-key boon-x-map "h" help-map)
 (define-key boon-x-map "hh" 'helm-apropos)
 (define-key boon-x-map "j" 'join-line)
 (define-key boon-x-map "k" 'kill-this-buffer)
 (define-key boon-x-map "K" 'helm-show-kill-ring)
 (define-key boon-x-map "l" 'fill-paragraph)
 (define-key boon-x-map "M" 'menu-bar-open)
 (define-key boon-x-map "s" 'save-buffer)
 (define-key boon-x-map "u" 'mc/edit-lines); cUrsors: multiple
 (define-key boon-x-map "vv" 'magit-status)
 (define-key boon-x-map "g" 'magit-status)
 (define-key boon-x-map "x" 'helm-M-x)
 (define-key boon-x-map "y" boon-flycheck-map)

 
)


(defun boon-mark-region (regs)
  (interactive (boon-spec-region "mark"))
  (dolist (reg regs)
    (set-mark (car reg))
    (goto-char (cdr reg)))
  (activate-mark))

(defun boon-end-of-region (regs)
  (interactive (boon-spec-region "go to end"))
  (dolist (reg regs)
    (goto-char (cdr reg))))

(defun boon-beginning-of-region (regs)
  (interactive (boon-spec-region "go to beginnig"))
  (dolist (reg regs)
    (goto-char (car reg))))

(defun boon-take-region (regs)
  (interactive (boon-spec-region "take"))
  (dolist (reg regs)
    (kill-region (car reg) (cdr reg))))


(defun boon-swap-region (regs)
  "Swap the region with the top of the kill ring"
  (interactive (boon-spec-region "swap"))
  (dolist (reg regs)
    (kill-region (car reg) (cdr reg)))
  (insert-for-yank (current-kill 1))
  
  (save-excursion
    (goto-char (car mark-ring))
    (insert-for-yank (current-kill -1)))
  )
  
(defun boon-treasure-region (regs)
  (interactive (boon-spec-region "treasure"))
  (dolist (reg regs)
    (kill-ring-save (car reg) (cdr reg))))

(defun boon-substitute-region (regs)
  (interactive (boon-spec-region "replace"))
  (boon-take-region regs)
  (boon-set-insert-state))

(defun boon-isearch-region (forward beg end)
 "search the selection"
 (let ((selection (buffer-substring-no-properties beg end)))
   (deactivate-mark)
   (if forward (goto-char end) (goto-char (- beg 1))) ; ensuring that we find the next match
   (isearch-mode forward nil nil nil)
   (isearch-yank-string selection)))

(defun on-region (f)
 "apply the argument to the current region"
   (funcall f (region-beginning) (region-end)))

(defadvice isearch-exit (after ysph-hl-search activate compile)
  "after isearch, highlight the search term "
  (setq boon-regexp isearch-string)
  (boon-highlight-regexp))

(defun boon-search-regexp (forward)
  (interactive)
  (when (not boon-regexp)
    (error "Search string not set"))
  (when (not isearch-success)
    (message "Wrapping around")
    (if forward (beginning-of-buffer) (end-of-buffer)))
  (setq isearch-success nil)
  (if forward 
      (re-search-forward boon-regexp)
    (re-search-backward boon-regexp))
  (setq isearch-success t) ; If search fails an exception is thrown and this won't be set. 
  )

(defun boon-qsearch (forward)
  (boon-highlight-regexp)
  (boon-search-regexp forward)
  (deactivate-mark))

(defun boon-qsearch-next ()
  "search the next occurence of the current search regexp"
  (interactive)
  (boon-qsearch t))

(defun boon-qsearch-previous ()
  "search the previous occurence of the current search regexp"
  (interactive)
  (boon-qsearch nil))

(defun boon-qsearch-next-at-point ()
  "search the next occurence of the current search regexp at point"
  (interactive)
  (boon-set-search-string (boon-stuff-at-point))
  (boon-qsearch t))

(defun boon-qsearch-previous-at-point ()
  "search the previous occurence of the current search regexp at point"
  (interactive)
  (boon-set-search-string (boon-stuff-at-point))
  (boon-qsearch nil))

(defun boon-set-search-string (string)
  (interactive "M")
  (setq boon-regexp (cond ((if (and (eq isearch-case-fold-search t)
                                     search-upper-case)
                                (isearch-no-upper-case-p
                                 string isearch-regexp)
                              isearch-case-fold-search)
                            ;; Turn isearch-string into a case-insensitive
                            ;; regexp.
                            (mapconcat
                             (lambda (c)
                               (let ((s (string c)))
                                 (if (string-match "[[:alpha:]]" s)
                                     (format "[%s%s]" (upcase s) (downcase s))
                                   (regexp-quote s))))
                             string ""))
                           (t (regexp-quote string)))))

(defun boon-highlight-regexp ()
  (interactive)
  ;; (global-hi-lock-mode 1)
  (hi-lock-face-buffer boon-regexp 'hi-yellow))

(defun boon-unhighlight ()
  (interactive)
  (when (bound-and-true-p hi-lock-interactive-patterns)
    (hi-lock-unface-buffer (car (car hi-lock-interactive-patterns)))))

(defun boon-multi-occur ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

(defun boon-stuff-at-point ()
  (interactive)
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (or (thing-at-point 'symbol)
          (error "Nothing relevant at point; move to a symbol or select a region"))))

(progn
  (suppress-keymap boon-command-map 't)  ; so that typing is disabled altogether in command mode

  ;; Special keys
  (define-key boon-command-map [f1] 'helm-apropos)
  (define-key boon-command-map "\\" 'undefined)
  (define-key boon-command-map [(return)] 'undefined)
  (define-key boon-command-map (kbd "RET") 'undefined)
  (define-key boon-command-map [(backspace)] 'undefined)
  (define-key boon-command-map (kbd "DEL") 'undefined)
  (define-key boon-command-map "`" 'boon-toggle-case)
  (dolist (d '("M-0" "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9"
               "C-0" "C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
    (define-key boon-command-map (read-kbd-macro d) 'digit-argument))
  (define-key boon-command-map "_" 'redo)
  (define-key boon-command-map "-" 'undo)
  (define-key boon-command-map "\\" 'universal-argument)
  
  ;; LEFT HAND

  ;; Top row
  ;; q
  (define-key boon-command-map "q" 'boon-quote-character)
  
  ;; w,f
  ;; where is? find?
  (define-key boon-moves-map "w "  'isearch-backward)
  (define-key boon-moves-map "f "  'isearch-forward)

  (define-key boon-moves-map "wt"  'boon-qsearch-previous-at-point)
  (define-key boon-moves-map "ft"  'boon-qsearch-next-at-point)
  (define-key boon-moves-map "ws"  'boon-qsearch-previous-at-point)
  (define-key boon-moves-map "fs"  'boon-qsearch-next-at-point)

  (define-key boon-moves-map "ww"  'boon-qsearch-previous)
  (define-key boon-moves-map "ff"  'boon-qsearch-next)

  (define-key boon-moves-map "W"  'boon-qsearch-previous)
  (define-key boon-moves-map "F"  'boon-qsearch-next)
  (define-key boon-moves-map "wp"  'boon-qsearch-previous)
  (define-key boon-moves-map "fp"  'boon-qsearch-next)
  (define-key boon-moves-map "we"  'previous-error)
  (define-key boon-moves-map "fe"  'next-error)
  (define-key boon-moves-map "wk"  'flycheck-previous-error)
  (define-key boon-moves-map "fk"  'flycheck-next-error)
  (define-key boon-moves-map "wb"  'previous-buffer)
  (define-key boon-moves-map "fb"  'next-buffer)

  (define-key boon-command-map "fa" 'agda2-goto-definition-keyboard)
  

  ;; p
  ;; Pinpoint Place
  (define-key boon-command-map "p" 'helm-occur)

  ;; Misc crap
  (define-key boon-command-map "P" 'kmacro-end-or-call-macro) ; Play
  (define-key boon-command-map "X" 'boon-highlight-regexp)

  ;; g Go to
  (define-key boon-command-map "gg" 'helm-resume)
  (define-key boon-command-map "gf" 'helm-for-files) ;; see http://amitp.blogspot.se/2012/10/emacs-helm-for-finding-files.html
  (define-key boon-command-map "gl" 'goto-line)

  (define-key boon-command-map "gi" 'helm-git-grep)
  (define-key boon-command-map "gt" 'helm-etags-select)
  (define-key boon-command-map "gy" 'helm-flycheck)
  (define-key boon-command-map "gb" 'helm-buffers-list)
  (define-key boon-command-map "gm" 'boon-multi-occur)

  ;; home row
  ;; a
  (define-key boon-command-map "a" 'boon-enclose) ; around
  (define-key boon-command-map "A" 'boon-swap-region) ; swap

  ;; r
  (define-key boon-command-map "r" 'boon-substitute-region) ; replace
  (define-key boon-command-map "R" 'kmacro-start-macro) ; Record

  ;; s
  (define-key boon-command-map "s" 'boon-splice) ; splice
  (define-key boon-command-map "S" 'yank-pop)

  ;; t
  (define-key boon-command-map "t" 'boon-take-region) ; "take"
  (define-key boon-command-map "T" 'boon-treasure-region) ; "treasure"

  ;; d
  (define-key boon-command-map "d" 'boon-replace-character) ; "displace"

  ;; Bottom row
  ;; z
  ;; reserved (repeat?)
  ;; x
  (define-key boon-command-map "x" boon-x-map)
  ;; c
  (define-key boon-command-map "c" 'undefined)
  ;; v
  (define-key boon-command-map (kbd "C-v") 'boon-open-line-and-insert)
  (define-key boon-command-map "V" 'boon-open-next-line-and-insert)
  (define-key boon-command-map "v" 'boon-set-insert-like-state) ; 'v' looks like an insertion mark
  ;; b
  (define-key boon-command-map "B" 'boon-copy-to-register)
  (define-key boon-command-map "b" 'insert-register)
  
  ;; RIGHT HAND: movement and marking commands.
  ;; Most of these are actually in the boon-moves-map; however some don't quite work there; so they end up here.
  (define-key boon-command-map (kbd "C-u") 'scroll-up-line)
  (define-key boon-command-map (kbd "C-y") 'scroll-down-line)
  ;; (define-key boon-command-map "J" 'jump-to-register)
  ;; (define-key boon-command-map "j" 'ace-jump-line-mode) ; jump
  (define-key boon-command-map "h" 'ace-jump-word-mode) ; hop
  (define-key boon-command-map "H" 'ace-jump-char-mode) ; Hop
  (define-key boon-command-map "'" 'boon-toggle-mark)

  (define-key boon-command-map " " 'boon-drop-mark)
  (define-key boon-command-map [(escape)] 'boon-quit)

  (define-key boon-insert-map [remap newline] 'boon-newline-dwim)
  (progn
    (setq boon-indent-map (make-sparse-keymap))
    (define-key boon-indent-map "e" 'boon-unindent-rigidly)
    (define-key boon-indent-map "i" 'indent-rigidly))

)

(setq boon-mode-map-alist (list (cons 'boon-command-state boon-command-map)
                                 (cons 'boon-off-state     boon-off-map)
                                 (cons 'boon-insert-state  boon-insert-map)
                                 (cons 'boon-helm-command-state boon-helm-command-map)))
(push 'boon-mode-map-alist emulation-mode-map-alists)

(defun boon-move-thing (cmd)
  (let ((expand-region-fast-keys-enabled nil))
    (call-interactively cmd)))

(defun special-mode-p ()
  (interactive)
  (memq major-mode '(Buffer-menu-mode
                     Custom-mode
                     completion-list-mode
                     debugger-mode
                     ediff-mode
                     magit-key-mode
                     magit-branch-manager-mode
                     git-rebase-mode
                     magit-log-mode
                     magit-status-mode)))

(define-minor-mode boon-local-mode
  "Minor mode for setting up command mode in a single buffer."
  :init-value nil
  :lighter (:eval (boon-modeline-string))
  :keymap nil
  (cond
   (boon-local-mode
    ;; restore the proper value of `major-mode' in Fundamental buffers
    (when (eq major-mode 'turn-on-boon-mode)
      (setq major-mode 'fundamental-mode))
    ;; The initial state is usually setup by `boon-initialize' when
    ;; the major-mode in a buffer changes. This preliminary
    ;; initialization is only for the case when `boon-local-mode' is
    ;; called directly for the first time in a buffer.
    (set (make-local-variable 'boon-regexp) nil)
    (cond
     ((special-mode-p)
      (boon-set-off-state))
     ((memq major-mode '(magit-commit-mode
                         git-commit-mode
                         ))
      (boon-set-insert-state))
     (t (boon-set-command-state))))
   (t
    (boon-set-off-state)
    (message "Boon disabled")
    )))

(add-hook 'minibuffer-setup-hook 'boon-minibuf-hook)

(defun eq-if-bound (sym val)
  (and (boundp sym) (eq (eval sym) val)))

(defun boon-minibuf-hook ()
  (cond
   ((eq-if-bound 'helm-map (current-local-map))
    (boon-helm-set-command-state))
   ((eq-if-bound 'helm-git-grep-map (current-local-map))
    (boon-helm-set-command-state))
  (t (setq cursor-type 'bar))))

(defun boon-initialize ()
  "Enable Boon in the current buffer, if appropriate. To enable Boon globally, do (boon-mode 1)."
  (unless (or (minibufferp) 
              ;; (eq major-mode 'inferior-haskell-mode)
              ;; (eq major-mode 'compilation-mode)
              )
    (boon-local-mode 1)))

(define-globalized-minor-mode boon-mode
  boon-local-mode boon-initialize)

;; The function `boon-initialize' should only be used to initialize
;; `boon-local-mode' from the globalized minor-mode `boon-mode'. It is
;; called whenever boon is enabled in a buffer for the first time or
;; when boon is active and the major-mode of the buffer changes. 

(defun turn-on-boon-mode (&optional arg)
  "Turn on Boon in the current buffer. (and push a mark to remember the last edition point)"
  (interactive)
  (boon-local-mode (or arg 1)))

(defun turn-off-boon-mode (&optional arg)
  "Turn off Boon in the current buffer"
  (interactive)
  (boon-local-mode (or arg -1)))

(progn
  (define-key key-translation-map "xc " (kbd "C-c C-SPC"))
  (define-key key-translation-map "xc," (kbd "C-c C-,"))
  (define-key key-translation-map "xc." (kbd "C-c C-."))
  (define-key key-translation-map "xc=" (kbd "C-c C-="))
  (define-key key-translation-map "xc!" (kbd "C-c !"))
  (define-key key-translation-map "xc[" (kbd "C-c ]"))
  (define-key key-translation-map "xc]" (kbd "C-c ]"))
  (define-key key-translation-map "xc?" (kbd "C-c C-?"))
  (define-key key-translation-map "xca" (kbd "C-c C-a"))
  (define-key key-translation-map "xcb" (kbd "C-c C-b"))
  (define-key key-translation-map "xcc" (kbd "C-c C-c"))
  (define-key key-translation-map "xcd" (kbd "C-c C-d"))
  (define-key key-translation-map "xce" (kbd "C-c C-e"))
  (define-key key-translation-map "xcf" (kbd "C-c C-f"))
  (define-key key-translation-map "xcg" (kbd "C-c C-g"))
  (define-key key-translation-map "xch" (kbd "C-c C-h"))
  (define-key key-translation-map "xci" (kbd "C-c C-i"))
  (define-key key-translation-map "xcj" (kbd "C-c C-j"))
  (define-key key-translation-map "xck" (kbd "C-c C-k"))
  (define-key key-translation-map "xcl" (kbd "C-c C-l"))
  (define-key key-translation-map "xcm" (kbd "C-c C-m"))
  (define-key key-translation-map "xcn" (kbd "C-c C-n"))
  (define-key key-translation-map "xco" (kbd "C-c C-o"))
  (define-key key-translation-map "xcp" (kbd "C-c C-p"))
  (define-key key-translation-map "xcq" (kbd "C-c C-q"))
  (define-key key-translation-map "xcr" (kbd "C-c C-r"))
  (define-key key-translation-map "xcs" (kbd "C-c C-s"))
  (define-key key-translation-map "xct" (kbd "C-c C-t"))
  (define-key key-translation-map "xcu" (kbd "C-c C-u"))
  (define-key key-translation-map "xcv" (kbd "C-c C-v"))
  (define-key key-translation-map "xcw" (kbd "C-c C-w"))
  (define-key key-translation-map "xcxd" (kbd "C-c C-x C-d"))
  (define-key key-translation-map "xcxq" (kbd "C-c C-x C-q"))
  (define-key key-translation-map "xcy" (kbd "C-c C-y"))
  (define-key key-translation-map "xcz" (kbd "C-c C-z"))
)

(defun boon-set-state (state)
  (set (make-local-variable 'boon-command-state) nil)
  (set (make-local-variable 'boon-insert-state) nil)
  (set (make-local-variable 'boon-off-state) nil)

  (set state t)
  (unless boon-command-state
    (deactivate-mark)
    (save-excursion
      (when (not (bolp))
        (let ((orig (point)))
          (skip-chars-forward " " (line-end-position))
          (when (eolp) (delete-and-extract-region orig (point))))))
    (when (bound-and-true-p boon-modeline-face-cookie)
      (face-remap-remove-relative boon-modeline-face-cookie))
    (setq cursor-type 'bar))
  (cond (boon-command-state
         ;; (do-auto-save)
         (when (< 2 (abs (-
                          (line-number-at-pos (point))
                          (line-number-at-pos (mark)))))
           (push-mark)) ; remember where the last edition was by pushing a mark
         (setq cursor-type 'box)
         (set (make-local-variable 'boon-modeline-face-cookie)
              (face-remap-add-relative
               'mode-line '((:foreground "darkred") mode-line))))
        (boon-off-state)
        (boon-insert-state)
        (t (message "Unknown state!")))
  (force-mode-line-update))

(defun boon-set-insert-state ()
  (interactive) (boon-set-state 'boon-insert-state))

(defun boon-set-insert-like-state ()
  (interactive)
  (if (special-mode-p) (boon-set-off-state) (boon-set-insert-state)))

(defun boon-set-command-state ()
  (interactive) (boon-set-state 'boon-command-state))

(defun boon-set-off-state ()
  (interactive) (boon-set-state 'boon-off-state))

(defun boon-modeline-string ()
  "returns a string describing the current state"
  (concat " Boon:" (cond
   (boon-command-state "CMD")
   (boon-insert-state  "INS")
   (boon-off-state     "OFF")
   (t "???"))))
