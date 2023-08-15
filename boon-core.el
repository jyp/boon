
;;; boon-core.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module sets up the emulation keymaps for each boon state.
;; Functions to switch to each state is also provided.

;;; Code:

(require 'cl-macs)
(require 'dash)
(require 'subr-x)

(defgroup boon nil "Boon: An Ergonomic Command Mode." :group 'Editing)

;; Maps
(defvar boon-x-map (let ((map (make-sparse-keymap))) (set-keymap-parent map ctl-x-map))
  "Extended boon command map.

\\{boon-x-map}")
(fset 'boon-x-map boon-x-map)

(defvar boon-moves-map (make-sparse-keymap)
  "Keymap for moves (subset of command mode).

\\{boon-moves-map}")

(defvar boon-command-map (let ((map (make-sparse-keymap)))
                           (suppress-keymap map 't)
                           (set-keymap-parent map boon-moves-map))
  "Keymap used in Boon command mode.
\\{boon-command-map}")

(defvar boon-select-map (make-sparse-keymap)
  "Keymap for text regions selectors.
\\{boon-select-map}

Any move is also a valid region selector, see `boon-moves-map'.")
(defvar boon-insert-map (make-sparse-keymap))
(defvar boon-special-map (make-sparse-keymap) "Keymap used in special modes.
See also `boon-special-mode-list'.

\\{boon-special-map}")

(defvar-local boon-mode-map-alist nil)
(push 'boon-mode-map-alist emulation-mode-map-alists)

;; States
(defvar-local boon-off-state nil "State where boon is disabled boon altogether.") ;; without having to fiddle with `emulation-mode-map-alists'
(defvar-local boon-command-state nil "Non-nil when boon command mode is activated.
Boon commands can be entered in this mode.")
(defvar-local boon-insert-state nil "Non-nil when boon insert mode is activated.")
(defvar-local boon-special-state nil "Non-nil when special state is activated.
Special is active when `special-mode' buffers (see `boon-special-mode-list') are
activated.  This buffers have their own set of commands, so we use
those.  See `boon-special-map' for exceptions.")

(defvar boon/insert-command-history nil "History of changes in this insertion round.")
(defvar boon/insert-command nil "Command which started the insertion.")
(defvar boon/insert-origin 0 "Point at start of insert mode.")

(defcustom boon-default-cursor-type 'bar "Default `cursor-type', also used for the minibuffer." :group 'boon :type 'sexp)
(defcustom boon-command-cursor-type 'box "`cursor-type' for command mode." :group 'boon :type 'sexp)
(defcustom boon-insert-cursor-type 'bar "`cursor-type' for insert mode." :group 'boon :type 'sexp)
(defcustom boon-special-cursor-type 'box "`cursor-type' for special mode." :group 'boon :type 'sexp)

(defcustom boon-default-cursor-color nil
  "Default `cursor-color', also used for the minibuffer.

If you want to use different cursor colors in Boon, setting this
variable is mandatory.  Apart from that, you may set any number
of `boon-command-cursor-color', `boon-insert-cursor-color' and
`boon-special-cursor-color' to your liking."
  :group 'boon
  :type 'string)

(defcustom boon-command-cursor-color nil
  "`cursor-color' for command mode.
`boon-default-cursor-color' must also be set."
  :group 'boon
  :type 'string)

(defcustom boon-insert-cursor-color nil
  "`cursor-color' for insert mode.
`boon-default-cursor-color' must also be set."
  :group 'boon
  :type 'string)

(defcustom boon-special-cursor-color nil
  "`cursor-color' for special mode.
`boon-default-cursor-color' must also be set."
  :group 'boon
  :type 'string)

(defun boon-update-cursor ()
  "Update the cursor depending on the current boon mode."
  (with-current-buffer (window-buffer)
    (pcase
        (cond
         (boon-insert-state (list boon-insert-cursor-type boon-insert-cursor-color))
         (boon-command-state (list boon-command-cursor-type boon-command-cursor-color))
         (boon-special-state (list boon-special-cursor-type boon-special-cursor-color))
         (t (list boon-default-cursor-type boon-default-cursor-color)))
      (`(,type ,color)
        
       (setq cursor-type type)
       ;; The default value for all cursor colors is `nil', which skips calling
       ;; `set-cursor-color' completely. This avoids accidentaly changing the
       ;; behaviour as it's difficult to provide a single meaningful default
       ;; color name, e.g. because the user could change the theme while using
       ;; Boon.
       (when (or color boon-default-cursor-color)
         (set-cursor-color (or color boon-default-cursor-color)))))))

(add-hook 'buffer-list-update-hook #'boon-update-cursor)

(defun boon-interactive-insert (&rest args)
  "Boon insert commands must call this function after `interactive'.
The effect of this function is to remember the current command
and ARGS so that it can be repeated later by
`boon-set-insert-like-state'.  The current command must take an
optional list of changes as its last argument."
  (unless boon/insert-command
    (setq boon/insert-command (cons this-command (-map (lambda (x) (list 'quote x)) args)))))

(defun boon/after-change-hook (begin end old-len)
  "Remember the change in `boon/insert-command-history'.
Change is defined by BEGIN END OLD-LEN."
  (when (and boon-insert-state
             (not (bound-and-true-p mc--executing-command-for-fake-cursor)))
    ;; (message "bach: %s" boon/insert-command-history (list begin end old-len))
    (pcase boon/insert-command-history
      ((and `((,bb ,del-len "") . ,rest) ; no insert
            (guard (eq begin end)) ; no insert
            (guard (eq (+ begin old-len) (+ boon/insert-origin bb)))) ; consecutive
       ;; two consecutive deletes: concat them.
       (setq boon/insert-command-history
             (cons (list (- begin boon/insert-origin) (+ old-len del-len) "")
                   rest)))
      ((and `((,bb 0 ,ins) . ,rest) ; no delete
            (guard (eq old-len 0)) ; no delete
            (guard (eq begin (+ boon/insert-origin bb (length ins))))) ; consecutive
       ;; two consecutive inserts: concat them.
       (setq boon/insert-command-history
             (cons (list bb 0 (concat ins (buffer-substring-no-properties begin end)))
                   rest)))
      (_
       (push (list (- begin boon/insert-origin) old-len
                   (buffer-substring-no-properties begin end))
             boon/insert-command-history)))))

(defun boon/replay-changes (chnges)
  "Replay the CHNGES at the current point."
  ;; (message "brc: %s" chnges)
  (let ((p0 (point)))
    (setq boon/insert-command nil) ;; did not go to insert mode after all
    (-each chnges (pcase-lambda (`(,start ,len ,txt))
                    (goto-char (+ p0 start))
                    (delete-region (+ p0 start) (+ p0 start len))
                    (insert txt)
                    (goto-char (+ p0 start (length txt)))))))

(defvar-local boon-input-method nil
"The input method to activate when going to insert state.
When leaving insert state the input-method is reset to nil.")

(defun boon-set-state (state)
  "Set the boon state (as STATE) for this buffer."
  (when boon-insert-state (setq-local boon-input-method current-input-method))
  (setq boon-command-state nil)
  (setq boon-insert-state nil)
  (setq boon-special-state nil)
  (set state t)
  (cond (boon-command-state
         (deactivate-input-method)
         (when (and boon/insert-command boon/insert-command-history)
           (push `(,@boon/insert-command
                   (quote ,@(list (nreverse boon/insert-command-history))))
                 command-history))
         (setq boon/insert-command nil)
         (setq boon/insert-command-history nil)
         (boon-update-cursor))
        (boon-special-state)
        (boon-insert-state
         (activate-input-method boon-input-method)
         (deactivate-mark)
         (save-excursion
           (when (not (bolp))
             (let ((orig (point)))
               (skip-chars-forward " " (line-end-position))
               (when (eolp) (delete-region orig (point))))))
         (boon-update-cursor)
         (push-mark) ;; remember where the last edition was by pushing a mark
         (setq boon/insert-command-history nil)
         (setq boon/insert-origin (point)))
        (boon-off-state)
        (t (error "Boon: Unknown state!")))
  (force-mode-line-update))

(defun boon-set-insert-state ()
  "Switch to insert state."
  (when (and buffer-read-only (not (boon-shell-mode-p)))
    (error "Buffer is read only, can't insert in it"))
  (when (get-text-property (point) 'read-only)
    (if-let ((writeable-pos (next-single-property-change (point) 'read-only nil (line-end-position))))
        (progn
          (when (get-text-property writeable-pos 'read-only)
            (error "Rest of the line is read only"))
          (goto-char writeable-pos))
      (error "Rest of the buffer is read only")))
  (boon-set-state 'boon-insert-state))

(defun boon-set-command-state ()
  "Switch to command state."
  (interactive) (boon-set-state 'boon-command-state))

(defun boon-set-special-state ()
  "Switch to special state."
  (boon-set-state 'boon-special-state))

(defcustom boon-special-mode-list
  '(Buffer-menu-mode
    cfw:calendar-mode
    debugger-mode
    ediff-mode
    ediff-meta-mode
    finder-mode
    git-rebase-mode
    mu4e-headers-mode
    mu4e-view-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    notmuch-tree-mode
    org-agenda-mode
    pass-mode
    view-mode)
    "A List of modes which should use `boon-special-state'."
    :group 'boon
    :type '(repeat symbol))

(defun boon-shell-mode-p ()
  "Is the `major-mode' any of the shell modes?"
  (derived-mode-p 'comint-mode 'eshell-mode 'term-mode 'vterm-mode))

(defcustom boon-special-conditions
  '((bound-and-true-p magit-blame-mode))
  "A list of sufficient conditions to trigger special state."
  :group 'boon :type '(list sexp))

(defcustom boon-insert-conditions '((eq major-mode 'message-mode))
  "A list of sufficient conditions to start in insert state."
  :group 'boon :type '(list sexp))

(defun boon-special-mode-p ()
  "Should the mode use `boon-special-state'?"
  (or (and (eq (get major-mode 'mode-class) 'special)
           (not (boon-shell-mode-p)))
      (-some 'eval boon-special-conditions)
      (memq major-mode boon-special-mode-list)))

;;; Initialisation and activation

(defun boon-set-natural-state ()
  "Set the natural state for the buffer."
  (cond ((boon-special-mode-p) (boon-set-state 'boon-special-state))
        ((-some 'eval boon-insert-conditions) (boon-set-insert-state))
        (t (boon-set-command-state))))

(define-minor-mode boon-local-mode
  "Minor mode for setting up command mode in a single buffer."
  :init-value nil
  :lighter (:eval (boon-modeline-string))
  :keymap nil
  (if (not boon-local-mode)
      (progn
        (remove-hook 'after-change-functions #'boon/after-change-hook)
        (boon-set-state 'boon-off-state))
    (setq boon-mode-map-alist
          (list (cons 'boon-command-state (or (get major-mode 'boon-map) boon-command-map))
                (cons 'boon-special-state (or (get major-mode 'boon-special-map) boon-special-map))
                (cons 'boon-insert-state  (or (get major-mode 'boon-insert-map) boon-insert-map))))
    (add-hook 'after-change-functions #'boon/after-change-hook nil t)
    (boon-set-natural-state)))

(add-hook 'minibuffer-setup-hook 'boon-minibuf-hook)

(defun boon-minibuf-hook ()
  "Set the cursor type to `bar'.
This is because no command mode is activated in the minibuffer."
  (setq cursor-type 'bar))

(defun boon-initialize ()
  "Setup boon in the current buffer.
Should only be used to
initialize `boon-local-mode' from the globalized minor-mode
`boon-mode'.  It is called whenever boon is enabled in a buffer
for the first time or when boon is active and the `major-mode' of
the buffer changes."
  (unless (minibufferp)
    (boon-local-mode 1)))

;;;###autoload (autoload 'boon-mode "boon" "Toggle boon in all buffers" t)
(define-globalized-minor-mode boon-mode boon-local-mode boon-initialize :group 'boon)

;;;###autoload
(defun turn-on-boon-mode ()
  "Turn on Boon in the current buffer."
  (interactive)
  (boon-local-mode 1))

;;;###autoload
(defun turn-off-boon-mode ()
  "Turn off Boon in the current buffer."
  (interactive)
  (boon-local-mode -1))

(defun boon-modeline-string ()
  "Return the modeline string appropriate for the current state."
  (concat " Boon:" (boon-state-string)))

(defun boon-state-string ()
  "Return a string describing the current state."
  (cond
   (boon-command-state "CMD")
   (boon-insert-state  "INS")
   (boon-special-state "SPC")
   (t "???")))

(with-eval-after-load 'multiple-cursors
  (defvar mc--default-cmds-to-run-for-all)
  (defvar mc--default-cmds-to-run-once)
  (setq mc--default-cmds-to-run-for-all
        (append '(boon-beginning-of-expression
                  boon-beginning-of-line
                  boon-end-of-expression
                  boon-end-of-line
                  boon-end-of-region
                  boon-find-char-backward
                  boon-find-char-forward
                  boon-quote-character
                  boon-replace-by-character
                  boon-smarter-backward
                  boon-smarter-forward
                  boon-splice
                  boon-split-line
                  boon-switch-mark
                  boon-toggle-character-case
                  boon-toggle-mark)))
  (setq mc--default-cmds-to-run-once
        (append mc--default-cmds-to-run-once
                '(boon-adjust-indent
                  boon-navigate-forward
                  boon-navigate-backward
                  boon-drop-mark
                  boon-enclose
                  boon-qsearch-next
                  boon-qsearch-next-at-point
                  boon-qsearch-previous
                  boon-qsearch-previous-at-point
                  boon-query-replace
                  boon-quit
                  boon-set-command-state
                  boon-set-insert-like-state
                  boon-substitute-region
                  boon-take-region
                  boon-toggle-character-case
                  boon-toggle-case))))

;; When switching away from a window (for example by clicking in
;; another window), return the buffer hosting it to its "natural"
;; state (otherwise it's surprising to the user when coming back to it)
(add-hook 'window-selection-change-functions
          (defun boon-reset-state-for-switchw (window)
            "Reset the boon state to natural when switching windows."
            (let* ((old (old-selected-window))
                   (prev-buf (window-buffer old)))
              (with-current-buffer prev-buf
                (boon-set-natural-state)))))

(defadvice isearch-exit (after boon-isearch-set-search activate compile)
  "After isearch, highlight the search term."
  (boon-hl-regexp (if isearch-regexp isearch-string (regexp-quote isearch-string))))

(defadvice swiper--action (after boon-swiper-set-search activate compile)
  "After swiper, highlight the search term."
  (boon-hl-regexp (car regexp-search-ring)))

(provide 'boon-core)
;;; boon-core.el ends here
