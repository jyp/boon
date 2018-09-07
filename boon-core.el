;;; boon-core.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module sets up the emulation keymaps for each boon state.
;; Functions to switch to each state is also provided.

;;; Code:

(require 'cl-macs)
(require 'dash)

(defgroup boon nil "Boon" :group 'Editing)

;; Maps
(defvar boon-x-map)
(define-prefix-command 'boon-x-map)
(set-keymap-parent boon-x-map ctl-x-map)

(defvar boon-command-map (make-sparse-keymap)
  "Keymap used in Boon command mode.

\\{boon-command-map}")
(suppress-keymap boon-command-map 't)
(defvar boon-moves-map (make-sparse-keymap)
  "Keymap for moves (subset of command mode).

\\{boon-moves-map}")
(set-keymap-parent boon-command-map boon-moves-map)
(defvar boon-select-map (make-sparse-keymap)
  "Keymap for text regions selectors.
\\{boon-select-map}

Any move is also a valid region selector, see `boon-moves-map'.")
(defvar boon-insert-map (make-sparse-keymap))
(defvar boon-special-map (make-sparse-keymap) "Keymap used in special modes.
See also `boon-special-mode-list'.

\\{boon-special-map}")

(defvar boon-mode-map-alist (list (cons 'boon-command-state boon-command-map)
                                  (cons 'boon-special-state boon-special-map)
                                  (cons 'boon-insert-state  boon-insert-map)))
(push 'boon-mode-map-alist emulation-mode-map-alists)

;; States
(defvar-local boon-command-state nil "Non-nil when boon command mode is activated. (Boon commands can be entered in this mode.)")
(defvar-local boon-insert-state nil "Non-nil when boon insert mode is activated.")
(defvar-local boon-special-state nil "Non-nil when special state is
activated. Special is active when special-mode buffers (see `boon-special-mode-list') are
activated. This buffers have their own set of commands, so we use
those. See `boon-special-map' for exceptions.")

(defvar boon/insert-command-history nil "History of changes in this insertion round.")
(defvar boon/insert-command nil "Command which started the insertion.")
(defvar boon/insert-origin 0 "Point at start of insert mode.")

(defun boon-interactive-insert (&rest args)
  "Boon insert commands must call this function after `interactive'.
The effect of this function is to remember the current command
and ARGS so that it can be repeated later by
`boon-set-insert-like-state'.  The current command must take an
optional list of changes as its last argument."
  (unless boon/insert-command
    (setq boon/insert-command (cons this-command (-map (lambda (x) (list 'quote x)) args)))))

(defun boon/after-change-hook (begin end old-len)
  "Remember the change defined by BEGIN END OLD-LEN in `boon/insert-command-history'."
  (when (and boon-insert-state (not mc--executing-command-for-fake-cursor))
    ;; (message "bach: %s" boon/insert-command-history (list begin end old-len))
    (cond ((and boon/insert-command-history
                (string= "" (nth 2 (car boon/insert-command-history))) ;; no insert
                (eq begin end) ;; no insert
                (eq (+ begin old-len) (+ boon/insert-origin
                                         (car (car boon/insert-command-history)))))
           ;; two consecutive deletes: concat them.
           (setq boon/insert-command-history (cons (list (- begin boon/insert-origin)
                                                         (+ old-len (nth 1 (car boon/insert-command-history)))
                                                         "")
                                                   (cdr boon/insert-command-history))))
          ((and boon/insert-command-history
                (eq 0 (nth 1 (car boon/insert-command-history))) ;; no delete
                (eq 0 old-len) ;; no delete
                (eq begin (+ boon/insert-origin
                             (car (car boon/insert-command-history))
                             (length (nth 2 (car boon/insert-command-history))))))
           ;; two consecutive inserts: concat them.
           (setq boon/insert-command-history (cons (list (car (car boon/insert-command-history))
                                                         0
                                                         (concat (nth 2 (car boon/insert-command-history)) (buffer-substring-no-properties begin end)))
                                                   (cdr boon/insert-command-history))))
          (t
           (push (list (- begin boon/insert-origin) old-len (buffer-substring-no-properties begin end))
                 boon/insert-command-history)))))

(defun boon/replay-changes (changes)
  "Replay the CHANGES at the current point."
  (let ((p0 (point)))
    (setq boon/insert-command nil) ;; did not go to insert mode after all
    (dolist (change changes)
      (goto-char (+ p0 (nth 0 change)))
      (delete-region (+ p0 (nth 0 change)) (+ p0 (nth 0 change) (nth 1 change)))
      (insert (nth 2 change)))))

(defun boon-set-state (state)
  "Set the boon state (as STATE) for this buffer."
  (setq boon-command-state nil)
  (setq boon-insert-state nil)
  (setq boon-special-state nil)
  (set state t)
  (cond (boon-command-state
         ;; (do-auto-save)
         (when (and boon/insert-command boon/insert-command-history)
           (push `(,@boon/insert-command
                   (quote ,@(list (nreverse boon/insert-command-history))))
                 command-history))
         (setq boon/insert-command nil)
         (setq boon/insert-command-history nil)
         (setq cursor-type 'box))
        (boon-special-state (setq cursor-type 'box))
        (boon-insert-state
         (deactivate-mark)
         (save-excursion
           (when (not (bolp))
             (let ((orig (point)))
               (skip-chars-forward " " (line-end-position))
               (when (eolp) (delete-region orig (point))))))
         (setq cursor-type 'bar)
         (push-mark) ;; remember where the last edition was by pushing a mark
         (setq boon/insert-command-history nil)
         (setq boon/insert-origin (point)))
        (t (error "Boon: Unknown state!")))
  (force-mode-line-update))

(defun boon-set-insert-state ()
  "Switch to insert state."
  (if buffer-read-only (error "buffer is read only, can't insert in it."))
  (boon-set-state 'boon-insert-state))

(defun boon-set-command-state ()
  "Switch to command state."
  (interactive) (boon-set-state 'boon-command-state))

(defun boon-set-special-state ()
  "Switch to special state."
  (boon-set-state 'boon-special-state))

(defcustom boon-special-mode-list
  '(Buffer-menu-mode
    debugger-mode
    ediff-mode
    git-rebase-mode
    mu4e-headers-mode
    mu4e-view-mode
    org-agenda-mode
    cfw:calendar-mode)
    "A List of modes which should use `boon-special-state'."
    :group 'boon
    :type '(repeat symbol))

(defcustom boon-special-conditions
  '(magit-blame-mode)
  "A list of sufficient conditions to trigger special state."
  :group 'boon)

(defun boon-special-mode-p ()
  "Should the mode use `boon-special-state'?"
  (or (and (eq (get major-mode 'mode-class) 'special)
           (not (derived-mode-p 'comint-mode 'eshell-mode)))
      (-some 'eval boon-special-conditions)
      (memq major-mode boon-special-mode-list)))

;;; Initialisation and activation

(define-minor-mode boon-local-mode
  "Minor mode for setting up command mode in a single buffer."
  :init-value nil
  :lighter (:eval (boon-modeline-string))
  :keymap nil
  (when boon-local-mode
    (unless (memq 'boon/after-change-hook after-change-functions)
      (push 'boon/after-change-hook after-change-functions))
    (if (boon-special-mode-p)
        (boon-set-state 'boon-special-state)
     (boon-set-command-state))))

(add-hook 'minibuffer-setup-hook 'boon-minibuf-hook)

(defun boon-minibuf-hook ()
  "Set the cursor type to 'bar'.
This is because no command mode is activated in the minibuffer."
  (setq cursor-type 'bar))

;; The function `boon-initialize' should only be used to initialize
;; `boon-local-mode' from the globalized minor-mode `boon-mode'. It is
;; called whenever boon is enabled in a buffer for the first time or
;; when boon is active and the major-mode of the buffer changes.
(defun boon-initialize ()
  "Enable Boon in the current buffer, if appropriate.  To enable Boon globally, do (boon-mode 1)."
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

(provide 'boon-core)
;;; boon-core ends here
