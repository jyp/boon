;;; boon-core.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module sets up the emulation keymaps for each boon state.
;; Functions to switch to each state is also provided.

;;; Code:

(require 'cl-macs)
;; Maps

(defvar boon-c-map (make-sparse-keymap))
(defvar boon-x-map (make-sparse-keymap))
(set-keymap-parent boon-x-map ctl-x-map)

(defvar boon-moves-map (make-sparse-keymap) "Keymap for moves.")
(defvar boon-command-map (make-sparse-keymap) "Keymap used in Boon command mode.")
(set-keymap-parent boon-command-map boon-moves-map)
(suppress-keymap boon-command-map 't)  ; so that typing is disabled altogether in command mode
(defvar boon-select-map (make-sparse-keymap) "Keymap for non-moves text regions.")
(set-keymap-parent boon-select-map boon-moves-map)
(defvar boon-off-map (make-sparse-keymap))
(defvar boon-insert-map (make-sparse-keymap))
(defvar boon-special-map (make-sparse-keymap))

(defvar boon-mode-map-alist (list (cons 'boon-command-state boon-command-map)
                                  (cons 'boon-off-state     boon-off-map)
                                  (cons 'boon-special-state boon-special-map)
                                  (cons 'boon-insert-state  boon-insert-map)))
(push 'boon-mode-map-alist emulation-mode-map-alists)

;; States
(defvar-local boon-command-state nil)
(defvar-local boon-insert-state nil)
(defvar-local boon-off-state nil)
(defvar-local boon-special-state nil)


(defun boon-set-state (state)
  "Set the boon state (as STATE) for this buffer."
  (setq boon-command-state nil)
  (setq boon-insert-state nil)
  (setq boon-off-state nil)
  (setq boon-special-state nil)

  (set state t)
  (unless (or boon-command-state boon-special-state)
    (deactivate-mark)
    (save-excursion
      (when (not (bolp))
        (let ((orig (point)))
          (skip-chars-forward " " (line-end-position))
          (when (eolp) (delete-region orig (point))))))
    (setq cursor-type 'bar))
  (cond (boon-command-state
         ;; (do-auto-save)
         (setq cursor-type 'box))
        (boon-special-state (setq cursor-type 'box))
        (boon-off-state)
        (boon-insert-state
         ;; remember where the last edition was by pushing a mark
         (push-mark))
        (t (message "Unknown state!")))
  (force-mode-line-update))

(defun boon-set-insert-like-state ()
  "Switch to off or insert state, depending on mode."
  (interactive)
  (if (boon-special-mode-p) (boon-set-off-state) (boon-set-state 'boon-insert-state)))

(defun boon-set-insert-state ()
  "Switch to insert state."
  (interactive)
  (boon-set-state 'boon-insert-state))

(defun boon-set-command-state ()
  "Switch to command state and push a mark to remember the last edition point."
  (interactive) (boon-set-state 'boon-command-state))

(defun boon-set-off-state ()
  "Switch to off state."
  (interactive) (boon-set-state 'boon-off-state))

(defun boon-set-special-state ()
  "Switch to off state."
  (interactive) (boon-set-state 'boon-special-state))

(defcustom boon-special-mode-list
  '(
    Buffer-menu-mode
    debugger-mode
    ediff-mode
    git-rebase-mode
    ;; magit-revision-mode
    mu4e-headers-mode
    mu4e-view-mode
    )
    "A List of modes which should use boon-special-mode."
    :group 'boon)

(defun boon-special-mode-p ()
  "Should the mode use boon-special-mode?"
  (or
   (and (eq (get major-mode 'mode-class) 'special)
        (not (derived-mode-p 'comint-mode 'eshell-mode)))
   (memq major-mode boon-special-mode-list)))

;;; Initialisation and activation

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
    (cond
     ((boon-special-mode-p)
      (boon-set-state 'boon-special-state))
     (t (boon-set-command-state))))
   (t
    (boon-set-off-state)
    (message "Boon disabled")
    )))

;; No hooks are run in Fundamental buffers, so other measures are
;; necessary to initialize Boon in these buffers. When Boon is
;; enabled globally, the default value of `major-mode' is set to
;; `turn-on-boon-mode', so that Boon is enabled in Fundamental
;; buffers as well. Then, the buffer-local value of `major-mode' is
;; changed back to `fundamental-mode'. (Since the `boon-mode' function
;; is created by a macro, we use `defadvice' to augment it.)
(defadvice boon-mode (after start-boon activate)
  "Enable Boon in Fundamental mode."
  (if boon-mode
      (when (eq (default-value 'major-mode) 'fundamental-mode)
        ;; changed back by `boon-local-mode'
        (setq-default major-mode 'turn-on-boon-mode))
    (when (eq (default-value 'major-mode) 'turn-on-boon-mode)
      (setq-default major-mode 'fundamental-mode))))

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
(define-globalized-minor-mode boon-mode
  boon-local-mode boon-initialize)

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
   (boon-off-state     "OFF")
   (boon-special-state "SPC")
   (t "???")))

(provide 'boon-core)
;;; boon-core ends here

