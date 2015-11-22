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
(defvar boon-helm-command-map (make-sparse-keymap))
(suppress-keymap boon-helm-command-map 't)
(defvar boon-moves-map (make-sparse-keymap) "Keymap for moves.")
(defvar boon-command-map (make-sparse-keymap) "Keymap used in Boon command mode.")
(set-keymap-parent boon-command-map boon-moves-map)
(suppress-keymap boon-command-map 't)  ; so that typing is disabled altogether in command mode
(defvar boon-select-map (make-sparse-keymap) "Keymap for non-moves text regions.")
(set-keymap-parent boon-select-map boon-moves-map)
(defvar boon-off-map (make-sparse-keymap))
(defvar boon-insert-map (make-sparse-keymap))

(defvar boon-mode-map-alist (list (cons 'boon-command-state boon-command-map)
                                  (cons 'boon-off-state     boon-off-map)
                                  (cons 'boon-insert-state  boon-insert-map)
                                  (cons 'boon-helm-command-state boon-helm-command-map)))
(push 'boon-mode-map-alist emulation-mode-map-alists)

;; States
(defvar-local boon-command-state nil)
(defvar-local boon-insert-state nil)
(defvar-local boon-off-state nil)
(defvar-local boon-helm-command-state nil
  "non-nil if the helm command mode is active. Makes sense only
  in a helm minibuffer.")

(defun boon-set-state (state)
  "Set the boon state (as STATE) for this buffer."
  (setq boon-command-state nil)
  (setq boon-insert-state nil)
  (setq boon-off-state nil)

  (set state t)
  (unless boon-command-state
    (deactivate-mark)
    (save-excursion
      (when (not (bolp))
        (let ((orig (point)))
          (skip-chars-forward " " (line-end-position))
          (when (eolp) (delete-region orig (point))))))
    (setq cursor-type 'bar))
  (cond (boon-command-state
         ;; (do-auto-save)
         (setq cursor-type 'box)
           )
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

(defun boon-helm-set-insert-state ()
  "Switch to insert state in an helm minibuffer."
  (interactive)
  (setq boon-helm-command-state nil)
  (setq cursor-type 'bar))

(defun boon-helm-set-command-state ()
  "Switch to command state in an helm minibuffer."
  (interactive)
  (setq boon-helm-command-state t)
  (setq cursor-type 'box))

(defcustom boon-special-mode-list
  '(
    Buffer-menu-mode
    Custom-mode
    completion-list-mode
    debugger-mode
    dired-mode
    ediff-mode
    git-rebase-mode
    magit-branch-manager-mode
    magit-key-mode
    magit-log-mode
    magit-popup-mode
    magit-refs-mode
    magit-revision-mode
    magit-status-mode
    package-menu-mode
    mu4e-headers-mode
    mu4e-view-mode
    mu4e-main-mode
    Info-mode
    )
    "List of modes which start in boon-off-state, and go back to off state instead of inserting."
    :group 'boon)

(defun boon-special-mode-p ()
  "Is the major mode in boon-special-mode-list?"
  (memq major-mode boon-special-mode-list))

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
      (boon-set-off-state))
     ((memq major-mode '(magit-commit-mode
                         git-commit-mode
                         ))
      (boon-set-insert-like-state))
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
  "Detect if the minibuffer is a helm minibuffer, and activate boon helm command mode if so."
  (if (and (bound-and-true-p helm--minor-mode)
           (not (equal (minibuffer-prompt) "M-x ")
           ;; another variables to check for special conditions could be helm-map
           ))
      (boon-helm-set-command-state)
    (setq cursor-type 'bar)))


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
   (t "???")))

(provide 'boon-core)
;;; boon-core ends here

