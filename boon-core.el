;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'face-remap)
;; Maps

(defvar boon-x-map (make-sparse-keymap))
(set-keymap-parent boon-x-map ctl-x-map)
(defvar boon-helm-command-map (make-sparse-keymap))
(defvar boon-moves-map (make-sparse-keymap) "Keymap for moves.")
(defvar boon-command-map (make-sparse-keymap) "Keymap used in Boon command mode.")
(set-keymap-parent boon-command-map boon-moves-map)
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
  "Set the boon state for this buffer."
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
  "(and push a mark to remember the last edition point)"
  (interactive) (boon-set-state 'boon-command-state))

(defun boon-set-off-state ()
  (interactive) (boon-set-state 'boon-off-state))

(defun boon-helm-set-insert-state ()
  (interactive)
  (setq boon-helm-command-state nil)
  (setq cursor-type 'bar))

(defun boon-helm-set-command-state ()
  (interactive)
  (setq boon-helm-command-state t)
  (setq cursor-type 'box))


;;; Initialisation and activation
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
  "Turn on Boon in the current buffer."
  (interactive)
  (boon-local-mode (or arg 1)))

(defun turn-off-boon-mode (&optional arg)
  "Turn off Boon in the current buffer."
  (interactive)
  (boon-local-mode (or arg -1)))

(defun boon-modeline-string ()
  "Return a string describing the current state."
  (concat " Boon:" (cond
   (boon-command-state "CMD")
   (boon-insert-state  "INS")
   (boon-off-state     "OFF")
   (t "???"))))

(provide 'boon-core)
;;; boon-core ends here

