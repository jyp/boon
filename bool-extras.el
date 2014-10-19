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
