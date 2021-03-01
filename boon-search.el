;;; boon-search.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'boon-utils)
(require 'isearch)
(require 'dash)
(require 'boon-hl)

(defun boon-cur-pattern ()
  (car boon-hl-patterns))
              
(defun boon-qsearch (forward)
  "Search the `boon-cur-pattern'.
Do so in the direction specified (as FORWARD).  Point is set at
the beginning of the match."
  (if-let ((pattern (boon-cur-pattern)))
      (setq isearch-success
            (save-excursion
          (goto-char (if forward (1+ (point)) (1- (point))))
          (or (boon-hl-search pattern (not forward))
              (if isearch-success
                  (prog1 nil
                    (message "No more occurences %s" (if forward "below" "above")))
                (message "Wrapping around")
                (goto-char (if forward (point-min) (point-max)))
                (boon-hl-search pattern (not forward))))))
    (error "Nothing to search: boon-hl something before using boon-qsearch."))
  (when isearch-success (goto-char (match-beginning 0))))

;;;###autoload
(defun boon-qsearch-next ()
  "Search the next occurence of the current search regexp."
  (interactive)
  (boon-qsearch t))

;;;###autoload
(defun boon-qsearch-previous ()
  "Search the previous occurence of the current search regexp."
  (interactive)
  (boon-qsearch nil))

;;;###autoload
(defun boon-qsearch-next-at-point ()
  "Search the next occurence of the current string at point and select the match."
  (interactive)
  (boon-hl-symbol (boon-stuff-at-point))
  (boon-qsearch t)
  (deactivate-mark))

;;;###autoload
(defun boon-qsearch-previous-at-point ()
  "Search the previous occurence of the current string at point and select the match."
  (interactive)
  (boon-hl-symbol (boon-stuff-at-point))
  (boon-qsearch nil)
  (deactivate-mark))

;;;###autoload
(defun boon-navigate (forward)
  "Go to the next item of interest, FORWARD or backwards."
  (cond
   ((and (bound-and-true-p multiple-cursors-mode) (> (mc/num-cursors) 1))
    (if forward (mc/cycle-forward) (mc/cycle-backward)))
   ((boon-cur-pattern)
    (boon-qsearch forward))
   (t (next-error (if forward 1 -1)))))

;;;###autoload
(defun boon-navigate-forward ()
  "Go to the next item of interest."
  (interactive)
  (boon-navigate t))

;;;###autoload
(defun boon-navigate-backward ()
  "Go to the next item of interest."
  (interactive)
  (boon-navigate nil))


(provide 'boon-search)
;;; boon-search.el ends here
