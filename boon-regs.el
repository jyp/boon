;;; boon-regs.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(defun boon-mk-reg (mrk pnt &optional cursor)
  "Make a region with MRK PNT and CURSOR."
  (list mrk pnt cursor))

(defun boon-reg-nil (reg)
  "Return t if either the point or the mark of REG is nil."
  (not (or (boon-reg-point reg) (boon-reg-mark reg))))

(defun boon-regs-from-bounds (bnds)
  "Convert BNDS as (mark . point) in to a list of boon regions."
  (when bnds
    (list (boon-mk-reg (car bnds) (cdr bnds) nil))))

(defun boon-reg-mark (reg) (nth 0 reg))

(defun boon-reg-point (reg) (nth 1 reg))

(defun boon-reg-cursor (reg) (nth 2 reg))

(defun boon-reg-to-markers (reg)
  "Put copy the markers defining REG borders, and return that."
  (boon-mk-reg (copy-marker (boon-reg-mark reg)) (copy-marker (boon-reg-point reg)) (boon-reg-cursor reg)))

(defun boon-reg-from-markers (reg)
  "Put convert markers to numbers in REG."
  (boon-mk-reg (marker-position (boon-reg-mark reg)) (marker-position (boon-reg-point reg)) (boon-reg-cursor reg)))

(defun boon-borders (reg how-much)
  "Given a normalized region REG, return its borders (as a region list).
The size of the borders is HOW-MUCH."
  ;; TODO: if the results would touch or overlap, return the input region
  (list (boon-mk-reg (boon-reg-end reg)   (- (boon-reg-end reg) how-much) (boon-reg-cursor reg))
        (boon-mk-reg (boon-reg-begin reg) (+ (boon-reg-begin reg) how-much) (boon-reg-cursor reg))))

(defun boon-include-surround-spaces (reg)
  "Return REG, extended to include spaces around 'boon-reg-point'.
The spaces are searched after 'boon-regpoint' if the region is
directed forward, or or before, if the region is backwards."
  (save-excursion
    (let* ((mk (boon-reg-mark  reg))
           (pt (boon-reg-point reg))
           (fwd (> pt mk))
           (skip-pt (if fwd 'skip-syntax-forward 'skip-syntax-backward))
           (skip-mk (if fwd 'skip-syntax-backward 'skip-syntax-forward))
           (pt-ext (progn (goto-char pt) (funcall skip-pt "-") (point))))
      (boon-mk-reg (if (equal pt-ext pt) (progn (goto-char mk) (funcall skip-mk "-") (point)) mk)
                   pt-ext
                   (boon-reg-cursor reg)))))

(defun boon-reg-begin (reg)
  "The begining of region REG."
  (min (boon-reg-point reg) (boon-reg-mark reg)))

(defun boon-reg-end (reg)
  "The end of region REG."
  (max (boon-reg-point reg) (boon-reg-mark reg)))

(defun boon-content (reg)
  "Given a region REG, return its contents (crop the region by 1)."
  (boon-mk-reg (+ (boon-reg-begin reg) 1) (- (boon-reg-end reg) 1) (boon-reg-cursor reg)))

(defun boon-reg-before (r1 r2)
  "Return non-nil when R1 occurs before R2."
  (< (boon-reg-begin r1) (boon-reg-end r2)))

(defun boon-reg-cur-after (r1 r2)
  (declare (obsolete "unused" "July 2017"))
  (or (not (boon-reg-cursor r1))
      (and (boon-reg-cursor r2)
           (> (overlay-end (boon-reg-cursor r1))
              (overlay-end (boon-reg-cursor r2))))))

(provide 'boon-regs)
;;; boon-regs.el ends here
