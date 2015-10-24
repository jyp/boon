;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:


;; A region list has the following form: ((begining . end) (end . begining) ...)

;;; Code:

(defun boon-normalize-reg (reg)
  "Normalize the region REG by making sure car < cdr."
  (cons (boon-reg-begin reg) (boon-reg-end reg)))

(defun boon-reg-to-markers (reg)
  "Put copy the markers defining REG borders, and return that."
  (cons (copy-marker (car reg)) (copy-marker (cdr reg))))

(defun boon-borders (reg how-much)
  "Given a normalized region REG, return its borders (as a region list).
The size of the borders is HOW-MUCH."
  ;; TODO: if the results would tourch or overlap, return the input region
  (list (cons (boon-reg-end reg)   (- (boon-reg-end reg) how-much))
        (cons (boon-reg-begin reg) (+ (boon-reg-begin reg) how-much))))

(defun boon-reg-begin (reg)
  "The begining of region REG."
  (min (cdr reg) (car reg)))

(defun boon-reg-end (reg)
  "The end of region REG."
  (max (cdr reg) (car reg)))

(defun boon-content (reg)
  "Given a region REG, return its contents (crop the region by 1)."
  (cons (+ (boon-reg-begin reg) 1) (- (boon-reg-end reg) 1)))

(provide 'boon-regs)
;;; boon-regs.el ends here
