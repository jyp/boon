;;; boon --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:


;; A region list has the following form: ('region (begining . end) (begining . end) ...)

;;; Code:

(defun boon-normalize-reg (reg)
  "Normalize the region REG by making sure beginning < end."
  (cons (min (cdr reg) (car reg)) (max (cdr reg) (car reg))))

(defun boon-reg-to-markers (reg)
  "Put copy the markers defining REG borders, and return that."
  (cons (copy-marker (car reg)) (copy-marker (cdr reg))))

(defun boon-borders (reg how-much)
  "Given a normalized region REG, return its borders, whose size is HOW-MUCH."
  (list (cons (cdr reg) (- (cdr reg) how-much))
        (cons (car reg) (+ (car reg) how-much))))

(defun boon-content (reg)
  "Given a normalized region REG, return its contents (crop the region by 1)."
  (cons (+ (car reg) 1) (- (cdr reg) 1)))

(provide 'boon-regs)
;;; boon-regs.el ends here
