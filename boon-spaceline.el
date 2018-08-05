;;; boon-spaceline.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module gives an example setup for spaceline using boon.

;;; Code:

(require 'boon-core)
(require 'boon-powerline)
(require 'spaceline-config)

;; This requires https://github.com/TheBB/spaceline/pull/201/commits/45c4c4b26d923c541ede138c3b3834e2f75778f8 to work.
(spaceline-define-segment boon
  "Boon status"
  (boon-state-string)
  :when (bound-and-true-p boon-mode)
  :face (if (powerline-selected-window-active) (boon-state-face) 'modeline-inactive))



(provide 'boon-spaceline)
;;; boon-spaceline.el ends here
