;;; boon.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; Boon brings modal editing capabilities to Emacs and...
;;
;; - It tries to be as ergonomic as possible.
;; - It remains lightweight (~300 loc for its core.)
;; - It attempts to integrate with Emacs as smoothly as possible

;;; Code:

(require 'boon-main)
(require 'boon-keys)
(require 'boon-search)
(require 'boon-core)
(require 'boon-moves)

(provide 'boon)
;;; boon.el ends here
