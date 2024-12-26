;;; boon.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;; Package-Version: 1.3
;; Package-Requires: ((emacs "26.1") (dash "2.12.0") (expand-region "0.10.0") (multiple-cursors "1.3.0"))

;; SPDX-License-Identifier: GPL-3.0-only

;;; Commentary:

;; Boon brings modal editing capabilities to Emacs and...
;;
;; - It tries to be as ergonomic as possible.
;; - It remains lightweight (~300 loc for its core.)
;; - It attempts to integrate with Emacs as smoothly as possible

;;; Code:

(require 'boon-loaddefs)
(require 'boon-keys)
(require 'boon-core)

(provide 'boon)
;;; boon.el ends here
