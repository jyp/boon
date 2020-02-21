;;; boon-test.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module tests boon.

;;; Code:

(package-initialize)
(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-refresh-contents)

(dolist (pkg '(multiple-cursors
               dash
               expand-region
               powerline ;; only for boon-powerline
               spaceline ;; only for boon-spaceline
               ))
  (package-install pkg)
  (package-activate pkg))

(add-to-list 'load-path default-directory)

(require 'boon)

(ert-deftest boon-open-next-line-and-insert-at-buffer-end ()
  (should (string=
           "first-line\n\n"
           (with-temp-buffer
             (insert "first-line")
             (goto-char 5)
             (boon-open-next-line-and-insert)
             (buffer-substring-no-properties 1 (point-max))))))



;; Untested:
;; "boon-core.el"

(require 'dash)

(setq byte-compile-error-on-warn t)
(unless
 (-all? #'byte-compile-file
        '("boon.el"
          "boon-arguments.el"
          "boon-colemak.el"
          "boon-core.el"
          "boon-emacs.el"
          "boon-keys.el"
          "boon-main.el"
          "boon-moves.el"
          "boon-pkg.el"
          "boon-powerline.el"
          "boon-qwerty.el"
          "boon-qwertz.el"
          "boon-regs.el"
          "boon-search.el"
          "boon-spaceline.el"
          "boon-test.el"
          "boon-tutorial.el"
          "boon-utils.el"
          "boon-workman.el"))
 (kill-emacs 1))

(ert-run-tests-batch-and-exit)

;;; boon-test.el ends here
