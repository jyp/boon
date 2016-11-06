;;; boon-test.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:

;; This module tests boon.

;;; Code:

(package-initialize)
(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-refresh-contents)

(package-install 'multiple-cursors)
(package-install 'dash)
(package-install 'expand-region)

(package-activate 'multiple-cursors)
(package-activate 'dash)
(package-activate 'expand-region)

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

(setq byte-compile-error-on-warn t)

(and
 (byte-compile-file "boon.el")
 (ert-run-tests-batch-and-exit))

;;; boon-test.el ends here
