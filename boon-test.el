(package-initialize t)
(list-packages)
(package-install 'multiple-cursors)
(package-install 'dash)
(package-install 'expand-region)

(add-to-list 'load-path default-directory)

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
 (require 'boon)
 (ert-run-tests-batch-and-exit))
