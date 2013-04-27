;;; test-runner.el --- Test runner for sclang-extensions.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: [yas] elisp error! Symbol's value as variable is void: insert-timestamp

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test runner for sclang-extensions.

;;; Code:

(message "--> Starting tests...")

(require 'ert)

(defmacro check (desc &rest body)
  "Wrap `ert-deftest' with a simpler interface.
* DESC is a string describing the test.
* BODY forms constitute the test."
  (declare (indent 1))
  `(ert-deftest
       ,(intern (replace-regexp-in-string "[ .]" "-" desc)) ()
     ,@body))

(defun should-match (regex str)
  "Assert REGEX is a match on string STR."
  (should (string-match-p regex str)))

(defun should= (x y)
  "Assert X is `equal' to Y."
  (should (equal x y)))

;;; Load tests

(message "--> Setting load-path...")

(defun expand-dir (dir)
  (file-name-as-directory (expand-file-name dir)))

(add-to-list 'load-path (expand-dir "."))
(add-to-list 'load-path (expand-dir ".."))
(add-to-list 'load-path (expand-dir "./test"))

(defun scl:run-tests-batch ()
  (message "--> Loading tests...")
  (require 'sclang-extensions-utils-tests)
  (message "--> Started.")
  (ert-run-tests-batch t))

(message "--> Ready.")

(provide 'test-runner)

;; Local Variables:
;; lexical-binding: t
;; no-byte-compile: t
;; End:

;;; test-runner.el ends here
