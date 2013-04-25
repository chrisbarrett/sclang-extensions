;;; sclang-ac-mode-tests --- Tests for sclang-ac-mode

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

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

;; Tests for sclang-ac-mode

;;; Code:

(require 'ert)
(require 'dash)
(require 'sclang-ac-mode)

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

;;; ----------------------------------------------------------------------------

(with-temp-buffer (sclang-start))

(check "methods for SinOsc should contain AR"
  (should (-contains? (-flatten (scd--methods "SinOsc.class"))
                      'ar)))

(provide 'sclang-ac-mode-tests)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; sclang-ac-mode-tests.el ends here
