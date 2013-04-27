;;; sclang-extensions-utils-tests.el --- Tests for sclang-extensions-utils.

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

;; Tests for sclang-extensions-utils.

;;; Code:

(require 'sclang-extensions-utils)

(defmacro with-stubbed-response (response-string &rest body)
  "Rebind `scl:blocking-eval-string' to return RESPONSE-STRING in BODY."
  (declare (indent 1))
  `(flet ((scl:blocking-eval-string (&rest _args) ,response-string))
    ,@body))

(defmacro check-parses (response-string _-> expected _sep desc)
  "Check that the given response from SuperCollider is parsed to expected.
* DESC describes the type of response being parsed.
* RESPONSE-STRING is simulates a response from SuperCollider.
* EXPECTED is the that should be output by the parser."
  (declare (indent 2))
  `(check ,(concat "check parses " desc)
     (with-stubbed-response ,response-string
       (should (equal ,expected (scl:request "SHOULD BE STUBBED OUT"))))))

(check-parses "[1, 2, 3]" -> '(1 2 3) : "Arrays to lists")

(check-parses " \"foo\" " -> "foo"    : "Strings to strings")

(check-parses ""          -> nil      : "empty strings to nil")

(check-parses " "         -> nil      : "blank strings to nil")

(provide 'sclang-extensions-utils-tests)

;;; sclang-extensions-utils-tests.el ends here
