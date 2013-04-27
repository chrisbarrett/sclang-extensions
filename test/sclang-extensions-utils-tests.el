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
(require 's)
(autoload 'check "test-runner")

;;; Response parsing

(defmacro with-stubbed-response (response-string &rest body)
  "Rebind `scl:blocking-eval-string' to return RESPONSE-STRING in BODY."
  (declare (indent 1))
  `(flet ((scl:blocking-eval-string (&rest _args) ,response-string))
    ,@body))

(defmacro check-parses (desc _sep response-string _-> expected )
  "Check that the given response from SuperCollider is parsed to expected.
* DESC describes the type of response being parsed.
* RESPONSE-STRING is simulates a response from SuperCollider.
* EXPECTED is the that should be output by the parser."
  `(check ,(concat "check parses " desc)
     (with-stubbed-response ,response-string
       (should (equal ,expected (scl:request "SHOULD BE STUBBED OUT"))))))

(check-parses "Arrays to lists"      : "[1, 2, 3]" -> '(1 2 3))

(check-parses "Strings to strings"   : " \"foo\" " -> "foo")

(check-parses "empty strings to nil" : ""          -> nil)

(check-parses "blank strings to nil" : " "         -> nil)

;;; Syntax

(check "foo.bar form is understood as a member access"
  (with-temp-buffer
    (insert "foo.bar")
    (should (scl:looking-at-member-access?))))

(check "foobar form is not understood as a member access"
  (with-temp-buffer
    (insert "foobar")
    (should (not (scl:looking-at-member-access?)))))

(check "foo(bar) form is not understood as a member access"
  (with-temp-buffer
    (insert "foo(bar)")
    (should (not (scl:looking-at-member-access?)))))

(check "foo [bar] form is not understood as a member access"
  (with-temp-buffer
    (insert "foo [bar]")
    (should (not (scl:looking-at-member-access?)))))

(defmacro move-to-expr-start (desc before _-> after)
  "Check that a given motion moves point to an expected position.
* ACTION is the movement to apply.
* BEFORE and AFTER are strings, where a vertical pipe `|` represents POINT.
* DESC is a description of the test."
  (declare (indent 1))
  (cl-assert (equal (length before) (length after)))
  `(check ,(concat "check move to expression start " desc)
     (with-temp-buffer
       ;; Do all sorts of wacky string replacement. I could have just compared
       ;; the position of point against the pipe character, but comparing
       ;; strings gives you much better error feedback in ert.
       (insert ,before)
       ;; delete the pipe in BEFORE
       (goto-char (1+ (s-index-of "|" ,before)))
       (delete-char 1)
       (goto-char (scl:expression-start-pos))
       ;; put a pipe where we are now.
       (insert "|")
       ;; assert that the buffer now looks like AFTER.
       (should= ,after (buffer-string)))))

(move-to-expr-start "stops at semicolon at same nesting level"
  "{ foo; foo| }" -> "{ foo;| foo }")

(move-to-expr-start "skips semicolon at different nesting level"
  " { foo; foo } |" -> "| { foo; foo } ")

(move-to-expr-start "skips over braces"
  "foo { bar } |" -> "|foo { bar } ")

(move-to-expr-start "skips over lists"
  " [1, 2, 3] |" -> "| [1, 2, 3] ")

(move-to-expr-start "skips over arglists"
  " foo(bar) |" -> "| foo(bar) ")

(move-to-expr-start "moves to start in multiline expression"
  "foo { \n bar \n } |" -> "|foo { \n bar \n } ")

(provide 'sclang-extensions-utils-tests)

;;; sclang-extensions-utils-tests.el ends here
