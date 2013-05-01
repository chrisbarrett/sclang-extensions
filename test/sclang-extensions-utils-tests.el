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
(require 'ert)
(autoload 'check "test-runner")

;;; Response parsing
;;;
;;; Ensure that the response from SuperCollider is correctly deserialized and
;;; read into Lisp objects.
;;;
;;; sclang-mode is responsible for the interaction with the SuperCollider
;;; process, so we assume that works and just test whether the parsed
;;; representations look good.

(defmacro with-stubbed-response (response-string &rest body)
  "Rebind `scl:blocking-eval-string' to return RESPONSE-STRING in BODY."
  (declare (indent 1))
  `(flet ((scl:blocking-eval-string (&rest _args) ,response-string))
     ,@body))

(defmacro check-parses (desc response-string _-> expected )
  "Check that the given response from SuperCollider is parsed as expected.

* DESC describes the type of response being parsed.

* RESPONSE-STRING is the simulated response from SuperCollider.

* EXPECTED is the expect output by the parser."
  (declare (indent 1))
  `(check ,(concat "check parses " desc)
     (with-stubbed-response ,response-string
       (should (equal (scl:request ,response-string) ,expected)))))

(check-parses "Arrays to lists"
  "[1, 2, 3]" -> '(1 2 3))

(check-parses "Strings to strings"
 " \"foo\" " -> "foo")

(check-parses "empty strings to nil"
 "" -> nil)

(check-parses "blank strings to nil"
 " " -> nil)

(check-parses "Symbols to lisp symbols"
 "\\symbol" -> 'symbol)

(check-parses "sane requests only"
 "ERROR: " -> nil)

;;; Member access
;;;
;;; Basic checking for whether a given form is a member or not. This is helpful
;;; for determining whether to show auto-competion candidates for methods or
;;; classes.

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

;;; Expression parsing
;;;
;;; `scl:expression-start-pos' is used to extract type and context information
;;; around point. It does textual analysis to figure out where expressions begin
;;; so we can do basic type-inference for method-lookup. It's important to test
;;; it thoroughly.
;;;
;;; As a rule of thumb, `scl:expression-start-pos' ought to return the first
;;; non-whitespace character of the expression, or the start of the line if
;;; we're outside a brace context.

(cl-defmacro move-to-expr-start (desc before _-> after
                                      &key (point-marker-char "|"))
  "Check that a given motion moves POINT to an expected position.

* BEFORE and AFTER are strings to compare.

* DESC is a description of the test.

* POINT-MARKER-CHAR is the character that will represent the
  position of point in BEFORE and AFTER strings."
  (declare (indent 1))
  (cl-assert (equal (length before) (length after)))
  `(check ,(concat "check move to expression start " desc)
     (with-temp-buffer
       ;; Do all sorts of wacky string replacement. I could have just compared
       ;; the position of point against the pipe character, but comparing
       ;; strings gives you much better error feedback in ERT.
       (insert ,before)
       ;; delete the point marker in BEFORE
       (goto-char (1+ (s-index-of ,point-marker-char ,before)))
       (delete-char 1)
       (goto-char (scl:expression-start-pos))
       ;; put a marker where we are now.
       (insert ,point-marker-char)
       ;; assert that the buffer now looks like AFTER.
       (should (equal ,after (buffer-string))))))

(move-to-expr-start "stops at semicolon at same nesting level"
  "{ foo; foo| }" -> "{ foo;| foo }")

(move-to-expr-start "skips semicolon at different nesting level"
  " { foo; foo } |" -> "| { foo; foo } ")

(move-to-expr-start "stops at comma at same nesting level"
  "( foo, foo| )" -> "( foo,| foo )")

(move-to-expr-start "stops at plus operator at same nesting level"
  "( foo + foo| )" -> "( foo +| foo )")

(move-to-expr-start "stops at div operator at same nesting level"
  "( foo / foo| )" -> "( foo /| foo )")

(move-to-expr-start "stops at mult operator at same nesting level"
  "( foo * foo| )" -> "( foo *| foo )")

(move-to-expr-start "stops at neg operator at same nesting level"
  "( foo - foo| )" -> "( foo -| foo )")

(move-to-expr-start "stops at pipe operator at same nesting level"
  "( foo | foo% )" -> "( foo |% foo )"
  :point-marker-char "%")

(move-to-expr-start "skips comma at different nesting level"
  " ( foo, foo ) |" -> "| ( foo, foo ) ")

(move-to-expr-start "stops at comma before parenthesized expression"
  "( foo(), foo| )" -> "( foo(),| foo )")

(move-to-expr-start "stops at open brace at same nesting level"
  "{ foo| }" -> "{| foo }")

(move-to-expr-start "bounded at open brace"
  "{| foo }" -> "{| foo }")

(move-to-expr-start "no change when already at expression start brace"
  "{| foo }" -> "{| foo }")

(move-to-expr-start "bounded at open paren"
  "(| foo )" -> "(| foo )")

(move-to-expr-start "no change when already at expression start paren"
  "(| foo )" -> "(| foo )")

(move-to-expr-start "bounded at open square"
  "[| foo ]" -> "[| foo ]")

(move-to-expr-start "no change when already at expression start square"
  "[| foo ]" -> "[| foo ]")

(move-to-expr-start "skips over braces"
  "foo { bar } |" -> "|foo { bar } ")

(move-to-expr-start "skips over lists"
  " [1, 2, 3] |" -> "| [1, 2, 3] ")

(move-to-expr-start "skips over arglists"
  " foo(bar) |" -> "| foo(bar) ")

(move-to-expr-start "moves to start in multiline expression"
  "foo { \n bar \n } |" -> "|foo { \n bar \n } ")

;;; Class inference of literals
;;;
;;; Should infer the types of literals in code without communicating with
;;; SuperCollider.

(defmacro check-infers (expr _-> class)
  "Check that string EXPR is inferred be an instance of CLASS."
  `(check ,(format "check infers %s -> %s" expr class)
     (with-temp-buffer
       (insert ,expr)
       (goto-char (point-max))
       (should (equal (scl:class-of-thing-at-point)
                      ,(symbol-name class))))))

(check-infers "[1,2,3]"           -> Array)
(check-infers "[1,2,3].collect"   -> Array)
(check-infers "1"                 -> Integer)
(check-infers "1.pow"             -> Integer)
(check-infers " \"Hello\" "       -> String)
(check-infers " \"Hello\".world " -> String)
(check-infers " 'Hello' "         -> Symbol)
(check-infers " 'Hello'.method "  -> Symbol)
(check-infers " \\Symbol "        -> Symbol)
(check-infers " \\Symbol.method " -> Symbol)

;;; Request Caching
;;;
;;; Most structured calls to SuperCollider use `scl:defun-memoized' to ensure
;;; their results are cached. `scl:cached' is the underlying mechanism for this.

(check "caching macro returns uncached value"
  (let ((ht (make-hash-table)))
    (should (scl:cached "key" ht "value"))))

(check "caching macro caches result to given hash-table"
  (let ((ht (make-hash-table)))
    (scl:cached "key" ht "value")
    (should (equal "value" (gethash "key" ht)))))

(check "caching macro caches result to given hash-table"
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "key" "value" ht)
    (should (equal "value"
                   (scl:cached "key" ht
                     (error "Did not use cached value"))))))

(provide 'sclang-extensions-utils-tests)

;;; sclang-extensions-utils-tests.el ends here
