;;; sclang-extensions-utils.el --- Subprocess communication and parsing utilities.

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

;; Subprocess communication and parsing utilities.

;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)
(require 'cl) ; needed for `flet'
(autoload 'sclang-eval-string "sclang-help")
(autoload 'thing-at-point-looking-at "thingatpt")

;;; ----------------------------------------------------------------------------

(defcustom sclang-ac-verbose nil
  "If non-nil, print extra debugging info to the messages buffer."
  :group 'sclang-extensions)

;;; ----------------------------------------------------------------------------
;;; Communication

(cl-defun scl:blocking-eval-string (expr &optional (timeout-ms 50))
  "Ask SuperCollider to evaluate the given string EXPR. Wait a maximum TIMEOUT-MS."
  (unless (s-blank? expr)
    (let ((result nil)
          (elapsed 0)
          ;; Prevent expressions from crashing sclang.
          (fmt (format "try { Emacs.message((%s).asCompileString) } {|err| err;}" expr))
          )
      ;; SuperCollider will eval the string and then call back with the result.
      ;; We rebind Emacs' `message' action to intercept the response.

      (flet ((message (str &rest _) (setq result str)))

        (sclang-eval-string fmt)

        ;; Block until we receive a response or the timeout expires.
        (while (and (not result) (> timeout-ms elapsed))
          (sleep-for 0 10)
          (setq elapsed (+ 10 elapsed)))
        result))))

(defun scl:deserialize (str)
  "Parse the SuperCollider response STR."
  (when str
    (->> str
      ;; Parse SuperCollider arrays to lists.
      (s-replace "," "")
      (s-replace "[" "(")
      (s-replace "]" ")")
      (read))))

(cl-defun scl:request (format-string &rest args)
  "Define a blocking request to SuperCollider.
Empty responses are returned as nil.
Requests that appear malformed are also ignored unless UNSAFE? is non-nil."
  (let ((request (apply 'format format-string args)))
    ;; Validate input is not a cycle of bad..
    (unless (s-contains? "ERROR:" request)
      (let ((response (scl:blocking-eval-string request)))
        (unless (or (null response)
                    (and (stringp response)
                         (s-blank? (s-trim response))))
          (scl:deserialize response))))))

(defmacro scl:logged (&rest body)
  "Like `progn', but logs the result to messages if `sclang-ac-verbose' is non-nil."
  (declare (indent 0))
  (let ((result (cl-gensym)))
    `(let ((,result (progn ,@body)))
       (when sclang-ac-verbose
         (message "[scl] %s" ,result))
       ,result)))

(defmacro scl:cached (key table &rest body)
  "Return the value for KEY in hash-table TABLE.
If KEY is not found, evaluate BODY forms and insert the result into the table."
  (declare (indent 2))
  (let ((-key   (cl-gensym))
        (-table (cl-gensym)))
    `(let ((,-key ,key)
           (,-table  ,table))
       (or (gethash ,-key ,-table)
           (let ((value (progn ,@body)))
             (puthash ,-key value ,-table)
             value)))))

(defmacro scl:defun-memoized (name arguments docstring &rest body)
  "Define a memoized function.
Will return the cached value for ARGUMENTS on subsequent calls."
  (declare (indent defun) (doc-string 3))
  (let ((cache-name (intern (format "%s-cache" name))))
    `(eval-and-compile

       ;; Define backing field.
       (defvar ,cache-name
         (make-hash-table :test 'equal)
         ,(format "Auto-generated cache for `%s'" name))

       ;; Define function.
       (defun ,name ,arguments
         ,docstring
         (scl:cached (list ,@arguments) ,cache-name
           ,@body)))))

;;; ----------------------------------------------------------------------------
;;; Reflection

(scl:defun-memoized scl:methods (class)
  "Return a list of methods implemented by CLASS."
  (unless (s-blank? class)
    (scl:request "%s.methods.collect {|m| [m.name, m.argList, m.ownerClass] }"
                 class)))

(scl:defun-memoized scl:all-methods (class)
  "Return a list of methods implemented by CLASS and its superclasses."
  (unless (s-blank? class)
    (->> (cons class (scl:superclasses class))
      (-mapcat 'scl:methods)
      (-uniq))))

(scl:defun-memoized scl:instance-vars (class)
  "Return a list of the instance variables of CLASS."
  (unless (s-blank? class)
    (scl:request "%s.instVarNames.collect(_.asString)" class)))

(scl:defun-memoized scl:class-vars (class)
  "Return a list of the class variables of CLASS."
  (unless (s-blank? class)
    (scl:request "%s.classVarNames.collect(_.asString)" class)))

(scl:defun-memoized scl:superclasses (class)
  "Return a list of superclasses for CLASS."
  (unless (s-blank? class)
    (-map 'symbol-name (scl:request "%s.superclasses" class))))

(scl:defun-memoized scl:subclasses (class)
  "Return the direct subclasses of CLASS."
  (unless (s-blank? class)
    (-map 'symbol-name (scl:request "%s.subclasses" class))))

(scl:defun-memoized scl:class-summary (class)
  "Return the summary for the given class."
  (unless (s-blank? class)
    (-when-let (response (scl:request "SCDoc.documents[\"Classes/%s\"].summary" class))
      ;; Ensure the class summary ends with a period.
      (if (s-ends-with? "." response)
          response
        (format "%s." response)))))

(defun scl:class-of (expr)
  "Evaluate EXPR and return the class of the result."
  (unless (s-blank? expr)
    (scl:blocking-eval-string (format "(%s).class" expr))))

(defun scl:ensure-non-meta-class (class)
  "Make sure that the given CLASS name is not prefixed by Meta_.
This is necessary when looking up documentation, because class
methods are actually instance methods of the meta-class."
  (s-chop-prefix "Meta_" class))

(scl:defun-memoized scl:method-arg-info (class method-name)
  "Get the name and description of each argument for a method. "
  (let ((k (scl:ensure-non-meta-class class)))
    (or
     ;; Try class method.
     (scl:request
      (concat "SCDoc.getMethodDoc(\"%s\", \"*%s\")"
              ".findChild(\\METHODBODY)"
              ".findChild(\\ARGUMENTS).children.collect{|x| "
              "[x.text, x.findChild(\\PROSE).findChild(\\TEXT).text] "
              "} ") k method-name)
     ;; Try instance method.
     (scl:request
      (concat "SCDoc.getMethodDoc(\"%s\", \"-%s\")"
              ".findChild(\\METHODBODY)"
              ".findChild(\\ARGUMENTS).children.collect{|x| "
              "[x.text, x.findChild(\\PROSE).findChild(\\TEXT).text] " "} ")
      k method-name))))

(scl:defun-memoized scl:all-classes ()
  "Return the list of all classes known to SuperCollider."
  (->> "Class.allClasses.asArray"
    (scl:blocking-eval-string)
    (s-replace "class" "")
    (scl:deserialize)
    (-map 'symbol-name)))

;;; ----------------------------------------------------------------------------
;;; Syntax

(defconst scl:close-braces (rx (any "}" ")" "]")))
(defconst scl:open-braces  (rx (any "{" "(" "[")))

(defun scl:between? (n start end)
  "Non-nil if N is between START and END, inclusively."
  (and (>= n start) (<= n end)))

(defun scl:char-before-point-looking-at? (regex)
  "Like `thing-before-point-looking-at', but strictly for the char before point."
  (unless (equal (point) (point-min))
    (s-matches? regex (buffer-substring (1- (point)) (point)))))

(defun scl:open-brace-position ()
  "Return point if on open braces, otherwise search backwards for open braces."
  (if (scl:char-before-point-looking-at? scl:open-braces)
      (point)
    (save-excursion
      (search-backward-regexp scl:open-braces nil t))))

(defun scl:close-brace-position ()
  (if (scl:char-before-point-looking-at? scl:close-braces)
      (point)
    (save-excursion
      (search-forward-regexp scl:close-braces nil t))))

(cl-defun scl:find-enclosing-braces-forward (&optional (pos (point)))
  "Find the extents of braces surrounding POS, looking forwards."
  (save-excursion
    (goto-char pos)
    (-when-let (end (scl:close-brace-position))
      (goto-char end)
      (ignore-errors (backward-sexp))
      (when (scl:between? pos (point) end)
        (cons (point) end)))))

(cl-defun scl:find-enclosing-braces-backward (&optional (pos (point)))
  "Find the extents of braces surrounding POS, looking backward."
  (save-excursion
    (goto-char pos)
    (-when-let (start (scl:open-brace-position))
      (goto-char start)
      (ignore-errors (forward-sexp))
      (when (scl:between? pos start (point))
        (cons start (point))))))

(cl-defun scl:surrounding-braces (&optional (pos (point)))
  "If POS is inside a set of balanced braces return a cons, else nil.
The car is the opening brace position and the cdr is its matching
closing brace position."
  ;; Search both forward and backward to make it more likely to work for
  ;; unbalanced expressions.
  (let ((forward (scl:find-enclosing-braces-forward pos))
        (back    (scl:find-enclosing-braces-backward pos)))
    (if (and forward back)
        ;; Return the narrowest enclosing set of braces.  This will be the pair
        ;; with the higher starting position.
        (if (<= (car forward) (car back))
            back forward)
      ;; Return the match we have.
      (or forward back))))

(defun scl:find-delimiter-backwards ()
  "Find the first delimiter backwards within the current context."
  (save-excursion
    (search-backward-regexp (rx (any "," ";" ":" "%" "+" "*" "/" "-" "=" "|"))
                            ;; Braces define surrounding context.
                            (car (scl:surrounding-braces)) t)))

(cl-defun scl:expression-start-pos (&optional (pt (point)))
  "Return the start of the current sclang expression."
  (save-excursion
    (goto-char pt)
    (let ((delimiter (scl:find-delimiter-backwards))
          (context (scl:surrounding-braces pt)))
      (cond
       ((scl:char-before-point-looking-at? scl:close-braces)
        (backward-sexp)
        (unless (bobp) (forward-char -1))
        (scl:expression-start-pos))

       ((scl:char-before-point-looking-at? scl:open-braces)
        (point))

       ;; Go to delimiters at the current level of nesting.
       ((and delimiter (equal (scl:surrounding-braces (1+ delimiter)) context))
        (goto-char (1+ delimiter)))

       ;; Go to the start of the current context.
       (context
        (goto-char (1+ (car context))))

       ;; Skip over braced expressions in the current context.
       ((or (thing-at-point-looking-at (rx (any "}" "]" "]" ")" "\"")))
            (search-backward-regexp (rx (any "}" "]" "]" ")" "\"") (* nonl)) nil t))
        (forward-char)                  ; Move to position after brace.
        (backward-sexp)                 ; Skip over braces.
        (ignore-errors
          (forward-char -1))            ; Move to position before brace.
        (scl:expression-start-pos))     ; Recur and continue search backward.

       ;; Otherwise return the start of the line.
       (t
        (line-beginning-position))))))

(defun scl:class-of-thing-at-point ()
  "Return the class of the sclang expression at point."
  (scl:logged
    ;; Find the first sclang token in the expression.
    (let* ((words
            (-map 's-trim
                  (-> (buffer-substring-no-properties
                       (scl:expression-start-pos)
                       (point))
                    (s-trim)
                    (split-string (rx (any "."))))))
           (token (nth 0 words))
           (next  (nth 1 words)))
      (cond
       ;; Return immediately for literals.
       ((null token)                nil)
       ((s-starts-with? "[" token)  "Array")
       ((s-ends-with? "]" token)    "Array")
       ((s-starts-with? "\"" token) "String")
       ((s-ends-with? "\"" token)   "String")
       ((s-starts-with? "\\" token) "Symbol")
       ((s-starts-with? "'" token)  "Symbol")
       ((s-starts-with? "~" token)  "Buffer")
       ((and (s-numeric? token)
             next
             (s-numeric? next)      "Float"))
       ((s-numeric? token)          "Integer")
       ;; Evaluate with SuperCollider.
       (t
        (-when-let (response (scl:class-of token))
          (unless (s-starts-with? "ERROR" response)
            response)))))))

(defun scl:looking-at-member-access? ()
  "Return point if not looking at a member access."
  (s-contains? "." (buffer-substring (scl:expression-start-pos) (point))))

;;; ----------------------------------------------------------------------------
;;; Formatting functions

(defun scl:arguments (arglist)
  "Split the given arglist into a list of its arguments."
  (->> arglist (s-chop-prefix "(") (s-chop-suffix ")") (s-split (rx space))))


(provide 'sclang-extensions-utils)

;;; NB: We need to use `flet', an obsolete macro. Suppress the usage warning.

;; Local Variables:
;; byte-compile-warnings: (not obsolete cl-functions)
;; End:

;;; sclang-extensions-utils.el ends here
