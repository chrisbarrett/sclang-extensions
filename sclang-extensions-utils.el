;;; sclang-extensions-utils.el --- Subprocess communication and parsing utilities.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.2.3
;; Keywords: sclang supercollider languages tools

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
(autoload 'sclang-eval-string "sclang-help")
(autoload 'thing-at-point-looking-at "thingatpt")

(defcustom sclang-ac-verbose nil
  "If non-nil, print extra debugging info to the messages buffer."
  :group 'sclang-extensions)

;;; ----------------------------------------------------------------------------
;;; Communication

(defun* scl:blocking-eval-string (expr &optional (timeout-ms 50))
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

(defun scl:request (format-string &rest args)
  "Define a blocking request to SuperCollider.
Empty responses are returned as nil."
  (let ((response
         (scl:deserialize
          (scl:blocking-eval-string
           (apply 'format format-string args)))))
    (if (and (stringp response)
             (s-blank? response))
        nil
      response)))

(defmacro scl:logged (&rest body)
  "Like `progn', but logs the result to messages if `sclang-ac-verbose' is non-nil."
  (declare (indent 0))
  (let ((result (cl-gensym)))
    `(let ((,result (progn ,@body)))
       (when sclang-ac-verbose
         (message "[sclang-ac]: %s" ,result))
       ,result)))

;;; ----------------------------------------------------------------------------
;;; Reflection

(defun scl:methods (class)
  "Return a list of methods implemented by CLASS."
  (unless (s-blank? class)
    (scl:request "%s.methods.collect {|m| [m.name, m.argList, m.ownerClass] }"
                 class)))

(defun scl:all-methods (class)
  "Return a list of methods implemented by CLASS and its superclasses."
  (unless (s-blank? class)
    (->> (cons class (scl:superclasses class))
      (-mapcat 'scl:methods)
      (-uniq))))

(defun scl:instance-vars (class)
  "Return a list of the instance variables of CLASS."
  (unless (s-blank? class)
    (scl:request "%s.instVarNames.collect(_.asString)" class)))

(defun scl:class-vars (class)
  "Return a list of the class variables of CLASS."
  (unless (s-blank? class)
    (scl:request "%s.classVarNames.collect(_.asString)" class)))

(defun scl:superclasses (class)
  "Return a list of superclasses for CLASS."
  (unless (s-blank? class)
    (-map 'symbol-name (scl:request "%s.superclasses" class))))

(defun scl:subclasses (class)
  "Return the direct subclasses of CLASS."
  (unless (s-blank? class)
    (-map 'symbol-name (scl:request "%s.subclasses" class))))

(defun scl:class-summary (class)
  "Return the summary for the given class."
  (unless (s-blank? class)
    (scl:request "SCDoc.documents[\"Classes/%s\"].summary" class)))

(defun scl:class-of (expr)
  "Evaluate EXPR and return the class of the result."
  (unless (s-blank? expr)
    (scl:blocking-eval-string (format "(%s).class" expr))))

(defun scl:ensure-non-meta-class (class)
  "Make sure that the given CLASS name is not prefixed by Meta_.
This is necessary when looking up documentation, because class
methods are actually instance methods of the meta-class."
  (s-chop-prefix "Meta_" class))

(defun scl:method-arg-info (class method-name)
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

(defun scl:all-classes ()
  "Return the list of classes known to SuperCollider."
  (->> "Class.allClasses.asArray"
      (scl:blocking-eval-string)
      (s-replace "class" "")
      (scl:deserialize)
      (-map 'symbol-name)))

;;; ----------------------------------------------------------------------------
;;; Syntax

(defun scl:between? (n start end)
  "Non-nil if N is between START and END, inclusively."
  (and (>= n start) (<= n end)))

(defun* scl:find-enclosing-braces-forward (&optional (pos (point)))
  "Find the extents of braces surrounding POS, looking forwards."
  (save-excursion
    (-when-let (end (search-forward-regexp (rx (any "}" ")" "]")) nil t))
      (backward-sexp)
      (when (scl:between? pos (point) end)
        (cons (point) end)))))

(defun* scl:find-enclosing-braces-backward (&optional (pos (point)))
  "Find the extents of braces surrounding POS, looking backward."
  (save-excursion
    (-when-let (start (search-backward-regexp (rx (any "{" "(" "[")) nil t))
      (forward-sexp)
      (when (scl:between? pos start (point))
        (cons start (point))))))

(defun* slc:surrounding-braces (&optional (pos (point)))
  "If POS is inside a set of balanced braces return a cons, else nil.
The car is the opening brace and the cdr is its matching closing brace. "
  ;; Search both forward and backward to make it more likely to work for
  ;; unbalanced expressions.
  (let ((forward (scl:find-enclosing-braces-forward pos))
        (back    (scl:find-enclosing-braces-backward pos)))
    (if (and forward back)
        ;; If we have a match, find the narrowest enclosing set of braces.
        (cons
         (max (car forward) (car back))
         (min (cdr forward) (cdr back)))
      ;; Return the match we have.
      (or forward back))))

(defun* scl:expression-start-pos (&optional (pt (point)))
  "Return the start of the current sclang expression."
  (save-excursion
    (goto-char pt)
    (let* ((bol (line-beginning-position))
           (semicolon (save-excursion (search-backward ";" bol t)))
           (context (slc:surrounding-braces pt)))
      (cond
       ;; Go to semicolons at the current level of nesting.
       ((and semicolon (equal (slc:surrounding-braces semicolon) context))
        (goto-char (1+ semicolon)))

       ;; Go to the start of the current context.
       (context
        (goto-char (1+ (car context))))

       ;; Skip over braced expressions in the current context.
       ((or (thing-at-point-looking-at (rx (any "}" "]" "]" ")" "\"")))
            (search-backward-regexp (rx (any "}" "]" "]" ")" "\"") (* nonl)) nil t))
        (forward-char)                  ; Move to position after brace.
        (backward-sexp)                 ; Skip over braces.
        (forward-char -1)               ; Move to position before brace.
        (scl:expression-start-pos))     ; Recur and continue search backward.

       ;; Otherwise return the start of the line.
       (t
        (line-beginning-position))))))

(defun scl:class-of-thing-at-point ()
  "Return the class of the sclang expression at point."
  (scl:logged
    (->> (buffer-substring-no-properties (scl:expression-start-pos) (point))
      (s-trim)
      ;; Remove trailing dot-accessor.
      (s-chop-suffix ".")
      (scl:class-of))))

(defun scl:looking-at-member-access? ()
  "Return point if not looking at a member access."
  (s-contains? "." (buffer-substring (scl:expression-start-pos) (point))))


(provide 'sclang-extensions-utils)

;;; NB: We need to use `flet', an obsolete macro. Suppress the usage warning.

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; lexical-binding: t
;; End:

;;; sclang-extensions-utils.el ends here
