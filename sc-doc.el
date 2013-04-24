;;; sc-doc --- SuperCollider documentation bindings.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
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

;; SuperCollider documentation bindings.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'auto-complete)
(autoload 'sclang-eval-string "sclang-help")

(cl-defun scd--blocking-eval-string (expr &optional (timeout-ms 100))
  "Ask SuperCollider to evaluate the given string EXPR. Wait a maximum TIMEOUT-MS."
  (let ((result nil)
        (elapsed 0)
        ;; Prevent expressions from crashing sclang.
        (fmt (format "try { Emacs.message((%s).asCompileString) } {|err|}" expr))
        )
    ;; SuperCollider will eval the string and then call back with the result.
    ;; We rebind Emacs' `message' action to intercept the response.

    (flet ((message (str &rest _) (setq result str)))

      (sclang-eval-string fmt)

      ;; Block until we receive a response or the timeout expires.
      (while (and (not result) (> timeout-ms elapsed))
        (sleep-for 0 10)
        (setq elapsed (+ 10 elapsed)))
      result)))

(defun scd--deserialize (str)
  "Parse the SuperCollider response STR."
  (->> str
    ;; Parse SuperCollider arrays to lists.
    (s-replace "," "")
    (s-replace "[" "(")
    (s-replace "]" ")")
    (read)))

(defun scd--methods (class)
  "Return a list of methods implemented by CLASS."
  (->> (concat class ".methods.collect {|m| [m.name, m.argList, m.ownerClass] } ")
    (scd--blocking-eval-string)
    (scd--deserialize)))

(defun scd--instance-vars (class)
  "Return a list of the instance variables of CLASS."
  (->> (concat class ".instVarNames.collect(_.asString)")
    (scd--blocking-eval-string)
    (scd--deserialize)))

(defun scd--class-vars (class)
  "Return a list of the class variables of CLASS."
  (->> (concat class ".classVarNames.collect(_.asString)")
    (scd--blocking-eval-string)
    (scd--deserialize)))

(defun scd--class-of (expr)
  "Evaluate EXPR and return the class of the result."
  (scd--blocking-eval-string (format "(%s).class" expr)))

(defun scd--all-classes ()
  "Return the list of classes known to SuperCollider."
  (->> "Class.allClasses.asArray"
      (scd--blocking-eval-string)
      (s-replace "class" "")
      (scd--deserialize)))

(defun sclang--expression-to-point ()
  "Return the sclang expression on the current line up to point."
  (->> (buffer-substring-no-properties (line-beginning-position) (point))
    ;; Remove trailing `.` (member accessor)
    (s-trim)
    (s-chop-suffix ".")))

;;; ----------------------------------------------------------------------------
;;; Auto-completion

(defvar scd--known-classes (-map 'symbol-name (scd--all-classes))
  "A cache of all class names known to SuperCollider.")

(defun scd--class-of-thing-at-point ()
  "Test whether the thing at point is a class name.
Return the name of the class if looking at a class name.
Otherwise evaluate the expression to determine its class."
  (let ((expr (sclang--expression-to-point)))
    (if (-contains? scd--known-classes expr)
        expr
      (scd--class-of expr))))

(defmacro when-sclang-class (varname &rest body)
  "Bind the sclang expression at point to VARNAME and execute BODY forms."
  (declare (indent 1))
  `(let ((,varname (scd--class-of-thing-at-point)))
     (when ,varname
       ,@body)))

(defvar scd--ac-method-cache nil
  "Cache containing methods for auto-complete.")

(defun scd--ac-init-methods (class)
  "Get the list of methods for CLASS and process the list for auto-complete."
  (--map (destructuring-bind (name arglist owner) it
           ;; Turn method name symbol to string.
           (list (symbol-name (eval name))
                 (eval arglist)
                 (symbol-name owner)))
         (scd--methods class)))

(defun scd--ac-method-documentation (method-name)
  "Format a documentation page for METHOD-NAME."
  (destructuring-bind (name arglist owner)
      (assoc method-name scd--ac-method-cache)
    ;; Build docstring.
    (s-concat
     (format "%s.%s\n\n" owner name)
     (unless (s-blank? arglist) arglist))))

;;; Method completion source.
(ac-define-source sclang-methods
  '((init       . (setq scd--ac-method-cache (when-sclang-class k
                                              (scd--ac-init-methods k))))
    (candidates . (-map 'car scd--ac-method-cache))
    (document   . scd--ac-method-documentation)
    (symbol     . "f")))

;;; Instance variable completion source.
(ac-define-source sclang-ivars
  '((candidates . (when-sclang-class k
                    (scd--instance-vars k)))
    (symbol     . "v")))

;;; ----------------------------------------------------------------------------

(defun sclang-electric-dot ()
  "Open the auto-complete menu with candidates for the preceding sclang form."
  (interactive)
  (insert-string ".")
  (auto-complete (list ac-source-sclang-methods
                       ac-source-sclang-ivars)))

(when (boundp 'sclang-mode-map)
  (define-key sclang-mode-map (kbd ".") 'sclang-electric-dot))

(provide 'sc-doc)

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not obsolete)
;; End:

;;; sc-doc.el ends here
