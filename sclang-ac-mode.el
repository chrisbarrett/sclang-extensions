;;; sclang-ac-mode.el --- Improved auto-complete for SuperCollider.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.2.2
;; Package-Requires: ((auto-complete "1.4.0") (s "1.3.1") (dash "1.2.0") (cl-lib "0.2") (emacs "24.1"))
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

;; Provides `sclang-ac-mode', a minor-mode that overrides the default
;; auto-complete behavior for sclang-mode.  Communicates with the SuperCollider
;; runtime to provide more intelligent auto-complete candidates.

;;; Installation:

;; Use Emacs' built-in package installer to install this package:
;; M-x package-install-file RET path/to/this/file.el
;;
;; Then add this mode to your sclang hooks:
;; (add-hook 'sclang-mode-hook 'sclang-ac-mode)

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'auto-complete)
(autoload 'sclang-eval-string "sclang-help")
(autoload 'thing-at-point-looking-at "thingatpt")

(cl-defun scd--blocking-eval-string (expr &optional (timeout-ms 100))
  "Ask SuperCollider to evaluate the given string EXPR. Wait a maximum TIMEOUT-MS."
  (unless (s-blank? expr)
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
        result))))

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

(defun scd--superclasses (class)
  "Return a list of superclasses for CLASS."
  (->> (concat class ".superclasses")
    (scd--blocking-eval-string)
    (scd--deserialize)
    (-map 'symbol-name)))

(defun scd--subclasses (class)
  "Return the direct subclasses of CLASS."
  (->> (concat class ".subclasses")
    (scd--blocking-eval-string)
    (scd--deserialize)
    (-map 'symbol-name)))

(defun scd--class-of (expr)
  "Evaluate EXPR and return the class of the result."
  (scd--blocking-eval-string (format "(%s).class" expr)))

(defun scd--all-classes ()
  "Return the list of classes known to SuperCollider."
  (->> "Class.allClasses.asArray"
      (scd--blocking-eval-string)
      (s-replace "class" "")
      (scd--deserialize)
      (-map 'symbol-name)))

(defun scd--expression-to-point ()
  "Return the sclang expression on the current line up to point."
  (->> (buffer-substring-no-properties (line-beginning-position) (point))
    ;; Remove trailing `.` (member accessor)
    (s-trim)
    (s-chop-suffix ".")))

;;; ----------------------------------------------------------------------------
;;; Auto-completion

(defvar scd--known-classes nil
  "A cache of all class names known to SuperCollider.")

(defvar scd--ac-method-cache nil
  "Cache containing methods for auto-complete.")

(defun scd--class-of-thing-at-point ()
  "Test whether the thing at point is a class name.
Return the name of the class if looking at a class name.
Otherwise evaluate the expression to determine its class."
  (let ((expr (scd--expression-to-point)))
    (if (-contains? scd--known-classes expr)
        expr
      (scd--class-of expr))))

(defmacro when-sclang-class (varname &rest body)
  "Bind the sclang expression at point to VARNAME and execute BODY forms."
  (declare (indent 1))
  `(-when-let (,varname (scd--class-of-thing-at-point))
     ,@body))

(defun scd--ac-init-methods (class)
  "Get the list of methods for CLASS and process the list for auto-complete."
  (--map (destructuring-bind (name arglist owner) it
           ;; Turn method name symbol to string.
           (list (symbol-name (eval name))
                 (eval arglist)
                 (symbol-name owner)))
         (scd--methods class)))

(defun scd--ac-method-documentation (method)
  "Format a help popup for METHOD."
  (destructuring-bind (name arglist owner)
      (assoc method scd--ac-method-cache)
    ;; Build docstring.
    (s-concat
     (format "%s.%s\n\n" owner name)
     (unless (s-blank? arglist) arglist))))

(defun scd--ac-class-documentation (class)
  "Format a help popup for CLASS."
  (let* ((super (s-join " < " (scd--superclasses class)))
         ;; Take a set number of subclasses before ellispsizing.
         (max-subs 5)
         (bullet "\n• ")
         (subclasses (scd--subclasses class))
         (sub-str    (->> subclasses
                       (-take max-subs)
                       (s-join bullet )
                       (s-prepend bullet))))
    (s-concat
     ;; Show either the class name or its inheritance hierarchy.
     (if (s-blank? super) (format "%s: %s" class super) class)
     ;; Show list of subclasses. It will be ellipsized if longer than MAX-SUBS.
     (when subclasses
       (format "\n\nsubclasses:%s"
               (if (< max-subs (length subclasses))
                   (s-append "\n  …" sub-str)
                 sub-str))))))

(defun scd--looking-at-member-access? ()
  "Non-nil if the expression at point is a member access."
  (save-excursion
    (forward-word -1)
    (thing-at-point-looking-at "[.]")))

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

;;; Class completion source.
(ac-define-source sclang-classes
  '((init       . (setq scd--known-classes (scd--all-classes)))
    (candidates . (unless (scd--looking-at-member-access?)
                    scd--known-classes))
    (document   . scd--ac-class-documentation)
    (symbol     . "s")))

;;; ----------------------------------------------------------------------------

(defun sclang-electric-dot ()
  "Open the auto-complete menu with candidates for the preceding sclang form."
  (interactive)
  (insert-string ".")
  (auto-complete (list ac-source-sclang-methods
                       ac-source-sclang-ivars)))

(defvar sclang-ac-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd ".") 'sclang-electric-dot)
    map))

;;;###autoload
(define-minor-mode sclang-ac-mode
  "Minor mode that provides more intelligent auto-complete behaviour for SuperCollider."
  nil nil sclang-ac-mode-map
  ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (auto-complete-mode +1)
  ;; Override the sources defined by sclang-mode.
  (setq ac-sources (list ac-source-sclang-classes)))

(provide 'sclang-ac-mode)

;;; NB: We need to use `flet', an obsolete macro. Suppress the usage warning.

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not obsolete)
;; End:

;;; sclang-ac-mode.el ends here
